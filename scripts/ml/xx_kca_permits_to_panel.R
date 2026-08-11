# =============================================================================
# xx_kca_permits_to_panel.R  —  KCA permit history as panel features
# =============================================================================
# Why this exists
# ---------------
# xx_permits_to_panel.R sources permits from a City of Seattle SDCI extract
# ("New Construction for OERF AV <date>.xlsx").  That file is rich but city-
# scoped and cycle-specific.  The KCA extract (EXTR_Permit ~206,885 rows +
# EXTR_PermitDetail ~10,239 rows) is the assessor's own permit history: it
# covers every parcel in the roll, carries PermitVal and PcntComplete, and is
# keyed on Major/Minor so it joins without an address or GIS match.
#
# This script ADDS `kcap_*` features.  It does not replace the SDCI features —
# the two sources overlap but disagree on coverage and on valuation basis, and
# letting LightGBM see both is more informative than picking one.  The only
# merged column is `any_newconst`, which takes the max of the two.
#
# Field layout (Permit History Record Description):
#   Major, Minor, PermitNbr, PermitType, IssueDate, PermitVal, PermitStatus,
#   PcntComplete, UpdatedBy, UpdateDate
# Detail (LookUp type 161):
#   PermitNbr, PermitItem, ItemValue
#   41 = Owner-Reported Value, 51 = Square Feet, 52 = Nbr Stories,
#   53 = Nbr Units, 55 = Nbr Buildings, 57 = Occupancy, 12 = Project Name
#
# Expects `panel_tbl` in scope (same contract as xx_permits_to_panel.R).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(janitor)
  library(stringr)
  library(zoo)
  library(here)
})

message("Running xx_kca_permits_to_panel.R ...")

kca_date <- get("kca_date_data_extracted", envir = .GlobalEnv)
kca_root <- here::here("data", "kca", kca_date)

permit_path <- file.path(kca_root, "EXTR_Permit.csv")
detail_path <- file.path(kca_root, "EXTR_PermitDetail.csv")

kcap_predictors <- c(
  "kcap_permits_1yr", "kcap_permits_3yr", "kcap_permits_5yr",
  "kcap_val_3yr", "kcap_val_5yr",
  "kcap_sqft_3yr", "kcap_units_3yr",
  "kcap_pct_complete_max", "kcap_open_permits",
  "kcap_years_since_permit",
  "log_kcap_val_3yr", "log_kcap_sqft_3yr"
)

if (!file.exists(permit_path)) {
  message("  \u26a0\ufe0f  ", basename(permit_path),
          " not found — kcap_* features skipped")
} else {

  if (!exists("read_kca", mode = "function")) {
    read_kca <- function(path, ...) {
      dt <- fread(file = path, na.strings = c("", "NA", " "),
                  encoding = "Latin-1", ...)
      setDT(dt)
      chr <- names(dt)[vapply(dt, is.character, logical(1))]
      for (cc in chr)
        set(dt, j = cc, value = iconv(dt[[cc]], "UTF-8", "UTF-8", sub = ""))
      clean_names(dt)
    }
  }

  # ---- 1. Permit header ------------------------------------------------------
  pm <- read_kca(permit_path)
  pm[, parcel_id := paste0(str_pad(trimws(major), 6, "left", "0"), "-",
                           str_pad(trimws(minor), 4, "left", "0"))]

  # IssueDate is a 19-char string; formats vary across vintages of the extract.
  parse_kca_date <- function(x) {
    x <- trimws(as.character(x))
    d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
    d[is.na(d)] <- suppressWarnings(as.Date(x[is.na(d)], format = "%Y-%m-%d"))
    d
  }
  pm[, issue_date  := parse_kca_date(issue_date)]
  pm[, event_year  := data.table::year(issue_date)]
  pm[, permit_val  := suppressWarnings(as.numeric(permit_val))]
  pm[, pcnt_complete := suppressWarnings(as.numeric(pcnt_complete))]
  pm[, permit_type_u   := toupper(trimws(permit_type))]
  pm[, permit_status_u := toupper(trimws(permit_status))]

  # New-construction flag from permit type text.  KCA PermitType is free-ish
  # text ("NEW", "ADDITION", "ALTERATION", "DEMOLITION", ...).
  pm[, is_newconst := as.integer(
    !is.na(permit_type_u) &
      str_detect(permit_type_u, "\\b(NEW|NEW CONST|CONSTRUCTION|ADDITION)\\b"))]
  pm[, is_open := as.integer(
    !is.na(permit_status_u) &
      !str_detect(permit_status_u, "\\b(FINAL|COMPLETE|CLOSED|EXPIRED|CANCEL)"))]

  message("  EXTR_Permit rows: ", format(nrow(pm), big.mark = ","),
          " | parcels: ", format(uniqueN(pm$parcel_id), big.mark = ","),
          " | years ", min(pm$event_year, na.rm = TRUE), "-",
          max(pm$event_year, na.rm = TRUE))

  # ---- 2. Permit detail: sqft / units / owner value --------------------------
  det_wide <- NULL
  if (file.exists(detail_path)) {
    det <- read_kca(detail_path)
    det[, permit_item := suppressWarnings(as.integer(permit_item))]
    det[, num_val := suppressWarnings(as.numeric(
      str_replace_all(item_value, "[^0-9.\\-]", "")))]
    det_wide <- dcast(
      det[permit_item %in% c(41L, 51L, 53L, 55L) & !is.na(num_val)],
      permit_nbr ~ permit_item, value.var = "num_val",
      fun.aggregate = function(z) sum(z, na.rm = TRUE), fill = NA_real_)
    old <- setdiff(names(det_wide), "permit_nbr")
    new <- c(`41` = "owner_val", `51` = "permit_sqft",
             `53` = "permit_units", `55` = "permit_bldgs")[old]
    setnames(det_wide, old, unname(new))
    message("  EXTR_PermitDetail matched cols: ",
            paste(setdiff(names(det_wide), "permit_nbr"), collapse = ", "))
  } else {
    message("  \u2139\ufe0f  EXTR_PermitDetail.csv not found — sqft/unit features NA")
  }

  if (!is.null(det_wide))
    pm <- merge(pm, det_wide, by = "permit_nbr", all.x = TRUE)
  for (v in c("owner_val", "permit_sqft", "permit_units", "permit_bldgs"))
    if (!v %in% names(pm)) pm[, (v) := NA_real_]

  # PermitVal is frequently 0/NA on older rows; owner-reported value backfills.
  pm[, permit_val_use := fifelse(is.na(permit_val) | permit_val <= 0,
                                 owner_val, permit_val)]

  # ---- 3. Annual parcel-year aggregation ------------------------------------
  ann <- pm[!is.na(parcel_id) & !is.na(event_year),
            .(kcap_cnt        = .N,
              kcap_val_sum    = sum(permit_val_use, na.rm = TRUE),
              kcap_sqft_sum   = sum(permit_sqft,    na.rm = TRUE),
              kcap_units_sum  = sum(permit_units,   na.rm = TRUE),
              kcap_pct_max    = suppressWarnings(max(pcnt_complete, na.rm = TRUE)),
              kcap_open_sum   = sum(is_open,        na.rm = TRUE),
              kcap_new_flag   = as.integer(any(is_newconst == 1L, na.rm = TRUE))),
            by = .(parcel_id, tax_yr = event_year)]
  ann[!is.finite(kcap_pct_max), kcap_pct_max := NA_real_]

  # ---- 4. Join into the panel ------------------------------------------------
  pt <- data.table::as.data.table(panel_tbl)
  pt[, parcel_id := as.character(parcel_id)]
  pt[, tax_yr    := as.integer(tax_yr)]

  # Drop any prior kcap_* columns so re-runs don't create .x/.y twins —
  # this is the same defect that killed the econ block in the 2027+ panels.
  drop_existing <- grep("^kcap_|^log_kcap_", names(pt), value = TRUE)
  if (length(drop_existing)) pt[, (drop_existing) := NULL]

  pt <- merge(pt, ann, by = c("parcel_id", "tax_yr"), all.x = TRUE)

  zero_cols <- c("kcap_cnt", "kcap_val_sum", "kcap_sqft_sum",
                 "kcap_units_sum", "kcap_open_sum", "kcap_new_flag")
  for (cc in zero_cols) pt[is.na(get(cc)), (cc) := 0]

  # ---- 5. Rolling windows ----------------------------------------------------
  data.table::setorder(pt, parcel_id, tax_yr)
  roll_sum <- function(x, k) zoo::rollapplyr(x, k, sum, fill = 0, partial = TRUE)

  pt[, `:=`(
    kcap_permits_1yr = kcap_cnt,
    kcap_permits_3yr = roll_sum(kcap_cnt,       3),
    kcap_permits_5yr = roll_sum(kcap_cnt,       5),
    kcap_val_3yr     = roll_sum(kcap_val_sum,   3),
    kcap_val_5yr     = roll_sum(kcap_val_sum,   5),
    kcap_sqft_3yr    = roll_sum(kcap_sqft_sum,  3),
    kcap_units_3yr   = roll_sum(kcap_units_sum, 3),
    kcap_open_permits = kcap_open_sum
  ), by = parcel_id]

  pt[, kcap_pct_complete_max :=
       zoo::na.locf(kcap_pct_max, na.rm = FALSE), by = parcel_id]

  pt[, .last_new := fifelse(kcap_new_flag == 1L, tax_yr, NA_integer_)]
  pt[, .last_new := zoo::na.locf(.last_new, na.rm = FALSE), by = parcel_id]
  pt[, kcap_years_since_permit := fifelse(is.na(.last_new), NA_real_,
                                          as.numeric(tax_yr - .last_new))]
  pt[, .last_new := NULL]

  pt[, log_kcap_val_3yr  := log1p(pmax(kcap_val_3yr,  0))]
  pt[, log_kcap_sqft_3yr := log1p(pmax(kcap_sqft_3yr, 0))]

  # ---- 6. Union the new-construction flag with the SDCI one -----------------
  if ("any_newconst" %in% names(pt)) {
    pt[, any_newconst := pmax(as.integer(any_newconst),
                              as.integer(kcap_new_flag), na.rm = TRUE)]
  } else {
    pt[, any_newconst := as.integer(kcap_new_flag)]
  }

  present <- intersect(kcap_predictors, names(pt))
  cov_pct <- round(100 * mean(pt$kcap_permits_5yr > 0, na.rm = TRUE), 1)
  message("  \u2705 kcap features added (", length(present), "): ",
          paste(present, collapse = ", "))
  message("  parcel-years with a permit in the last 5 yrs: ", cov_pct, "%")

  panel_tbl <- pt
  assign("panel_tbl", pt, envir = .GlobalEnv)
  assign("kcap_predictors", kcap_predictors, envir = .GlobalEnv)
  rm(pm, ann, pt)
  gc(verbose = FALSE)
}

message("xx_kca_permits_to_panel.R loaded")
