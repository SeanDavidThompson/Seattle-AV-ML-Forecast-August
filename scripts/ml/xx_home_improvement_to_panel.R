# =============================================================================
# xx_home_improvement_to_panel.R  —  Home improvement exemptions as features
# =============================================================================
# Why this is worth having
# ------------------------
# The RCW 84.36.400 home improvement exemption defers the AV of a qualifying
# improvement for three assessment years.  That makes it one of the few
# genuinely MECHANICAL, forward-known AV events in the roll:
#
#   - while the exemption is active, taxable AV is suppressed by HomeImpVal
#   - in the year after LastBillYr, that value returns to the roll
#
# A model with no HI features has to learn those step-ups as noise.  With
# them, the roll-off is a scheduled event the model can see coming — and
# critically, LastBillYr is known for exemptions granted before the forecast
# starts, so `hi_rolloff_next_val` is populated in forecast years rather than
# going dead at the boundary the way the cs_off_* block did.
#
# Field layout:
#   EXTR_HomeImpApplication (~34,476): Major, Minor, HIExemptId, BldgNbr,
#     NoteId, ReceivedDate, EstCost, EstCompletionDate, DistrictName,
#     PermitNbr, PermitIssueDate, ..., Approved, ApprovedBy, ApprovedDate
#   EXTR_HomeImpExemption   (~32,258): Major, Minor, HIExemptId, BldgNbr,
#     NoteId, FirstBillYr, LastBillYr, HomeImpVal, ValuedBy, ValueDate
#
# Produces:
#   hi_app_cnt_3yr        approved applications in the last 3 years
#   hi_est_cost_3yr       estimated improvement cost, 3-year sum
#   hi_exempt_active      1 if an exemption covers this tax year
#   hi_exempt_val_active  suppressed AV currently under exemption
#   hi_rolloff_next_val   exempt value returning to the roll NEXT tax year
#   hi_rolloff_cur_val    exempt value that returned to the roll THIS tax year
#   hi_ever               1 if the parcel has ever had an exemption
#   log_hi_est_cost_3yr, log_hi_exempt_val_active
#
# Expects `panel_tbl` in scope.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(janitor)
  library(stringr)
  library(zoo)
  library(here)
})

message("Running xx_home_improvement_to_panel.R ...")

kca_date <- get("kca_date_data_extracted", envir = .GlobalEnv)
kca_root <- here::here("data", "kca", kca_date)

app_path <- file.path(kca_root, "EXTR_HomeImpApplication.csv")
exm_path <- file.path(kca_root, "EXTR_HomeImpExemption.csv")

hi_predictors <- c(
  "hi_app_cnt_3yr", "hi_est_cost_3yr",
  "hi_exempt_active", "hi_exempt_val_active",
  "hi_rolloff_next_val", "hi_rolloff_cur_val", "hi_ever",
  "log_hi_est_cost_3yr", "log_hi_exempt_val_active"
)

if (!file.exists(app_path) && !file.exists(exm_path)) {
  message("  \u26a0\ufe0f  no home improvement extracts found — hi_* features skipped")
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
  pid <- function(major, minor)
    paste0(str_pad(trimws(as.character(major)), 6, "left", "0"), "-",
           str_pad(trimws(as.character(minor)), 4, "left", "0"))

  pt <- data.table::as.data.table(panel_tbl)
  pt[, parcel_id := as.character(parcel_id)]
  pt[, tax_yr    := as.integer(tax_yr)]
  drop_existing <- grep("^hi_|^log_hi_", names(pt), value = TRUE)
  if (length(drop_existing)) pt[, (drop_existing) := NULL]

  yr_min <- min(pt$tax_yr, na.rm = TRUE)
  yr_max <- max(pt$tax_yr, na.rm = TRUE)

  # ---- 1. Applications -------------------------------------------------------
  app_ann <- NULL
  if (file.exists(app_path)) {
    ap <- read_kca(app_path)
    ap[, parcel_id := pid(major, minor)]
    ap[, est_cost := suppressWarnings(as.numeric(est_cost))]
    # Prefer ApprovedDate; fall back to ReceivedDate.
    pdate <- function(x) {
      x <- trimws(as.character(x))
      d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
      d[is.na(d)] <- suppressWarnings(as.Date(x[is.na(d)], format = "%Y-%m-%d"))
      d
    }
    ap[, ev_date := pdate(approved_date)]
    ap[is.na(ev_date), ev_date := pdate(received_date)]
    ap[, ev_yr := data.table::year(ev_date)]
    ap[, is_approved := as.integer(toupper(trimws(approved)) %chin% c("Y", "1", "T"))]

    app_ann <- ap[!is.na(ev_yr) & is_approved == 1L,
                  .(hi_app_cnt = .N,
                    hi_est_cost = sum(est_cost, na.rm = TRUE)),
                  by = .(parcel_id, tax_yr = ev_yr)]
    message("  applications: ", format(nrow(ap), big.mark = ","),
            " rows | approved parcel-years: ",
            format(nrow(app_ann), big.mark = ","))
  }

  # ---- 2. Exemptions expanded to a parcel-year grid --------------------------
  ex_ann <- NULL
  roll_ann <- NULL
  ever <- NULL
  if (file.exists(exm_path)) {
    ex <- read_kca(exm_path)
    ex[, parcel_id := pid(major, minor)]
    ex[, `:=`(first_bill_yr = suppressWarnings(as.integer(first_bill_yr)),
              last_bill_yr  = suppressWarnings(as.integer(last_bill_yr)),
              home_imp_val  = suppressWarnings(as.numeric(home_imp_val)))]
    ex <- ex[!is.na(first_bill_yr) & !is.na(last_bill_yr) &
               last_bill_yr >= first_bill_yr &
               is.finite(home_imp_val) & home_imp_val > 0]

    # Clamp to the panel window before expanding, so the grid stays small.
    ex[, fy := pmax(first_bill_yr, yr_min)]
    ex[, ly := pmin(last_bill_yr,  yr_max)]
    ex_open <- ex[ly >= fy]

    ex_ann <- ex_open[, .(tax_yr = seq(fy, ly)),
                      by = .(rid = seq_len(nrow(ex_open)))][
      ex_open[, .(rid = seq_len(nrow(ex_open)), parcel_id, home_imp_val)],
      on = "rid"][
      , .(hi_exempt_val_active = sum(home_imp_val, na.rm = TRUE),
          hi_exempt_active     = 1L),
      by = .(parcel_id, tax_yr)]

    # Roll-off: value returns to the roll the year AFTER last_bill_yr.
    roll_ann <- ex[, .(hi_rolloff_cur_val = sum(home_imp_val, na.rm = TRUE)),
                   by = .(parcel_id, tax_yr = last_bill_yr + 1L)]
    roll_ann <- roll_ann[tax_yr >= yr_min & tax_yr <= yr_max]

    ever <- unique(ex[, .(parcel_id, hi_ever = 1L)], by = "parcel_id")
    message("  exemptions: ", format(nrow(ex), big.mark = ","),
            " rows | parcels ever exempt: ",
            format(nrow(ever), big.mark = ","))
  }

  # ---- 3. Join ---------------------------------------------------------------
  if (!is.null(app_ann))
    pt <- merge(pt, app_ann, by = c("parcel_id", "tax_yr"), all.x = TRUE)
  if (!is.null(ex_ann))
    pt <- merge(pt, ex_ann,  by = c("parcel_id", "tax_yr"), all.x = TRUE)
  if (!is.null(roll_ann))
    pt <- merge(pt, roll_ann, by = c("parcel_id", "tax_yr"), all.x = TRUE)
  if (!is.null(ever))
    pt <- merge(pt, ever,    by = "parcel_id", all.x = TRUE)

  for (cc in c("hi_app_cnt", "hi_est_cost", "hi_exempt_active",
               "hi_exempt_val_active", "hi_rolloff_cur_val", "hi_ever"))
    if (cc %in% names(pt)) pt[is.na(get(cc)), (cc) := 0] else pt[, (cc) := 0]

  # ---- 4. Rolling + lead features -------------------------------------------
  data.table::setorder(pt, parcel_id, tax_yr)
  pt[, hi_app_cnt_3yr  := zoo::rollapplyr(hi_app_cnt,  3, sum, fill = 0,
                                          partial = TRUE), by = parcel_id]
  pt[, hi_est_cost_3yr := zoo::rollapplyr(hi_est_cost, 3, sum, fill = 0,
                                          partial = TRUE), by = parcel_id]

  # Next year's roll-off is a LEAD, and it is legitimately known in advance:
  # last_bill_yr is set when the exemption is granted.
  pt[, hi_rolloff_next_val := data.table::shift(hi_rolloff_cur_val, 1L,
                                                type = "lead", fill = 0),
     by = parcel_id]

  pt[, log_hi_est_cost_3yr      := log1p(pmax(hi_est_cost_3yr, 0))]
  pt[, log_hi_exempt_val_active := log1p(pmax(hi_exempt_val_active, 0))]

  present <- intersect(hi_predictors, names(pt))
  fc <- get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2027L)
  live <- pt[tax_yr >= fc, sum(hi_rolloff_next_val > 0, na.rm = TRUE)]
  message("  \u2705 hi features added (", length(present), "): ",
          paste(present, collapse = ", "))
  message("  scheduled roll-offs visible in forecast years (", fc, "+): ",
          format(live, big.mark = ","), " parcel-years")

  panel_tbl <- pt
  assign("panel_tbl", pt, envir = .GlobalEnv)
  assign("hi_predictors", hi_predictors, envir = .GlobalEnv)
  rm(pt)
  gc(verbose = FALSE)
}

message("xx_home_improvement_to_panel.R loaded")
