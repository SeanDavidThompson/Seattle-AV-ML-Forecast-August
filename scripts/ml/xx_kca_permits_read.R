# =============================================================================
# xx_kca_permits_read.R  —  KCA permit extract -> parcel-year aggregate
# =============================================================================
# Read/aggregate layer only.  Defines `kcap_read_annual()` and returns a small
# parcel-year table; it never touches `panel_tbl`.  The panel join, the rolling
# windows and the coverage report live in xx_kca_permits_to_panel.R.
#
# SCHEMA (KCA extract 2026-09-04)
# -------------------------------
#   EXTR_PermitHistory_V.csv        772,094 rows / 335,661 parcels (COUNTYWIDE)
#     Major, Minor, PermitNbr, PermitType, IssueDate, PermitVal,
#     PermitStatus, PcntComplete, UpdatedBy, UpdateDate
#   EXTR_PermitDetailHistory_V.csv  831,617 rows / 511,663 PermitNbr  (long EAV)
#     PermitNbr, PermitItem, ItemValue
#       item  8 = site address     (513k rows)
#       item 12 = description      (316k rows)  <- the only one we use
#       every other item code combined is under 250 rows
#
# This is NOT the layout the first version of xx_kca_permits_to_panel.R was
# written against.  That one expected EXTR_Permit.csv / EXTR_PermitDetail.csv
# at ~206,885 / ~10,239 rows with detail items 41/51/53/55 carrying owner
# value, square feet, unit count and building count.  Those codes are under
# 250 rows combined here, so the sqft/units features it advertised were
# structurally empty, and item 12 — which it documented as "Project Name" and
# discarded — is in fact the description field.  Hence the rewrite.
#
# LEAKAGE
# -------
# PermitStatus and PcntComplete carry no history: they describe the permit as
# of the extract date, so attaching "Complete" to a 2015 parcel-year feeds the
# model 2026 knowledge about 2015.  They are deliberately NOT in the `select=`
# below, which makes the leak unreachable rather than merely unused.  Every
# time-varying feature is built from IssueDate and PermitVal only.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# ---- Tunables ---------------------------------------------------------------

# Permit types that count as new construction.  The extract's PermitType is a
# closed vocabulary, not free text, so this is an exact-match set rather than a
# regex — a regex on "NEW" also catches nothing useful and silently mis-buckets
# anything the assessor adds later.  KCAP_TYPES_KNOWN is the full expected
# vocabulary; anything outside it is reported, never silently dropped.
KCAP_TYPE_NEWCONST <- c("BUILDING, NEW", "ACCESSORY, NEW")
KCAP_TYPE_REMODEL  <- c("REMODEL")
KCAP_TYPE_DEMO     <- c("DEMOLITION")
KCAP_TYPES_KNOWN   <- c(KCAP_TYPE_NEWCONST, KCAP_TYPE_REMODEL, KCAP_TYPE_DEMO,
                        "ELECTRICAL", "OTHER", "PLUMBING", "SIGN", "MOVE",
                        "HAZARDOUS WASTE")

# Item-12 description keywords that mark structural work.  This is the only
# field that separates "SF REROOF" from "New Construction" inside a single
# PermitType, which is why the flag exists at all.
KCAP_DESC_MAJOR_RX <- paste(
  "NEW CONST", "\\bNEW SF\\b", "\\bADDITION\\b", "\\bADDTN\\b", "\\bREMODEL\\b",
  "\\bFOUNDATION\\b", "\\bDEMOLI", "\\bDEMO\\b", "\\bSTRUCTURAL\\b",
  "\\bCONVERT\\b", "\\bCONVERSION\\b", "\\bALTERATION\\b", "\\bADU\\b",
  "\\bDADU\\b", "\\bTOWNHOU", "\\bDUPLEX\\b", "\\bTRIPLEX\\b", "\\bAPARTMENT",
  "\\bGARAGE\\b", "\\bBASEMENT\\b",
  sep = "|")

# Reported alongside the flag so the split stays auditable run to run; it does
# not affect the flag itself.
KCAP_DESC_MINOR_RX <- paste(
  "REROOF", "\\bROOF\\b", "FIRE ALARM", "SPRINKLER", "WATER HEATER",
  "\\bFURNACE\\b", "\\bHVAC\\b", "\\bSIGN\\b", "\\bREPAIR\\b",
  "\\bWINDOW", "\\bSIDING\\b", "\\bFENCE\\b",
  sep = "|")

# The header file is named EXTR_PermitHistory_V.csv in the 2026-09 extract; the
# bare EXTR_Permit.csv name is kept as a fallback so an older drop still reads.
KCAP_HEADER_CANDIDATES <- c("EXTR_PermitHistory_V.csv", "EXTR_Permit.csv")
KCAP_DETAIL_CANDIDATES <- c("EXTR_PermitDetailHistory_V.csv",
                            "EXTR_PermitDetail.csv")

KCAP_DETAIL_ITEM_DESC <- 12L

# ---- Helpers ----------------------------------------------------------------

kcap_find_file <- function(root, candidates) {
  hits <- candidates[file.exists(file.path(root, candidates))]
  if (!length(hits)) return(NA_character_)
  file.path(root, hits[1])
}

# Resolve requested columns against the file's actual header, case-insensitively,
# and fail loudly naming what is missing.  A silent guard here is how the SDCI
# permit block arrived constant and went unnoticed for months.
kcap_resolve_cols <- function(path, wanted, what) {
  have <- names(data.table::fread(file = path, nrows = 0L))
  idx  <- match(toupper(wanted), toupper(have))
  if (anyNA(idx))
    stop(what, ": missing required column(s) ",
         paste(wanted[is.na(idx)], collapse = ", "),
         "\n  file has: ", paste(have, collapse = ", "))
  stats::setNames(have[idx], wanted)
}

# IssueDate is a datetime whose format varies across vintages of the extract.
# Only the date part is ever used, so take the first 10 characters and try the
# two layouts KCA has shipped.
kcap_parse_date <- function(x) {
  x <- trimws(as.character(x))
  x[x %chin% c("", "NA", "NULL")] <- NA_character_
  head10 <- substr(x, 1L, 10L)
  d <- suppressWarnings(as.Date(head10, format = "%Y-%m-%d"))
  na <- is.na(d) & !is.na(x)
  if (any(na))
    d[na] <- suppressWarnings(as.Date(sub("\\s.*$", "", x[na]),
                                      format = "%m/%d/%Y"))
  d
}

# Major/Minor arrive zero-padded in the file, but they are all-digit, so fread
# type-guesses them to integer and strips the padding unless told otherwise.
# We read them as character and VERIFY the widths rather than blind-padding:
# a re-pad that silently repairs a real upstream change is how you end up not
# knowing the key format drifted.
kcap_fix_key_part <- function(v, width, label, verbose) {
  v <- trimws(as.character(v))
  n <- nchar(v)
  bad <- !is.na(v) & n != width
  if (any(bad)) {
    short <- sum(!is.na(v) & n < width)
    long  <- sum(!is.na(v) & n > width)
    if (verbose)
      message("  ⚠️  ", label, ": ", format(sum(bad), big.mark = ","),
              " of ", format(length(v), big.mark = ","),
              " values are not ", width, " chars (", short, " short, ",
              long, " long) — left-padding the short ones")
    v[!is.na(v) & n < width] <- formatC(v[!is.na(v) & n < width],
                                        width = width, flag = "0")
  } else if (verbose) {
    message("  ", label, ": all ", format(length(v), big.mark = ","),
            " values are ", width, " chars — no padding applied")
  }
  v
}

# =============================================================================
# kcap_read_annual()
# -----------------------------------------------------------------------------
# Returns a list:
#   $annual   parcel_id, tax_yr, kcap_n_all, kcap_n_newconst, kcap_n_remodel,
#             kcap_n_demo, kcap_n_desc_major, kcap_val_sum, kcap_val_max
#             (one row per parcel-year with at least one permit, ALL years
#              <= yr_max — not clipped at yr_min, because
#              kcap_years_since_newconst reads the full 1900-2026 history)
#   $newconst parcel_id, tax_yr for every parcel-year with a new-construction
#             permit, for the rolling-join that builds years-since
#   $diag     named list of counts for the caller's coverage report
#
# tax_yr_lag maps issue year to tax year.  0 reproduces xx_permits_to_panel.R
# exactly (event_year -> tax_yr).  See the note in xx_kca_permits_to_panel.R:
# under "tax_yr N is assessed 1 Jan N-1", 0 admits up to ~12 months of
# look-ahead and 1 is the leak-free choice.
#
# MEMORY: the detail file is read, reduced to one flag row per PermitNbr and
# dropped BEFORE the header file is opened.  The two raw tables are never
# resident at the same time.  Nothing bigger than the parcel-year aggregate is
# returned.
# =============================================================================
kcap_read_annual <- function(kca_root,
                             yr_max,
                             tax_yr_lag   = 0L,
                             val_winsor_p = 0.995,
                             yr_floor     = 1901L,
                             verbose      = TRUE) {

  stopifnot(length(yr_max) == 1L, is.finite(yr_max))
  yr_max      <- as.integer(yr_max)
  tax_yr_lag  <- as.integer(tax_yr_lag)
  diag <- list()

  header_path <- kcap_find_file(kca_root, KCAP_HEADER_CANDIDATES)
  detail_path <- kcap_find_file(kca_root, KCAP_DETAIL_CANDIDATES)

  if (is.na(header_path)) {
    if (verbose)
      message("  ⚠️  none of [",
              paste(KCAP_HEADER_CANDIDATES, collapse = ", "),
              "] found in ", kca_root)
    return(NULL)
  }
  if (verbose) message("  header: ", basename(header_path))

  # ---- 1. Detail -> one description flag per PermitNbr ----------------------
  # Read first, reduce hard, drop.  831,617 rows in, at most one row per
  # PermitNbr out.
  desc_flag <- NULL
  if (is.na(detail_path)) {
    if (verbose)
      message("  ℹ️  no permit detail file — kcap_desc_major_3yr will be all-zero")
    diag$detail_rows <- 0L
  } else {
    if (verbose) message("  detail: ", basename(detail_path))
    dcols <- kcap_resolve_cols(detail_path,
                               c("PermitNbr", "PermitItem", "ItemValue"),
                               basename(detail_path))
    det <- data.table::fread(
      file = detail_path, select = unname(dcols),
      colClasses = list(character = unname(dcols[c("PermitNbr", "ItemValue")])),
      na.strings = c("", "NA"), encoding = "Latin-1",
      showProgress = FALSE)
    data.table::setnames(det, unname(dcols), c("permit_nbr", "permit_item", "item_value"))
    diag$detail_rows <- nrow(det)

    det[, permit_item := suppressWarnings(as.integer(permit_item))]
    det <- det[permit_item == KCAP_DETAIL_ITEM_DESC & !is.na(item_value)]
    diag$detail_desc_rows <- nrow(det)

    det[, desc_u := toupper(trimws(item_value))]
    det[, is_major := as.integer(grepl(KCAP_DESC_MAJOR_RX, desc_u, perl = TRUE))]
    det[, is_minor := as.integer(grepl(KCAP_DESC_MINOR_RX, desc_u, perl = TRUE))]
    diag$desc_major <- sum(det$is_major)
    diag$desc_minor <- sum(det$is_minor & !det$is_major)
    diag$desc_other <- sum(!det$is_major & !det$is_minor)

    desc_flag <- det[, .(desc_major = max(is_major)), by = permit_nbr]
    data.table::setkey(desc_flag, permit_nbr)

    rm(det); gc(verbose = FALSE)
    if (verbose)
      message("  description rows (item ", KCAP_DETAIL_ITEM_DESC, "): ",
              format(diag$detail_desc_rows, big.mark = ","),
              " | structural ", format(diag$desc_major, big.mark = ","),
              " | cosmetic/systems ", format(diag$desc_minor, big.mark = ","),
              " | unclassified ", format(diag$desc_other, big.mark = ","))
  }

  # ---- 2. Header ------------------------------------------------------------
  # PermitStatus / PcntComplete / UpdatedBy / UpdateDate are intentionally
  # absent from `select=`: see the LEAKAGE note at the top of this file.
  hwant <- c("Major", "Minor", "PermitNbr", "PermitType", "IssueDate", "PermitVal")
  hcols <- kcap_resolve_cols(header_path, hwant, basename(header_path))
  pm <- data.table::fread(
    file = header_path, select = unname(hcols),
    colClasses = list(character = unname(hcols[c("Major", "Minor", "PermitNbr",
                                                 "PermitType", "IssueDate")])),
    na.strings = c("", "NA"), encoding = "Latin-1", showProgress = FALSE)
  data.table::setnames(pm, unname(hcols),
                       c("major", "minor", "permit_nbr", "permit_type",
                         "issue_date", "permit_val"))
  diag$header_rows <- nrow(pm)

  # ---- 3. Key ---------------------------------------------------------------
  # Undashed 10 characters: the panel's parcel_id is paste0(major, minor)
  # (01_import_res.R). A dashed id here matches zero panel rows — that is the
  # exact defect that left the SDCI permit features constant for months, and
  # the defect the previous version of the kcap block reintroduced.
  pm[, major := kcap_fix_key_part(major, 6L, "Major", verbose)]
  pm[, minor := kcap_fix_key_part(minor, 4L, "Minor", verbose)]
  pm[, parcel_id := paste0(major, minor)]
  pm[, c("major", "minor") := NULL]
  diag$parcels_in_file <- data.table::uniqueN(pm$parcel_id)

  # ---- 4. Dates -------------------------------------------------------------
  pm[, issue_date := kcap_parse_date(issue_date)]
  pm[, issue_yr   := data.table::year(issue_date)]

  diag$drop_bad_date <- sum(is.na(pm$issue_yr))
  pm <- pm[!is.na(issue_yr)]

  # IssueDate spans 1900-2027.  A 1900 stamp in a permit system is a null
  # sentinel, not a permit.
  diag$drop_sentinel <- sum(pm$issue_yr < yr_floor)
  pm <- pm[issue_yr >= yr_floor]

  pm[, tax_yr := issue_yr + tax_yr_lag]

  # Permits dated past the end of the panel: dropped BEFORE aggregation so a
  # 2027 permit cannot enter a 2026 trailing window.  They are not usable
  # today in any case — 05_extend_panel freezes the permit windows across the
  # forecast years — but making the drop explicit and counted is the point.
  diag$drop_future <- sum(pm$tax_yr > yr_max)
  pm <- pm[tax_yr <= yr_max]
  diag$rows_kept <- nrow(pm)

  if (verbose) {
    message("  header rows: ", format(diag$header_rows, big.mark = ","),
            " | parcels: ", format(diag$parcels_in_file, big.mark = ","))
    message("  dropped — unparseable date: ",
            format(diag$drop_bad_date, big.mark = ","),
            " | pre-", yr_floor, " sentinel: ",
            format(diag$drop_sentinel, big.mark = ","),
            " | dated past tax_yr ", yr_max, ": ",
            format(diag$drop_future, big.mark = ","))
    message("  rows kept: ", format(diag$rows_kept, big.mark = ","),
            " | tax_yr map: issue_yr + ", tax_yr_lag)
  }

  if (!nrow(pm)) {
    if (verbose) message("  ⚠️  no permit rows survived filtering")
    return(NULL)
  }

  # ---- 5. Type buckets ------------------------------------------------------
  pm[, type_u := toupper(trimws(permit_type))]
  pm[, permit_type := NULL]

  seen    <- pm[, .N, by = type_u][order(-N)]
  unknown <- seen[!type_u %chin% KCAP_TYPES_KNOWN]
  if (nrow(unknown) && verbose)
    message("  ⚠️  PermitType values outside the known vocabulary (",
            format(sum(unknown$N), big.mark = ","), " rows): ",
            paste0(unknown$type_u, " (", unknown$N, ")", collapse = ", "))
  diag$types_unknown_rows <- if (nrow(unknown)) sum(unknown$N) else 0L
  diag$types_seen <- seen

  pm[, is_newconst := as.integer(type_u %chin% KCAP_TYPE_NEWCONST)]
  pm[, is_remodel  := as.integer(type_u %chin% KCAP_TYPE_REMODEL)]
  pm[, is_demo     := as.integer(type_u %chin% KCAP_TYPE_DEMO)]
  pm[, type_u := NULL]

  # ---- 6. Value: winsorize, then it is safe to sum --------------------------
  # 17% of PermitVal is zero and the nonzero p90/median ratio is ~15x, with a
  # $1.6B maximum.  Cap at the p-th percentile of NONZERO values so one row
  # cannot dominate a parcel-year; log1p downstream handles the zeros and what
  # is left of the skew.
  pm[, permit_val := suppressWarnings(as.numeric(permit_val))]
  pm[!is.finite(permit_val) | permit_val < 0, permit_val := 0]
  nz <- pm[permit_val > 0, permit_val]
  cap <- if (length(nz)) as.numeric(stats::quantile(nz, val_winsor_p, names = FALSE)) else Inf
  diag$val_cap <- cap
  diag$val_capped_rows <- sum(pm$permit_val > cap)
  pm[permit_val > cap, permit_val := cap]
  if (verbose)
    message("  PermitVal winsorized at p", val_winsor_p * 100, " of nonzero = $",
            format(round(cap), big.mark = ","), " (",
            format(diag$val_capped_rows, big.mark = ","), " rows capped)")

  # ---- 7. Description flag onto the header ---------------------------------
  if (!is.null(desc_flag)) {
    pm[desc_flag, desc_major := i.desc_major, on = "permit_nbr"]
    diag$detail_matched <- sum(!is.na(pm$desc_major))
    if (verbose)
      message("  permits with a description row: ",
              format(diag$detail_matched, big.mark = ","), " of ",
              format(nrow(pm), big.mark = ","), " (",
              round(100 * diag$detail_matched / nrow(pm), 1), "%)")
    rm(desc_flag); gc(verbose = FALSE)
  } else {
    pm[, desc_major := NA_integer_]
    diag$detail_matched <- 0L
  }
  pm[is.na(desc_major), desc_major := 0L]
  pm[, permit_nbr := NULL]

  # ---- 8. Aggregate to parcel-year ------------------------------------------
  annual <- pm[, .(kcap_n_all        = .N,
                   kcap_n_newconst   = sum(is_newconst),
                   kcap_n_remodel    = sum(is_remodel),
                   kcap_n_demo       = sum(is_demo),
                   kcap_n_desc_major = sum(desc_major),
                   kcap_val_sum      = sum(permit_val),
                   kcap_val_max      = max(permit_val)),
               by = .(parcel_id, tax_yr)]
  newconst <- pm[is_newconst == 1L, .(parcel_id, tax_yr)]
  newconst <- unique(newconst)

  rm(pm); gc(verbose = FALSE)

  for (cc in c("kcap_n_all", "kcap_n_newconst", "kcap_n_remodel",
               "kcap_n_demo", "kcap_n_desc_major"))
    data.table::set(annual, j = cc, value = as.integer(annual[[cc]]))

  data.table::setkey(annual, parcel_id, tax_yr)
  data.table::setkey(newconst, parcel_id, tax_yr)

  diag$annual_rows    <- nrow(annual)
  diag$annual_parcels <- data.table::uniqueN(annual$parcel_id)
  if (verbose)
    message("  parcel-years aggregated: ",
            format(diag$annual_rows, big.mark = ","), " over ",
            format(diag$annual_parcels, big.mark = ","), " parcels")

  list(annual = annual, newconst = newconst, diag = diag)
}
