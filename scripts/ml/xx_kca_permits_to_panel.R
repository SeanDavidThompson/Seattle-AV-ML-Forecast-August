# =============================================================================
# xx_kca_permits_to_panel.R  —  KCA permit history as panel features
# =============================================================================
# Why this exists
# ---------------
# xx_permits_to_panel.R sources permits from a City of Seattle SDCI extract
# ("New Construction for OERF AV <date>.xlsx").  That file is rich but city-
# scoped and cycle-specific.  The KCA extract is the assessor's own permit
# history: it covers every parcel in the roll, carries PermitVal, and is keyed
# on Major/Minor so it joins without an address or GIS match.
#
# This script ADDS `kcap_*` features.  It does not replace the SDCI features
# and — unlike the previous version — it does not merge into them either.  The
# two sources overlap but disagree on coverage and on valuation basis, and the
# only way to decide what to drop is to see both separately:
#
#   kcap_permits_3yr           vs  permits_last_3yr      same construct, different universe
#   log_kcap_val_3yr           vs  log_val_last_3yr      same construct, different valuation basis
#   kcap_years_since_newconst  vs  years_since_newconst  direct overlap
#   kcap_newconst_3yr          vs  any_newconst          related; NOT merged (see below)
#   kcap_remodel_3yr / _demo_3yr / _desc_major_3yr       no SDCI analog
#
# The previous version folded its new-construction flag into `any_newconst`
# with a pmax().  That silently mutated an SDCI feature which now carries real
# gain, and made the two sources impossible to tell apart.  Removed.
#
# THE FEATURES (8)
# ----------------
#   kcap_newconst_3yr          "Building, New" + "Accessory, New", trailing 3yr
#   kcap_remodel_3yr           "Remodel", trailing 3yr
#   kcap_demo_3yr              "Demolition", trailing 3yr — opposite sign to the above
#   kcap_permits_3yr           all types, trailing 3yr — parcel activity level
#   log_kcap_val_3yr           log1p of winsorized trailing-3yr PermitVal sum
#   log_kcap_val_max_3yr       log1p of the largest single permit in the window
#   kcap_years_since_newconst  years since last new-construction permit; NA if never
#   kcap_desc_major_3yr        item-12 description marks structural work in the window
#
# The raw kcap_val_3yr / kcap_val_max_3yr columns are dropped after the logs
# are taken — two extra doubles across ~5.1M panel rows is ~82 MB for columns
# that are a monotone transform of ones we keep.
#
# LEAKAGE
# -------
# PermitStatus and PcntComplete describe the permit as of the extract date and
# carry no history, so attaching "Complete" to a 2015 parcel-year would feed
# the model 2026 knowledge about 2015.  They are not read at all — see the
# `select=` in xx_kca_permits_read.R.  Permits dated past the panel's last tax
# year are dropped before aggregation, counted, and reported.
#
# Expects `panel_tbl` in scope (same contract as xx_permits_to_panel.R).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

message("Running xx_kca_permits_to_panel.R ...")

if (!exists("kcap_read_annual", mode = "function"))
  source(here::here("scripts", "ml", "xx_kca_permits_read.R"))

kca_date <- get("kca_date_data_extracted", envir = .GlobalEnv)
kca_root <- here::here("data", "kca", kca_date)

# tax_yr convention ----------------------------------------------------------
# tax_yr N is the payable-N roll, assessed 1 Jan N-1.
#
# xx_permits_to_panel.R maps a permit's event year straight onto tax_yr
# (see its step 7: by = c("tax_yr" = "event_year")).  Lag 0 reproduces that
# exactly, which is what this block does so the two permit sources agree on
# timing.
#
# For the record, I think that mapping is wrong: a permit issued in November
# 2015 lands on tax_yr 2015, whose lien date was 1 Jan 2015, so it admits up
# to ~12 months of look-ahead.  The leak-free map is issue_yr + 1.  Set
# kcap_tax_yr_lag <- 1L before sourcing (or in CFG) to measure the difference
# without editing this file.
kcap_tax_yr_lag <- as.integer(
  get0("kcap_tax_yr_lag", envir = .GlobalEnv, ifnotfound = 0L))

kcap_predictors <- c(
  "kcap_newconst_3yr", "kcap_remodel_3yr", "kcap_demo_3yr",
  "kcap_permits_3yr",
  "log_kcap_val_3yr", "log_kcap_val_max_3yr",
  "kcap_years_since_newconst", "kcap_desc_major_3yr"
)
kcap_count_cols <- c("kcap_newconst_3yr", "kcap_remodel_3yr", "kcap_demo_3yr",
                     "kcap_permits_3yr", "kcap_desc_major_3yr")

assign("kcap_predictors", kcap_predictors, envir = .GlobalEnv)

# Read before copying the panel: on a missing extract we must not pay for a
# 5.1M-row as.data.table() we are about to throw away.
yr_min <- min(as.integer(panel_tbl$tax_yr), na.rm = TRUE)
yr_max <- max(as.integer(panel_tbl$tax_yr), na.rm = TRUE)

res <- kcap_read_annual(kca_root, yr_max = yr_max, tax_yr_lag = kcap_tax_yr_lag)

if (is.null(res)) {

  message("  ⚠️  no usable KCA permit extract in ", kca_root,
          " — kcap_* features skipped (", length(kcap_predictors),
          " columns not attached)")

} else {

  ann <- res$annual
  nc  <- res$newconst

  pt <- data.table::as.data.table(panel_tbl)
  pt[, parcel_id := as.character(parcel_id)]
  pt[, tax_yr    := as.integer(tax_yr)]

  # Drop any prior kcap_* columns so re-runs don't create .x/.y twins — this
  # is the same defect that killed the econ block in the 2027+ panels.
  drop_existing <- grep("^kcap_|^log_kcap_", names(pt), value = TRUE)
  if (length(drop_existing)) pt[, (drop_existing) := NULL]

  # ---- 1. Key format check --------------------------------------------------
  # The panel's parcel_id is the undashed 10-character form, paste0(Major,
  # Minor) — see 01_import_res.R.  A dashed id here matches zero rows, which
  # is exactly how the SDCI permit features arrived constant and went
  # unnoticed for months.  Verify rather than assume, and say the number out
  # loud on every run.
  panel_ids <- unique(pt$parcel_id)
  file_ids  <- unique(ann$parcel_id)
  n_in_panel <- sum(file_ids %chin% panel_ids)
  n_covered  <- sum(panel_ids %chin% file_ids)
  pct_file   <- round(100 * n_in_panel / length(file_ids), 1)
  pct_panel  <- round(100 * n_covered  / length(panel_ids), 1)

  message("  key format — panel: ",
          paste(sort(unique(nchar(head(panel_ids, 1000L)))), collapse = "/"),
          " chars, dashed=", any(grepl("-", head(panel_ids, 1000L))),
          " | extract: ",
          paste(sort(unique(nchar(head(file_ids, 1000L)))), collapse = "/"),
          " chars, dashed=", any(grepl("-", head(file_ids, 1000L))))
  message("  join — ", format(n_in_panel, big.mark = ","), " of ",
          format(length(file_ids), big.mark = ","),
          " extract parcels are in the panel (", pct_file,
          "%; the extract is countywide, the panel is Seattle)")
  message("  join — ", format(n_covered, big.mark = ","), " of ",
          format(length(panel_ids), big.mark = ","),
          " panel parcels have at least one permit (", pct_panel, "%)")

  if (n_in_panel == 0L) {
    warning("kcap: ZERO parcels joined. The key formats above disagree — ",
            "this is the SDCI dashed-parcel_id failure mode. ",
            "kcap_* columns will be constant and the models will drop them.")
    message("  ❌ ZERO JOIN — see warning above")
  } else if (pct_panel < 1) {
    warning("kcap: only ", pct_panel, "% of panel parcels matched. ",
            "Check the key format before trusting these features.")
  }

  # ---- 2. Trailing 3-year windows ------------------------------------------
  # Built on a compact grid rather than on the 5.1M-row panel: a parcel-year
  # can only be nonzero if it is within 2 years after some permit, so the grid
  # is (activity years) x {0,1,2}, which is a few hundred thousand rows.  The
  # window itself is a non-equi self-join, so it does not assume the panel or
  # the grid has a contiguous year sequence per parcel the way a shift() or a
  # rollapplyr() would.
  ann_win <- ann[tax_yr >= yr_min - 2L]
  grid <- unique(data.table::rbindlist(list(
    ann_win[, .(parcel_id, tax_yr)],
    ann_win[, .(parcel_id, tax_yr = tax_yr + 1L)],
    ann_win[, .(parcel_id, tax_yr = tax_yr + 2L)])))
  grid <- grid[tax_yr >= yr_min & tax_yr <= yr_max & parcel_id %chin% panel_ids]
  grid[, `:=`(lo = tax_yr - 2L, hi = tax_yr)]

  # An empty grid means nothing joined; go straight to the zero-fill rather
  # than calling max() over no rows and emitting a -Inf warning that reads
  # like a real problem on top of the join diagnostics above.
  win <- if (!nrow(grid)) {
    data.table::data.table(
      parcel_id = character(), tax_yr = integer(),
      kcap_permits_3yr = integer(), kcap_newconst_3yr = integer(),
      kcap_remodel_3yr = integer(), kcap_demo_3yr = integer(),
      kcap_desc_major_3yr = integer(),
      kcap_val_3yr = numeric(), kcap_val_max_3yr = numeric())
  } else ann[grid,
             on = .(parcel_id, tax_yr >= lo, tax_yr <= hi),
             by = .EACHI,
             .(kcap_permits_3yr    = sum(kcap_n_all),
               kcap_newconst_3yr   = sum(kcap_n_newconst),
               kcap_remodel_3yr    = sum(kcap_n_remodel),
               kcap_demo_3yr       = sum(kcap_n_demo),
               kcap_desc_major_3yr = as.integer(sum(kcap_n_desc_major) > 0L),
               kcap_val_3yr        = sum(kcap_val_sum),
               kcap_val_max_3yr    = max(kcap_val_max))]
  # The non-equi join names BOTH range-bound output columns after x's column,
  # so `win` comes back with two columns literally called "tax_yr" (the lower
  # bound then the upper). Rename positionally — by name is ambiguous, and
  # which suffix data.table appends has varied across versions. Columns 2 and
  # 3 are lo and hi; hi is the grid's tax_yr.
  if (identical(names(win)[1:3], c("parcel_id", "tax_yr", "tax_yr"))) {
    data.table::setnames(win, 1:3, c("parcel_id", ".lo", "tax_yr"))
    win[, .lo := NULL]
  } else {
    # The empty-grid shortcut above already has the right names. Anything else
    # means the join's output shape changed under us — stop rather than carry
    # on and join on the wrong column.
    stopifnot(identical(names(win)[1:2], c("parcel_id", "tax_yr")))
  }
  data.table::setkey(win, parcel_id, tax_yr)

  rm(ann_win, grid); gc(verbose = FALSE)
  message("  window rows built: ", format(nrow(win), big.mark = ","))

  # ---- 3. Years since new construction --------------------------------------
  # Rolling join over the FULL permit history (not just the 3-year window and
  # not just panel years), so a 1952 new-construction permit still dates a
  # 2015 parcel-year.  NA — not 0 — when the parcel has never had one, so the
  # model can split on missingness instead of reading "never built" as "built
  # this year".
  if (nrow(nc)) {
    pt[, .last_nc := nc[.SD, on = .(parcel_id, tax_yr), roll = TRUE, x.tax_yr],
       .SDcols = c("parcel_id", "tax_yr")]
    pt[, kcap_years_since_newconst :=
         data.table::fifelse(is.na(.last_nc), NA_real_,
                             as.numeric(tax_yr - .last_nc))]
    pt[, .last_nc := NULL]
  } else {
    pt[, kcap_years_since_newconst := NA_real_]
  }

  # ---- 4. Join the windows into the panel -----------------------------------
  pt[win, on = .(parcel_id, tax_yr), `:=`(
    kcap_permits_3yr    = i.kcap_permits_3yr,
    kcap_newconst_3yr   = i.kcap_newconst_3yr,
    kcap_remodel_3yr    = i.kcap_remodel_3yr,
    kcap_demo_3yr       = i.kcap_demo_3yr,
    kcap_desc_major_3yr = i.kcap_desc_major_3yr,
    kcap_val_3yr        = i.kcap_val_3yr,
    kcap_val_max_3yr    = i.kcap_val_max_3yr)]

  rm(win); gc(verbose = FALSE)

  # A panel row outside every window genuinely had no permit activity, so 0 is
  # the right fill here — unlike kcap_years_since_newconst, where 0 would be a
  # lie.  Counts stay integer to keep ~5.1M rows cheap.
  for (cc in kcap_count_cols)
    pt[is.na(get(cc)), (cc) := 0L]
  for (cc in c("kcap_val_3yr", "kcap_val_max_3yr"))
    pt[is.na(get(cc)), (cc) := 0]

  # ---- 5. Value transforms --------------------------------------------------
  # PermitVal is already winsorized at the 99.5th percentile of nonzero values
  # in the read layer; log1p handles the 17% zeros and what is left of the
  # skew.  Raw columns dropped once the logs exist (see header).
  pt[, log_kcap_val_3yr     := log1p(pmax(kcap_val_3yr,     0))]
  pt[, log_kcap_val_max_3yr := log1p(pmax(kcap_val_max_3yr, 0))]
  pt[, c("kcap_val_3yr", "kcap_val_max_3yr") := NULL]

  # ---- 6. Coverage report ---------------------------------------------------
  # Follows the report_cols pattern in xx_combine_parcel_history_changes.R: a
  # silent guard is how the missing gate columns went unnoticed for months.
  present <- intersect(kcap_predictors, names(pt))
  missing <- setdiff(kcap_predictors, names(pt))
  all_na  <- present[vapply(present, function(cc) all(is.na(pt[[cc]])),
                            logical(1))]
  all_const <- present[vapply(present, function(cc)
    data.table::uniqueN(pt[[cc]], na.rm = TRUE) <= 1L, logical(1))]
  nz_rows <- pt[, sum(kcap_permits_3yr > 0, na.rm = TRUE)]

  message("  ✅ kcap features attached: ", length(present), " of ",
          length(kcap_predictors), " (", paste(present, collapse = ", "), ")")
  if (length(missing))
    message("    ❌ NOT attached: ", paste(missing, collapse = ", "))
  message("    all-NA: ", length(all_na),
          if (length(all_na)) paste0(" (", paste(all_na, collapse = ", "), ")") else "",
          " | constant: ", length(all_const),
          if (length(all_const)) paste0(" (", paste(all_const, collapse = ", "), ")") else "")
  message("    parcel-years with a permit in the trailing 3 yrs: ",
          format(nz_rows, big.mark = ","), " of ",
          format(nrow(pt), big.mark = ","), " (",
          round(100 * nz_rows / nrow(pt), 1), "%)")
  message("    parcel-years with a dated new-construction permit: ",
          format(pt[, sum(!is.na(kcap_years_since_newconst))], big.mark = ","))

  if (length(all_na) || length(all_const))
    warning("kcap: ", length(all_na), " all-NA and ", length(all_const),
            " constant feature(s) — the models will drop these as ",
            "zero-variance. See the join diagnostics above.")

  panel_tbl <- pt
  assign("panel_tbl", pt, envir = .GlobalEnv)
  rm(ann, nc, res, pt)
  gc(verbose = FALSE)
}

message("xx_kca_permits_to_panel.R loaded")
