# =============================================================================
# 05_extend_panel_2026_2031_condo.R
# Extend the condo panel from the last retrofit year through 2031.
# Condo analog to 05_extend_panel_2026_2031.R.
#
# Key differences:
#   - Market signal is NWMLS condo (not SFH)
#   - Static snapshot is at the UNIT level (Major-Minor)
#   - Complex attributes are static (buildings don't change year-to-year)
#
# Reads:
#   panel_tbl_retro_condo            (from step 4 retrofit)
#   nwmls_condo_fcst_*_<scenario>.rds (from xx_nwmls_condo_to_panel.R)
#   econ_fcst_*_<scenario>.rds        (shared macro)
#
# Writes:
#   panel_tbl_2006_2031_inputs_<scenario>_condo  (GlobalEnv + cache)
# =============================================================================

scenario  <- get("scenario",  envir = .GlobalEnv)
cache_dir <- get("cache_dir", envir = .GlobalEnv)

stopifnot(exists("panel_tbl_retro_condo", envir = .GlobalEnv))
panel_hist <- copy(panel_tbl_retro_condo)
setDT(panel_hist)

base_yr    <- max(panel_hist$tax_yr, na.rm = TRUE)
fcst_years <- seq(base_yr + 1L,
                  get0("forecast_end", envir = .GlobalEnv, ifnotfound = 2032L))
message("  Extending condo panel ", base_yr + 1, " – ", max(fcst_years),
        " | units: ",
        uniqueN(panel_hist$parcel_id))

# ---- Load NWMLS condo forecast ---------------------------------------------
nwmls_files <- list.files(cache_dir,
  pattern    = paste0("nwmls_condo_fcst_.*_", scenario, "\\.rds$"),
  full.names = TRUE)

if (length(nwmls_files) > 0) {
  nwmls_fcst_ann <- readRDS(nwmls_files[which.max(file.info(nwmls_files)$mtime)])
  setDT(nwmls_fcst_ann)
  nwmls_fcst_ann <- nwmls_fcst_ann[tax_yr %in% fcst_years]
  message("  NWMLS condo forecast rows: ", nrow(nwmls_fcst_ann))
} else {
  warning("No NWMLS condo forecast cache found — market cols will be NA.")
  nwmls_fcst_ann <- data.table(tax_yr = fcst_years)
}

# ---- Load econ forecast ----------------------------------------------------
econ_files <- list.files(cache_dir,
  pattern    = paste0("econ_fcst_.*_", scenario, "\\.rds$"),
  full.names = TRUE)
# econ forecast is saved as a wide table (one row per tax_yr, one col per variable)
# by xx_econ_to_panel.R — merge directly, no dcast needed.
econ_fcst_wide <- NULL
if (length(econ_files) > 0) {
  econ_fcst_wide <- setDT(readRDS(econ_files[which.max(file.info(econ_files)$mtime)]))
  if ("tax_yr" %in% names(econ_fcst_wide)) {
    econ_fcst_wide <- econ_fcst_wide[tax_yr %in% fcst_years]
  } else {
    warning("econ forecast has no tax_yr column — skipping econ join.")
    econ_fcst_wide <- NULL
  }
}

# ---- Take last observed year as static unit snapshot -----------------------
last_snap <- panel_hist[tax_yr == base_yr]
yr_dt     <- data.table(tax_yr = fcst_years, join_key = 1L)
last_snap[, join_key := 1L]

extended <- last_snap[yr_dt, on = "join_key", allow.cartesian = TRUE]
extended[, join_key := NULL]
extended[, tax_yr := i.tax_yr]
if ("i.tax_yr" %in% names(extended)) extended[, i.tax_yr := NULL]
setDT(extended)

# Null out target AV columns
av_target_cols <- c("appr_land_val","appr_imps_val","total_assessed",
                     "log_appr_land_val","log_appr_imps_val","log_total_assessed",
                     "log_appr_land_val_lag1","log_appr_land_val_lag2",
                     "log_appr_imps_val_lag1")
for (col in intersect(av_target_cols, names(extended)))
  extended[, (col) := NA_real_]

# ---- Attach market forecasts -----------------------------------------------
if (nrow(nwmls_fcst_ann) > 0) {
  nwmls_cols <- setdiff(names(nwmls_fcst_ann), "tax_yr")
  # Overwrite stale market cols with forecast values
  for (col in nwmls_cols) {
    if (col %in% names(extended)) extended[, (col) := NULL]
  }
  extended <- merge(extended, nwmls_fcst_ann, by = "tax_yr", all.x = TRUE)
}
if (!is.null(econ_fcst_wide) && nrow(econ_fcst_wide) > 0)
  extended <- merge(extended, econ_fcst_wide, by = "tax_yr", all.x = TRUE)

# ---- Stack historical + extended -------------------------------------------
all_cols <- union(names(panel_hist), names(extended))
add_missing <- function(dt, cols) {
  for (col in setdiff(cols, names(dt))) dt[, (col) := NA]
  dt
}
panel_hist <- add_missing(panel_hist, all_cols)
extended   <- add_missing(extended,   all_cols)
setcolorder(panel_hist, all_cols)
setcolorder(extended,   all_cols)

panel_full_condo <- rbindlist(list(panel_hist, extended), use.names = TRUE)
setkeyv(panel_full_condo, c("parcel_id","tax_yr"))

message("  Extended condo panel rows: ", nrow(panel_full_condo),
        " | years: ", min(panel_full_condo$tax_yr), " – ", max(panel_full_condo$tax_yr))

out_name <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_condo")
assign(out_name, panel_full_condo, envir = .GlobalEnv)

# Parameterized alias so run_main_ml()'s horizon-based cache naming (and
# forecast_only=TRUE reloads) can find the condo extended panel.  The legacy
# out_name above is kept because 06_forecast_..._condo.R reads it directly.
alias_name <- paste0("panel_tbl_",
                     get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2027L), "_",
                     get0("forecast_end",   envir = .GlobalEnv, ifnotfound = 2032L),
                     "_inputs_", scenario, "_condo")
if (!identical(alias_name, out_name))
  assign(alias_name, panel_full_condo, envir = .GlobalEnv)

out_cache <- file.path(cache_dir, paste0(out_name, ".rds"))
saveRDS(panel_full_condo, out_cache)
message("  💾 cached: ", basename(out_cache))
message("05_extend_panel_2026_2031_condo.R loaded (scenario = ", scenario, ")")
