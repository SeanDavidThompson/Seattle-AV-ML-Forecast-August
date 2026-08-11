# 05_extend_panel_2026_2031.R --------------------------------------------
# Extend retrofitted parcel-year panel to 2031 with forecasted econ + NWMLS
# inputs. AV values are left NA for 2026-2031; 06_forecast fills them.
#
# Reads `scenario` from .GlobalEnv (set by main_ml.R) to select the correct
# econ and NWMLS forecast caches and name the output.
#
# Inputs (all in cache_dir/):
#   panel_tbl_retro.rds
#   econ_fcst_2026_2031_<scenario>.rds   (written by xx_econ_to_panel.R)
#   nwmls_fcst_2026_2031_<scenario>.rds  (written by xx_nwmls_to_panel.R)
#
# Output:
#   cache/panel_tbl_2006_2031_inputs_<scenario>.rds
#   wrangled/parcel_year_panel_2006_2031_inputs_<scenario>.parquet
# -------------------------------------------------------------------------

cache_dir  <- get0("cache_dir",  envir = .GlobalEnv, ifnotfound = here("data", "cache"))
output_dir <- get0("output_dir", envir = .GlobalEnv, ifnotfound = here("data", "outputs"))
dir.create(cache_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Read scenario from main_ml.R (validated there); default to baseline if run standalone
scenario <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
message("05_extend_panel_2026_2031.R: scenario = ", scenario)

# -------------------------------------------------------------------------
# 0) Load retrofitted historical panel
# -------------------------------------------------------------------------
# main_ml.R assigns panel_tbl_retro in GlobalEnv before sourcing this script.
# Fall back to the _res cache file for standalone use.
if (exists("panel_tbl_retro", envir = .GlobalEnv)) {
  panel_hist <- as.data.table(get("panel_tbl_retro", envir = .GlobalEnv))
} else {
  retro_path <- file.path(cache_dir, "panel_tbl_retro_res.rds")
  if (!file.exists(retro_path)) stop(
    "Missing: ", retro_path,
    "\nRun with retrofit_replicate=TRUE to build it."
  )
  panel_hist <- as.data.table(readRDS(retro_path))
  message("Loaded panel_tbl_retro from cache: ", basename(retro_path))
}
stopifnot(all(c("parcel_id", "tax_yr") %in% names(panel_hist)))
setorder(panel_hist, parcel_id, tax_yr)

hist_max_yr  <- max(panel_hist$tax_yr, na.rm = TRUE)
future_years <- (hist_max_yr + 1):get0("forecast_end", envir = .GlobalEnv, ifnotfound = 2032L)

message("History through tax_yr = ", hist_max_yr)
message("Extending to: ", paste(future_years, collapse = ", "))

# -------------------------------------------------------------------------
# 1) Load scenario-tagged forecast inputs
# -------------------------------------------------------------------------
econ_path  <- file.path(cache_dir, paste0("econ_fcst_2026_2031_",  scenario, ".rds"))
nwmls_path <- file.path(cache_dir, paste0("nwmls_fcst_2026_2031_", scenario, ".rds"))

if (!file.exists(econ_path)) stop(
  "Missing econ forecast: ", econ_path,
  "\nRun xx_econ_to_panel.R with scenario='", scenario, "' first."
)
if (!file.exists(nwmls_path)) stop(
  "Missing NWMLS forecast: ", nwmls_path,
  "\nRun xx_nwmls_to_panel.R with scenario='", scenario, "' first."
)

econ_fcst  <- as.data.table(readRDS(econ_path))
nwmls_fcst <- as.data.table(readRDS(nwmls_path))

stopifnot("tax_yr" %in% names(econ_fcst), "tax_yr" %in% names(nwmls_fcst))

# Keep only future years; enforce uniqueness
econ_fcst  <- unique(econ_fcst[tax_yr %in% future_years])
nwmls_fcst <- unique(nwmls_fcst[tax_yr %in% future_years])

if (nrow(econ_fcst) != length(future_years)) warning(
  "econ_fcst has ", nrow(econ_fcst), " rows for ", length(future_years), " future years"
)
if (nrow(nwmls_fcst) != length(future_years)) warning(
  "nwmls_fcst has ", nrow(nwmls_fcst), " rows for ", length(future_years), " future years"
)

# -------------------------------------------------------------------------
# 2) Build carry-forward base state from each parcel's last observed row
#
# Strategy: freeze parcel attributes at their last known state (typically 2025).
# Time-varying features like years_since_newconst are updated in step 4.
# AV values and prediction columns are dropped — they will be predicted in 06.
# -------------------------------------------------------------------------
last_row <- panel_hist[, .SD[.N], by = parcel_id]

drop_cols <- intersect(names(last_row), c(
  "appr_land_val", "appr_imps_val", "total_assessed",
  "log_appr_land_val", "log_appr_imps_val", "log_total_assessed",
  "appr_land_val_filled", "appr_imps_val_filled", "total_assessed_filled",
  "log_total_assessed_filled",
  "pred_log_land", "pred_land_val", "pred_log_impr", "pred_impr_val"
))

base_state <- copy(last_row)
if (length(drop_cols) > 0) base_state[, (drop_cols) := NULL]

# -------------------------------------------------------------------------
# 3) Expand to future years
# -------------------------------------------------------------------------
future_grid <- CJ(parcel_id = unique(panel_hist$parcel_id), tax_yr = future_years)
setkey(future_grid, parcel_id)

future_panel <- base_state[future_grid, on = "parcel_id"]
future_panel[, tax_yr := i.tax_yr]
future_panel[, i.tax_yr := NULL]

# -------------------------------------------------------------------------
# 4) Evolve time-varying features
# -------------------------------------------------------------------------
# years_since_newconst increments each year from the last observed state
if ("years_since_newconst" %in% names(future_panel)) {
  future_panel[, years_since_newconst := years_since_newconst + (tax_yr - hist_max_yr)]
}

# Permit rolling windows are frozen at last-observed values (no scenario model
# for future permits). The lag features will be recomputed year-by-year in 06.

# -------------------------------------------------------------------------
# 5) Join forecasted exogenous drivers (broadcast to all parcels by tax_yr)
# -------------------------------------------------------------------------
setkey(future_panel, tax_yr)
setkey(econ_fcst,    tax_yr)
setkey(nwmls_fcst,   tax_yr)

# Drop any stale econ/nwmls columns before joining to avoid duplicates
econ_cols  <- setdiff(names(econ_fcst),  "tax_yr")
nwmls_cols <- setdiff(names(nwmls_fcst), "tax_yr")

for (cn in intersect(c(econ_cols, nwmls_cols), names(future_panel))) {
  future_panel[, (cn) := NULL]
}

future_panel <- econ_fcst[future_panel,  on = "tax_yr"]
future_panel <- nwmls_fcst[future_panel, on = "tax_yr"]

# -------------------------------------------------------------------------
# 6) Create blank AV targets + lag placeholders for 2026-2031
#    06_forecast will fill log_land_filled / log_impr_filled year-by-year.
# -------------------------------------------------------------------------
future_panel[, appr_land_val  := NA_real_]
future_panel[, appr_imps_val  := NA_real_]
future_panel[, total_assessed := NA_real_]

future_panel[, log_appr_land_val  := NA_real_]
future_panel[, log_appr_imps_val  := NA_real_]
future_panel[, log_total_assessed := NA_real_]

# Lag columns: create as NA; 06_forecast rebuilds them each year via shift()
lag_cols <- grep("_lag\\d+$", names(panel_hist), value = TRUE)
for (lc in setdiff(lag_cols, names(future_panel))) {
  future_panel[, (lc) := NA_real_]
}

# -------------------------------------------------------------------------
# 7) Combine history + future, add scenario tag, write outputs
# -------------------------------------------------------------------------
panel_hist[, scenario := scenario]
future_panel[, scenario := scenario]

panel_all <- rbindlist(list(panel_hist, future_panel), use.names = TRUE, fill = TRUE)
setorder(panel_all, parcel_id, tax_yr)

# Quick QA
qa <- panel_all[tax_yr %in% future_years, .(
  n_rows           = .N,
  n_unique_parcels = uniqueN(parcel_id),
  n_missing_econ   = sum(is.na(.SD[[econ_cols[1]]]),  na.rm = TRUE),
  n_missing_nwmls  = sum(is.na(.SD[[nwmls_cols[1]]]), na.rm = TRUE)
), .SDcols = c(econ_cols[1], nwmls_cols[1])]
print(qa)

out_stem    <- paste0("panel_tbl_2006_2031_inputs_", scenario)
out_rds     <- file.path(cache_dir,    paste0(out_stem, ".rds"))
out_parquet <- file.path(output_dir,   paste0("parcel_year_", out_stem, ".parquet"))

saveRDS(as_tibble(panel_all), out_rds)
message("💾 saved: ", out_rds)

arrow::write_parquet(panel_all, out_parquet)
message("💾 saved: ", out_parquet)

# Assign to GlobalEnv so main_ml.R Step 5b can rename it to the _res-suffixed
# object and cache it under panel_tbl_2006_2031_inputs_<scenario>_res.rds.
assign(out_stem, panel_all, envir = .GlobalEnv)

message("05_extend_panel_2026_2031.R complete (scenario = ", scenario, ").")
