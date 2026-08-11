# =============================================================================
# 05_extend_panel_2026_2031_comm.R
# Extend the commercial panel from the last retrofit year through 2031.
# Commercial analog to 05_extend_panel_2026_2031.R.
#
# Key differences from residential:
#  - Market signal is CoStar (not NWMLS)
#  - No residential building features (sqft, grades, etc.)
#  - Uses commercial-specific feature set
#
# Reads:
#   panel_tbl_retro_com        (from step 4 retrofit)
#   costar_fcst_*_<scenario>.rds  (from xx_costar_to_panel.R)
#   econ_fcst_*_<scenario>.rds    (from xx_econ_to_panel.R — shared macro)
#
# Writes:
#   panel_tbl_2006_2031_inputs_<scenario>_com  (GlobalEnv + cache)
# =============================================================================


scenario  <- get("scenario",  envir = .GlobalEnv)
cache_dir <- get("cache_dir", envir = .GlobalEnv)

# run_main_ml() assigns the current subgroup's retro panel to the generic
# name panel_tbl_retro before sourcing this script; accept that first, with
# the legacy panel_tbl_retro_com name as fallback for standalone use.
if (exists("panel_tbl_retro", envir = .GlobalEnv)) {
  panel_com_hist <- copy(get("panel_tbl_retro", envir = .GlobalEnv))
} else {
  stopifnot(exists("panel_tbl_retro_com", envir = .GlobalEnv))
  panel_com_hist <- copy(panel_tbl_retro_com)
}
setDT(panel_com_hist)

# ---- Forecast years --------------------------------------------------------
base_yr    <- max(panel_com_hist$tax_yr, na.rm = TRUE)
fcst_years <- seq(base_yr + 1L, get0("forecast_end", envir = .GlobalEnv, ifnotfound = 2032L))
message("  Extending commercial panel from ", base_yr + 1, " to ",
        max(fcst_years), " ...")

# ---- Load CoStar forecast (prefer pre-built annual cache) ------------------
# xx_costar_to_panel.R saves two caches:
#   costar_ann_<scenario>.rds          — annual averages, already renamed to costar_*
#   costar_fcst_<yrs>_<scenario>.rds   — raw quarterly rows
# xx_costar_to_panel.R saves: costar_fcst_<minyear>_<maxyear>_<scenario>.rds
# This IS the annual series (already aggregated + renamed to costar_* cols).
costar_cache_pattern <- paste0("costar_fcst_.*_", scenario, "\\.rds$")
costar_ann_files <- list.files(cache_dir, pattern = costar_cache_pattern,
                                full.names = TRUE)

if (length(costar_ann_files) > 0) {
  ann_path <- costar_ann_files[which.max(file.info(costar_ann_files)$mtime)]
  costar_fcst_ann <- setDT(readRDS(ann_path))
  costar_fcst_ann <- costar_fcst_ann[tax_yr %in% fcst_years]
  message("  Loaded CoStar annual cache: ", basename(ann_path),
          " | fcst rows: ", nrow(costar_fcst_ann))
} else {
  # Fall back to raw quarterly cache (pre-2024 runs)
  costar_files <- list.files(cache_dir,
                              pattern    = paste0("costar_raw_qtrly_", scenario, "\\.rds$"),
                              full.names = TRUE)
  if (length(costar_files) > 0) {
    costar_q <- setDT(readRDS(costar_files[which.max(file.info(costar_files)$mtime)]))
    # raw quarterly has: yearq, tax_yr, quarter, vr_nro_q, vr_mf_q, ar_mf_q
    if (!"tax_yr" %in% names(costar_q))
      costar_q[, tax_yr := as.integer(stringr::str_extract(yearq, "^\\d{4}"))]
    costar_fcst_ann <- costar_q[
      tax_yr %in% fcst_years,
      .(costar_vr_nro = mean(vr_nro_q, na.rm = TRUE),
        costar_vr_mf  = mean(vr_mf_q,  na.rm = TRUE),
        costar_ar_mf  = mean(ar_mf_q,  na.rm = TRUE)),
      by = tax_yr
    ]
  } else {
    warning("No CoStar cache found — costar_* columns will be NA in extended panel.")
    costar_fcst_ann <- data.table(tax_yr = fcst_years)
  }
}

# ---- Load econ forecast (shared macro variables) ---------------------------
econ_cache_pattern <- paste0("econ_fcst_.*_", scenario, "\\.rds$")
econ_files <- list.files(cache_dir, pattern = econ_cache_pattern,
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

# ---- Take last observed year as template for static parcel features --------
last_yr_snap <- panel_com_hist[tax_yr == base_yr]

# For each parcel, replicate into forecast years
parcels <- unique(last_yr_snap$parcel_id)
message("  Parcels to extend: ", length(parcels))

# Cross join: parcel × forecast year
yr_dt <- data.table(tax_yr = fcst_years, join_key = 1L)
snap_dt <- last_yr_snap[, join_key := 1L]

extended_com <- snap_dt[yr_dt, on = "join_key", allow.cartesian = TRUE]
extended_com[, join_key := NULL]
# Overwrite tax_yr with the forecast year
extended_com[, tax_yr := i.tax_yr]
if ("i.tax_yr" %in% names(extended_com))
  extended_com[, i.tax_yr := NULL]

setDT(extended_com)

# ---- Null out forward-looking AV (to be predicted) -------------------------
av_target_cols <- c("appr_land_val","appr_imps_val","total_assessed",
                     "log_appr_land_val","log_appr_imps_val","log_total_assessed",
                     "log_appr_land_val_lag1","log_appr_land_val_lag2",
                     "log_appr_imps_val_lag1")
for (col in intersect(av_target_cols, names(extended_com))) {
  extended_com[, (col) := NA_real_]
}

# ---- Attach forecast market variables --------------------------------------
if (nrow(costar_fcst_ann) > 0) {
  extended_com <- merge(extended_com, costar_fcst_ann,
                         by = "tax_yr", all.x = TRUE, suffixes = c("", ".fcst"))
  # Overwrite existing costar_ cols with forecast values
  for (col in names(costar_fcst_ann)) {
    new_col <- paste0(col, ".fcst")
    if (new_col %in% names(extended_com)) {
      extended_com[!is.na(get(new_col)), (col) := get(new_col)]
      extended_com[, (new_col) := NULL]
    }
  }
}

if (!is.null(econ_fcst_wide) && nrow(econ_fcst_wide) > 0) {
  extended_com <- merge(extended_com, econ_fcst_wide,
                         by = "tax_yr", all.x = TRUE)
}

# ---- Stack historical + extended ------------------------------------------
all_hist_cols  <- names(panel_com_hist)
all_ext_cols   <- names(extended_com)
all_cols_union <- union(all_hist_cols, all_ext_cols)

add_missing <- function(dt, cols) {
  for (col in setdiff(cols, names(dt))) dt[, (col) := NA]
  dt
}
panel_com_hist <- add_missing(panel_com_hist, all_cols_union)
extended_com   <- add_missing(extended_com,   all_cols_union)

setcolorder(panel_com_hist, all_cols_union)
setcolorder(extended_com,   all_cols_union)

panel_full_com <- rbindlist(list(panel_com_hist, extended_com), use.names = TRUE)
setkeyv(panel_full_com, c("parcel_id","tax_yr"))

message("  Extended commercial panel rows: ", nrow(panel_full_com),
        " | years: ", min(panel_full_com$tax_yr), "-", max(panel_full_com$tax_yr))

# ---- Export ----------------------------------------------------------------
out_name <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_com")
assign(out_name, panel_full_com, envir = .GlobalEnv)

# Horizon-parameterized alias — run_main_ml() renames this to the subgroup-
# specific object and manages its caching/cleanup.  Bindings share memory,
# so the alias is free.
alias_name <- paste0("panel_tbl_",
                     get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2027L), "_",
                     get0("forecast_end",   envir = .GlobalEnv, ifnotfound = 2031L),
                     "_inputs_", scenario, "_com")
if (!identical(alias_name, out_name))
  assign(alias_name, panel_full_com, envir = .GlobalEnv)

out_cache <- file.path(cache_dir, paste0(out_name, ".rds"))
saveRDS(panel_full_com, out_cache)
message("  💾 cached: ", basename(out_cache))

message("05_extend_panel_2026_2031_comm.R loaded (scenario = ", scenario, ")")
