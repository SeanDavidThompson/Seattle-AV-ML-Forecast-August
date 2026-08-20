# =============================================================================
# 06_forecast_av_2026_2031_sequential_condo.R
# Year-by-year sequential AV forecast for CONDO unit parcels, 2026-2031.
#
# Logic (per year t):
#   1. Pull year-t rows from extended panel
#   2. Fill lag columns from t-1 predictions (or historical for t == first year)
#   3. Score LAND with lgb_condo_land_delta_cv (delta), fall back to level
#   4. Score IMPR with lgb_condo_impr_delta_cv (delta), fall back to level
#      (units with land but no improvement history are held at 0 - see the
#       land-only gate below; toggle with forecast_gate_land_only)
#   5. Back-transform; store predictions for t+1 lag inputs
#
# Writes:
#   cache/panel_tbl_2006_2031_forecasted_<scenario>_condo.rds
#   outputs/parcel_year_panel_2006_2031_forecasted_<scenario>_condo.parquet
# =============================================================================

scenario   <- get("scenario",   envir = .GlobalEnv)
cache_dir  <- get("cache_dir",  envir = .GlobalEnv)
output_dir <- get("output_dir", envir = .GlobalEnv)

# ---- Load extended panel ----------------------------------------------------
ext_name  <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_condo")
ext_cache <- file.path(cache_dir, paste0(ext_name, ".rds"))

if (exists(ext_name, envir = .GlobalEnv)) {
  panel_ext <- data.table::copy(get(ext_name, envir = .GlobalEnv))
} else if (file.exists(ext_cache)) {
  panel_ext <- readRDS(ext_cache)
  message("  Loaded condo extended panel from cache.")
} else {
  stop("Extended condo panel not found. Run 05_extend_panel_2026_2031_condo.R first.")
}

setDT(panel_ext)
setkeyv(panel_ext, c("parcel_id", "tax_yr"))

# ---- Defensive AV re-join ---------------------------------------------------
# The condo panel carries all-NA appr_* from the combine-script join defect.
# Without observed AV at the last observed year, the lag tracker below seeds
# NA for every unit, the delta path never fires, and ALL units fall to the
# level model — including the ~23K units with perfectly good observed values
# (measured effect: -18.5% matched-parcel 2027 from level-model shrinkage).
# Re-joining here restores observed AV for BOTH the lag tracker and the
# observed-year rows of the output cache.
.fs_chk <- get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2026L)
.lo_chk <- as.integer(.fs_chk) - 1L
.need_av <- !"log_appr_land_val" %in% names(panel_ext) ||
  panel_ext[tax_yr == .lo_chk, all(is.na(log_appr_land_val))]
if (.need_av) {
  av_cache_path <- file.path(cache_dir, "av_history_cln.rds")
  if (file.exists(av_cache_path)) {
    message("  AV all-NA at last observed year in condo extended panel — ",
            "re-joining from av_history_cln ...")
    av_fix <- data.table::as.data.table(readRDS(av_cache_path))
    av_fix[, parcel_id := gsub("-", "", parcel_id)]
    if (any(grepl("-", head(panel_ext$parcel_id, 10))))
      av_fix[, parcel_id := paste0(substr(parcel_id, 1, 6), "-",
                                   substr(parcel_id, 7, 10))]
    av_fix <- av_fix[parcel_id %in% unique(panel_ext$parcel_id),
                     .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
    panel_ext[av_fix, on = .(parcel_id, tax_yr),
              `:=`(appr_land_val = i.appr_land_val,
                   appr_imps_val = i.appr_imps_val)]
    for (cc in c("log_appr_land_val", "log_appr_imps_val",
                 "log_total_assessed"))
      if (!cc %in% names(panel_ext)) panel_ext[, (cc) := NA_real_]
    panel_ext[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
    panel_ext[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]
    panel_ext[, .tot_tmp :=
      fifelse(is.na(appr_land_val), 0, as.numeric(appr_land_val)) +
      fifelse(is.na(appr_imps_val), 0, as.numeric(appr_imps_val))]
    panel_ext[.tot_tmp > 0, log_total_assessed := log(.tot_tmp)]
    panel_ext[, .tot_tmp := NULL]
    rm(av_fix)
    message("  condo AV re-join: ",
            scales::comma(panel_ext[tax_yr == .lo_chk &
                                      !is.na(log_appr_land_val),
                                    data.table::uniqueN(parcel_id)]),
            " units carry observed ", .lo_chk,
            " values into the lag tracker (delta path); ",
            "uncovered units fall to the level model")
  } else {
    message("  ⚠️  av_history_cln.rds not found — all units will ",
            "use the level model")
  }
}

# ---- Load models (support both new split names and legacy aliases) -----------
resolve_cv <- function(...) {
  for (nm in c(...))
    if (exists(nm, envir = .GlobalEnv)) return(get(nm, envir = .GlobalEnv))
  NULL
}

land_delta_cv <- resolve_cv("lgb_condo_land_delta_cv", "lgb_condo_delta_cv")
land_level_cv <- resolve_cv("lgb_condo_land_level_cv", "lgb_condo_level_cv")
impr_delta_cv <- resolve_cv("lgb_condo_impr_delta_cv")
impr_level_cv <- resolve_cv("lgb_condo_impr_level_cv")

if (is.null(land_delta_cv) && is.null(land_level_cv))
  stop("No condo land models found. Run 03_model_condo_land.R first.")

has_land_delta <- !is.null(land_delta_cv)
has_land_level <- !is.null(land_level_cv)
has_impr_delta <- !is.null(impr_delta_cv)
has_impr_level <- !is.null(impr_level_cv)
has_impr       <- has_impr_delta || has_impr_level

# ---- Helper: safe predict ---------------------------------------------------
safe_predict <- function(cv_obj, data_dt) {
  x_cols <- cv_obj$x_cols
  dt     <- data.table::copy(data_dt)

  for (col in setdiff(x_cols, names(dt)))
    dt[, (col) := NA_real_]

  for (col in x_cols) {
    v <- dt[[col]]
    if (is.character(v) || is.factor(v))
      dt[, (col) := as.integer(as.factor(v))]
  }

  x_mat <- as.matrix(dt[, x_cols, with = FALSE])
  x_mat[!is.finite(x_mat)] <- NA_real_
  predict(cv_obj$model, x_mat)
}

# ---- Determine forecast years -----------------------------------------------
# forecast_start is set in main_ml.R CFG and pushed to GlobalEnv.
# Use it as the authoritative seed boundary so the script never iterates
# over historical years (which happens when log_total_assessed is all-NA
# in a cold-start or partially-built panel, causing max() to return -Inf).
.fcast_start <- get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2026L)
last_obs_yr <- as.integer(.fcast_start) - 1L
fcst_years  <- seq(as.integer(.fcast_start),
                   get0("forecast_end", envir = .GlobalEnv, ifnotfound = 2031L))
# Restrict to years actually present in the extended panel
fcst_years  <- fcst_years[fcst_years %in% unique(panel_ext$tax_yr)]
message("  Forecasting condo AV for years: ", paste(fcst_years, collapse = ", "))

# ---- LAND-ONLY UNITS: suppress invented improvement values ------------------
# A parcel with land but no improvements has appr_imps_val == 0 or NA in every
# historical year.  Those rows never enter the improvement training frames:
# build_subgroup_model_data() only takes log() of a positive appr_imps_val, so
# dlog_imps is NA for them and the mask drops them.
#
# The forecast still scores them.  The delta model is skipped because
# log_appr_imps_val_lag1 is NA, so they fall through to the IMPR level
# fallback, and exp() of any level prediction is strictly positive.  A unit
# that held zero improvements for a decade is assigned a building in the first
# forecast year.
#
# Condo is the largest affected track: 57,351 of 80,053 units (72%) had no
# usable 2026 improvement base, and 56,645 of those (98.8%) never carried a
# nonzero improvement value in 2015-2025.  The 706 units that DID have
# improvement history are excluded from the gate, since for them the level
# fallback is doing the right thing.
#
# NOTE for condo specifically: the gate requires land history, so units that
# carry neither land nor improvements are counted and reported but NOT gated.
# If condo unit value is recorded somewhere other than appr_land_val /
# appr_imps_val in this extract, check that count before trusting the result.
#
# Set forecast_gate_land_only = FALSE in the global env to restore the old
# behaviour for comparison.
GATE_LAND_ONLY <- isTRUE(get0("forecast_gate_land_only",
                              envir = .GlobalEnv, ifnotfound = TRUE))

land_only_ids <- character(0)
if (GATE_LAND_ONLY && "appr_imps_val" %in% names(panel_ext)) {
  .hist <- panel_ext[tax_yr <= last_obs_yr]
  .flags <- .hist[, .(
    ever_imps = any(!is.na(appr_imps_val) & appr_imps_val > 0),
    ever_land = any(!is.na(appr_land_val) & appr_land_val > 0)
  ), by = parcel_id]

  land_only_ids <- .flags[ever_imps == FALSE & ever_land == TRUE, parcel_id]
  .no_land <- .flags[ever_imps == FALSE & ever_land == FALSE, .N]

  message("  land-only gate: ", scales::comma(length(land_only_ids)),
          " of ", scales::comma(nrow(.flags)),
          " units have land but no improvement history through ",
          last_obs_yr, " - improvements held at 0")
  if (.no_land > 0)
    message("    note: ", scales::comma(.no_land),
            " unit(s) have neither land nor improvement history - NOT gated, ",
            "these need separate review")
  rm(.hist, .flags)
} else if (!GATE_LAND_ONLY) {
  message("  land-only gate: DISABLED (forecast_gate_land_only = FALSE)")
}

# ---- Initialise lag tracker from last observed year -------------------------
prev_preds <- panel_ext[tax_yr == last_obs_yr,
                         .(parcel_id,
                           pred_log_land  = log_appr_land_val,
                           pred_log_imps  = log_appr_imps_val,
                           pred_log_total = log_total_assessed)]

# Ensure prediction columns exist in panel_ext
pred_cols <- c("pred_log_land_val", "pred_log_imps_val",
               "pred_appr_land_val", "pred_appr_imps_val",
               "pred_total_assessed", "pred_log_total")
for (col in pred_cols) {
  if (!col %in% names(panel_ext))
    panel_ext[, (col) := NA_real_]
}

# ============================================================================
# YEAR LOOP
# ============================================================================
for (yr in fcst_years) {
  message("  Forecasting year: ", yr)

  yr_data <- data.table::copy(panel_ext[tax_yr == yr])

  # -- Fill lag columns from previous year predictions -----------------------
  lag_dt <- prev_preds[, .(
    parcel_id,
    log_appr_land_val_lag1 = pred_log_land,
    log_appr_imps_val_lag1 = pred_log_imps
  )]
  yr_data[, c("log_appr_land_val_lag1", "log_appr_imps_val_lag1") := NULL]
  yr_data <- merge(yr_data, lag_dt, by = "parcel_id", all.x = TRUE)

  # lag2 for land from panel_ext t-2 predictions
  if (yr > last_obs_yr + 1L) {
    lag2_dt <- panel_ext[tax_yr == yr - 2L,
                          .(parcel_id,
                            log_appr_land_val_lag2 = pred_log_land_val)]
    yr_data[, "log_appr_land_val_lag2" := NULL]
    yr_data <- merge(yr_data, lag2_dt, by = "parcel_id", all.x = TRUE)
  }

  # ---- LAND: delta model ---------------------------------------------------
  if (has_land_delta) {
    eligible <- !is.na(yr_data$log_appr_land_val_lag1)
    if (any(eligible))
      # Delta model predicts log-change; add lag1 anchor to reconstruct log level
      yr_data[eligible,
              pred_log_land_val := log_appr_land_val_lag1 + safe_predict(land_delta_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # LAND: level fallback
  if (has_land_level) {
    need_level <- is.na(yr_data$pred_log_land_val)
    if (any(need_level))
      yr_data[need_level,
              pred_log_land_val := safe_predict(land_level_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # ---- IMPR: delta model ---------------------------------------------------
  if (has_impr_delta) {
    eligible_impr <- !is.na(yr_data$log_appr_imps_val_lag1)
    if (any(eligible_impr))
      # Delta model predicts log-change; add lag1 anchor to reconstruct log level
      yr_data[eligible_impr,
              pred_log_imps_val := log_appr_imps_val_lag1 + safe_predict(impr_delta_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # IMPR: level fallback
  # Land-only units are excluded here: the level model was trained only on
  # rows with a positive improvement value, so scoring a unit that has never
  # had one is an extrapolation outside the training support, and exp() makes
  # the result strictly positive no matter what the model returns.
  if (has_impr_level) {
    need_impr_level <- is.na(yr_data$pred_log_imps_val)
    if (length(land_only_ids))
      need_impr_level <- need_impr_level & !(yr_data$parcel_id %in% land_only_ids)
    if (any(need_impr_level))
      yr_data[need_impr_level,
              pred_log_imps_val := safe_predict(impr_level_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # IMPR: ratio heuristic if no impr model available at all
  if (!has_impr) {
    hist_ratio <- panel_ext[
      tax_yr < yr & !is.na(appr_land_val) & !is.na(appr_imps_val) &
        appr_land_val > 0,
      .(ratio = median(appr_imps_val / appr_land_val, na.rm = TRUE)),
      by = parcel_id]
    global_ratio <- median(hist_ratio$ratio, na.rm = TRUE)
    yr_data <- merge(yr_data, hist_ratio, by = "parcel_id", all.x = TRUE)
    yr_data[is.na(ratio), ratio := global_ratio]
    .ratio_fill <- is.na(yr_data$pred_log_imps_val)
    if (length(land_only_ids))
      .ratio_fill <- .ratio_fill & !(yr_data$parcel_id %in% land_only_ids)
    yr_data[.ratio_fill,
            pred_log_imps_val := pred_log_land_val + log(ratio)]
    rm(.ratio_fill)
    yr_data[, ratio := NULL]
  }

  # ---- Back-transform -------------------------------------------------------
  yr_data[, pred_appr_land_val  := exp(pred_log_land_val)]
  yr_data[, pred_appr_imps_val  := exp(pred_log_imps_val)]

  # Land-only units carry zero improvements, not NA: an explicit 0 keeps them
  # in the matched-parcel population downstream (av_reconcile_certified.R drops
  # rows where av is NA) and keeps pred_total_assessed equal to land.
  if (length(land_only_ids)) {
    .lo <- yr_data$parcel_id %in% land_only_ids
    if (any(.lo)) yr_data[.lo, pred_appr_imps_val := 0]
    rm(.lo)
  }

  yr_data[, pred_total_assessed := pred_appr_land_val + pred_appr_imps_val]
  yr_data[, pred_log_total      := log(pmax(pred_total_assessed, 1))]

  # ---- Write predictions back into full panel -------------------------------
  panel_ext[tax_yr == yr, (pred_cols) := yr_data[, pred_cols, with = FALSE]]

  # ---- Update lag tracker ---------------------------------------------------
  prev_preds <- yr_data[, .(parcel_id,
                              pred_log_land  = pred_log_land_val,
                              pred_log_imps  = pred_log_imps_val,
                              pred_log_total = pred_log_total)]

  message("    Year ", yr, ": ",
          sum(!is.na(yr_data$pred_appr_land_val)), " units scored.")
  if (length(land_only_ids)) {
    .n_gated <- sum(yr_data$parcel_id %in% land_only_ids)
    if (.n_gated > 0)
      message("      land-only: ", scales::comma(.n_gated),
              " unit(s) held at 0 improvements")
    rm(.n_gated)
  }
}

# ============================================================================
# SAVE OUTPUTS
# ============================================================================
setkeyv(panel_ext, c("parcel_id", "tax_yr"))

cache_out <- file.path(
  cache_dir,
  paste0("panel_tbl_2006_2031_forecasted_", scenario, "_condo.rds")
)
saveRDS(panel_ext, cache_out)
message("  \U1f4be cached: ", basename(cache_out))

parquet_out <- file.path(
  output_dir,
  paste0("parcel_year_panel_2006_2031_forecasted_", scenario, "_condo.parquet")
)
tryCatch({
  arrow::write_parquet(panel_ext, parquet_out)
  message("  \U1f4be parquet: ", basename(parquet_out))
}, error = function(e) {
  message("  \u26a0\ufe0f  Parquet write failed: ", e$message)
  saveRDS(panel_ext, sub("\\.parquet$", ".rds", parquet_out))
  message("  \U1f4be saved as RDS instead.")
})

assign("panel_tbl_forecasted_condo", panel_ext, envir = .GlobalEnv)
message("\n06_forecast_av_2026_2031_sequential_condo.R loaded (scenario = ",
        scenario, ")")# =============================================================================
# 06_forecast_av_2026_2031_sequential_condo.R
# Year-by-year sequential AV forecast for CONDO unit parcels, 2026-2031.
#
# Logic (per year t):
#   1. Pull year-t rows from extended panel
#   2. Fill lag columns from t-1 predictions (or historical for t == first year)
#   3. Score LAND with lgb_condo_land_delta_cv (delta), fall back to level
#   4. Score IMPR with lgb_condo_impr_delta_cv (delta), fall back to level
#   5. Back-transform; store predictions for t+1 lag inputs
#
# Writes:
#   cache/panel_tbl_2006_2031_forecasted_<scenario>_condo.rds
#   outputs/parcel_year_panel_2006_2031_forecasted_<scenario>_condo.parquet
# =============================================================================

scenario   <- get("scenario",   envir = .GlobalEnv)
cache_dir  <- get("cache_dir",  envir = .GlobalEnv)
output_dir <- get("output_dir", envir = .GlobalEnv)

# ---- Load extended panel ----------------------------------------------------
ext_name  <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_condo")
ext_cache <- file.path(cache_dir, paste0(ext_name, ".rds"))

if (exists(ext_name, envir = .GlobalEnv)) {
  panel_ext <- data.table::copy(get(ext_name, envir = .GlobalEnv))
} else if (file.exists(ext_cache)) {
  panel_ext <- readRDS(ext_cache)
  message("  Loaded condo extended panel from cache.")
} else {
  stop("Extended condo panel not found. Run 05_extend_panel_2026_2031_condo.R first.")
}

setDT(panel_ext)
setkeyv(panel_ext, c("parcel_id", "tax_yr"))

# ---- Defensive AV re-join ---------------------------------------------------
# The condo panel carries all-NA appr_* from the combine-script join defect.
# Without observed AV at the last observed year, the lag tracker below seeds
# NA for every unit, the delta path never fires, and ALL units fall to the
# level model — including the ~23K units with perfectly good observed values
# (measured effect: -18.5% matched-parcel 2027 from level-model shrinkage).
# Re-joining here restores observed AV for BOTH the lag tracker and the
# observed-year rows of the output cache.
.fs_chk <- get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2026L)
.lo_chk <- as.integer(.fs_chk) - 1L
.need_av <- !"log_appr_land_val" %in% names(panel_ext) ||
  panel_ext[tax_yr == .lo_chk, all(is.na(log_appr_land_val))]
if (.need_av) {
  av_cache_path <- file.path(cache_dir, "av_history_cln.rds")
  if (file.exists(av_cache_path)) {
    message("  AV all-NA at last observed year in condo extended panel — ",
            "re-joining from av_history_cln ...")
    av_fix <- data.table::as.data.table(readRDS(av_cache_path))
    av_fix[, parcel_id := gsub("-", "", parcel_id)]
    if (any(grepl("-", head(panel_ext$parcel_id, 10))))
      av_fix[, parcel_id := paste0(substr(parcel_id, 1, 6), "-",
                                   substr(parcel_id, 7, 10))]
    av_fix <- av_fix[parcel_id %in% unique(panel_ext$parcel_id),
                     .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
    panel_ext[av_fix, on = .(parcel_id, tax_yr),
              `:=`(appr_land_val = i.appr_land_val,
                   appr_imps_val = i.appr_imps_val)]
    for (cc in c("log_appr_land_val", "log_appr_imps_val",
                 "log_total_assessed"))
      if (!cc %in% names(panel_ext)) panel_ext[, (cc) := NA_real_]
    panel_ext[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
    panel_ext[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]
    panel_ext[, .tot_tmp :=
      fifelse(is.na(appr_land_val), 0, as.numeric(appr_land_val)) +
      fifelse(is.na(appr_imps_val), 0, as.numeric(appr_imps_val))]
    panel_ext[.tot_tmp > 0, log_total_assessed := log(.tot_tmp)]
    panel_ext[, .tot_tmp := NULL]
    rm(av_fix)
    message("  condo AV re-join: ",
            scales::comma(panel_ext[tax_yr == .lo_chk &
                                      !is.na(log_appr_land_val),
                                    data.table::uniqueN(parcel_id)]),
            " units carry observed ", .lo_chk,
            " values into the lag tracker (delta path); ",
            "uncovered units fall to the level model")
  } else {
    message("  ⚠️  av_history_cln.rds not found — all units will ",
            "use the level model")
  }
}

# ---- Load models (support both new split names and legacy aliases) -----------
resolve_cv <- function(...) {
  for (nm in c(...))
    if (exists(nm, envir = .GlobalEnv)) return(get(nm, envir = .GlobalEnv))
  NULL
}

land_delta_cv <- resolve_cv("lgb_condo_land_delta_cv", "lgb_condo_delta_cv")
land_level_cv <- resolve_cv("lgb_condo_land_level_cv", "lgb_condo_level_cv")
impr_delta_cv <- resolve_cv("lgb_condo_impr_delta_cv")
impr_level_cv <- resolve_cv("lgb_condo_impr_level_cv")

if (is.null(land_delta_cv) && is.null(land_level_cv))
  stop("No condo land models found. Run 03_model_condo_land.R first.")

has_land_delta <- !is.null(land_delta_cv)
has_land_level <- !is.null(land_level_cv)
has_impr_delta <- !is.null(impr_delta_cv)
has_impr_level <- !is.null(impr_level_cv)
has_impr       <- has_impr_delta || has_impr_level

# ---- Helper: safe predict ---------------------------------------------------
safe_predict <- function(cv_obj, data_dt) {
  x_cols <- cv_obj$x_cols
  dt     <- data.table::copy(data_dt)

  for (col in setdiff(x_cols, names(dt)))
    dt[, (col) := NA_real_]

  for (col in x_cols) {
    v <- dt[[col]]
    if (is.character(v) || is.factor(v))
      dt[, (col) := as.integer(as.factor(v))]
  }

  x_mat <- as.matrix(dt[, x_cols, with = FALSE])
  x_mat[!is.finite(x_mat)] <- NA_real_
  predict(cv_obj$model, x_mat)
}

# ---- Determine forecast years -----------------------------------------------
# forecast_start is set in main_ml.R CFG and pushed to GlobalEnv.
# Use it as the authoritative seed boundary so the script never iterates
# over historical years (which happens when log_total_assessed is all-NA
# in a cold-start or partially-built panel, causing max() to return -Inf).
.fcast_start <- get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2026L)
last_obs_yr <- as.integer(.fcast_start) - 1L
fcst_years  <- seq(as.integer(.fcast_start),
                   get0("forecast_end", envir = .GlobalEnv, ifnotfound = 2031L))
# Restrict to years actually present in the extended panel
fcst_years  <- fcst_years[fcst_years %in% unique(panel_ext$tax_yr)]
message("  Forecasting condo AV for years: ", paste(fcst_years, collapse = ", "))

# ---- Initialise lag tracker from last observed year -------------------------
prev_preds <- panel_ext[tax_yr == last_obs_yr,
                         .(parcel_id,
                           pred_log_land  = log_appr_land_val,
                           pred_log_imps  = log_appr_imps_val,
                           pred_log_total = log_total_assessed)]

# Ensure prediction columns exist in panel_ext
pred_cols <- c("pred_log_land_val", "pred_log_imps_val",
               "pred_appr_land_val", "pred_appr_imps_val",
               "pred_total_assessed", "pred_log_total")
for (col in pred_cols) {
  if (!col %in% names(panel_ext))
    panel_ext[, (col) := NA_real_]
}

# ============================================================================
# YEAR LOOP
# ============================================================================
for (yr in fcst_years) {
  message("  Forecasting year: ", yr)

  yr_data <- data.table::copy(panel_ext[tax_yr == yr])

  # -- Fill lag columns from previous year predictions -----------------------
  lag_dt <- prev_preds[, .(
    parcel_id,
    log_appr_land_val_lag1 = pred_log_land,
    log_appr_imps_val_lag1 = pred_log_imps
  )]
  yr_data[, c("log_appr_land_val_lag1", "log_appr_imps_val_lag1") := NULL]
  yr_data <- merge(yr_data, lag_dt, by = "parcel_id", all.x = TRUE)

  # lag2 for land from panel_ext t-2 predictions
  if (yr > last_obs_yr + 1L) {
    lag2_dt <- panel_ext[tax_yr == yr - 2L,
                          .(parcel_id,
                            log_appr_land_val_lag2 = pred_log_land_val)]
    yr_data[, "log_appr_land_val_lag2" := NULL]
    yr_data <- merge(yr_data, lag2_dt, by = "parcel_id", all.x = TRUE)
  }

  # ---- LAND: delta model ---------------------------------------------------
  if (has_land_delta) {
    eligible <- !is.na(yr_data$log_appr_land_val_lag1)
    if (any(eligible))
      # Delta model predicts log-change; add lag1 anchor to reconstruct log level
      yr_data[eligible,
              pred_log_land_val := log_appr_land_val_lag1 + safe_predict(land_delta_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # LAND: level fallback
  if (has_land_level) {
    need_level <- is.na(yr_data$pred_log_land_val)
    if (any(need_level))
      yr_data[need_level,
              pred_log_land_val := safe_predict(land_level_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # ---- IMPR: delta model ---------------------------------------------------
  if (has_impr_delta) {
    eligible_impr <- !is.na(yr_data$log_appr_imps_val_lag1)
    if (any(eligible_impr))
      # Delta model predicts log-change; add lag1 anchor to reconstruct log level
      yr_data[eligible_impr,
              pred_log_imps_val := log_appr_imps_val_lag1 + safe_predict(impr_delta_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # IMPR: level fallback
  if (has_impr_level) {
    need_impr_level <- is.na(yr_data$pred_log_imps_val)
    if (any(need_impr_level))
      yr_data[need_impr_level,
              pred_log_imps_val := safe_predict(impr_level_cv, .SD),
              .SDcols = names(yr_data)]
  }

  # IMPR: ratio heuristic if no impr model available at all
  if (!has_impr) {
    hist_ratio <- panel_ext[
      tax_yr < yr & !is.na(appr_land_val) & !is.na(appr_imps_val) &
        appr_land_val > 0,
      .(ratio = median(appr_imps_val / appr_land_val, na.rm = TRUE)),
      by = parcel_id]
    global_ratio <- median(hist_ratio$ratio, na.rm = TRUE)
    yr_data <- merge(yr_data, hist_ratio, by = "parcel_id", all.x = TRUE)
    yr_data[is.na(ratio), ratio := global_ratio]
    yr_data[is.na(pred_log_imps_val),
            pred_log_imps_val := pred_log_land_val + log(ratio)]
    yr_data[, ratio := NULL]
  }

  # ---- Back-transform -------------------------------------------------------
  yr_data[, pred_appr_land_val  := exp(pred_log_land_val)]
  yr_data[, pred_appr_imps_val  := exp(pred_log_imps_val)]
  yr_data[, pred_total_assessed := pred_appr_land_val + pred_appr_imps_val]
  yr_data[, pred_log_total      := log(pmax(pred_total_assessed, 1))]

  # ---- Write predictions back into full panel -------------------------------
  panel_ext[tax_yr == yr, (pred_cols) := yr_data[, pred_cols, with = FALSE]]

  # ---- Update lag tracker ---------------------------------------------------
  prev_preds <- yr_data[, .(parcel_id,
                              pred_log_land  = pred_log_land_val,
                              pred_log_imps  = pred_log_imps_val,
                              pred_log_total = pred_log_total)]

  message("    Year ", yr, ": ",
          sum(!is.na(yr_data$pred_appr_land_val)), " units scored.")
}

# ============================================================================
# SAVE OUTPUTS
# ============================================================================
setkeyv(panel_ext, c("parcel_id", "tax_yr"))

cache_out <- file.path(
  cache_dir,
  paste0("panel_tbl_2006_2031_forecasted_", scenario, "_condo.rds")
)
saveRDS(panel_ext, cache_out)
message("  \U1f4be cached: ", basename(cache_out))

parquet_out <- file.path(
  output_dir,
  paste0("parcel_year_panel_2006_2031_forecasted_", scenario, "_condo.parquet")
)
tryCatch({
  arrow::write_parquet(panel_ext, parquet_out)
  message("  \U1f4be parquet: ", basename(parquet_out))
}, error = function(e) {
  message("  \u26a0\ufe0f  Parquet write failed: ", e$message)
  saveRDS(panel_ext, sub("\\.parquet$", ".rds", parquet_out))
  message("  \U1f4be saved as RDS instead.")
})

assign("panel_tbl_forecasted_condo", panel_ext, envir = .GlobalEnv)
message("\n06_forecast_av_2026_2031_sequential_condo.R loaded (scenario = ",
        scenario, ")")
