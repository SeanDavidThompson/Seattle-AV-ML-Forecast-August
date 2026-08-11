# =============================================================================
# 03_model_condo_land.R
# LightGBM land-value models for CONDO unit parcels.
#
# Mirrors 03_model_land.R (residential) in structure and CV method.
# Uses make_rolling_year_folds() from 00_init.R to enforce chronological
# cross-validation — the model never trains on future years to predict past.
#
# Condo land value notes:
#   - KCA assesses each condo unit as land + improvement.
#     appr_land_val at the unit level represents the unit's allocated share
#     of the complex's land value.
#   - Complex-level features (nbr_units, bldg_quality, amenities) are joined
#     onto the unit panel by 02_transfrm_condo.R / xx_combine_condo_panel.R.
#   - Market signal is NWMLS condo (not SFH) data.
#
# Two models trained:
#   DELTA  — predicts log(appr_land_val)  using prior-year lag as anchor.
#   LEVEL  — predicts log(appr_land_val)  directly (fallback, no lag needed).
#
# Outputs saved to model_dir/:
#   lgb_condo_land_delta_cv_<stamp>.rds   dv_condo_land_delta_<stamp>.rds
#   lgb_condo_land_level_cv_<stamp>.rds   dv_condo_land_level_<stamp>.rds
# Training frames saved to cache_dir/:
#   model_data_condo_land_delta.rds
#   model_data_condo_land_level.rds
#
# GlobalEnv objects exposed (consumed by 06_forecast_*_condo.R):
#   lgb_condo_land_delta_cv  dv_condo_land_delta
#   lgb_condo_land_level_cv  dv_condo_land_level
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lightgbm)
  library(here)
})

# ---- Globals ----------------------------------------------------------------
model_dir <- get("model_dir", envir = .GlobalEnv)
cache_dir  <- get("cache_dir", envir = .GlobalEnv)
stamp      <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Load panel_tbl_condo from cache if it was dropped from memory after Step 2
if (!exists("panel_tbl_condo", envir = .GlobalEnv)) {
  .p <- file.path(cache_dir, "panel_tbl_condo.rds")
  if (!file.exists(.p))
    stop("panel_tbl_condo not found in memory or cache. Run with panel_replicate=TRUE.")
  assign("panel_tbl_condo", readRDS(.p), envir = .GlobalEnv)
  message("  Loaded panel_tbl_condo from cache.")
  rm(.p)
}
if (!exists("make_rolling_year_folds", envir = .GlobalEnv))
  stop("make_rolling_year_folds() not found. Ensure 00_init.R has been sourced.")

panel_condo <- copy(panel_tbl_condo)
setDT(panel_condo)

# Re-join AV from av_history_cln with normalized (no-dash) IDs if needed
if ("appr_land_val" %in% names(panel_condo) && all(is.na(panel_condo$appr_land_val))) {
  message("  appr_land_val all-NA in panel_tbl_condo — re-joining from av_history_cln ...")
  if (!exists("av_history_cln", envir = .GlobalEnv)) {
    av_cache <- file.path(cache_dir, "av_history_cln.rds")
    if (file.exists(av_cache))
      assign("av_history_cln", readRDS(av_cache), envir = .GlobalEnv)
    else
      stop("av_history_cln not found in memory or cache. Re-run with panel_replicate=TRUE.")
  }
  av_fix <- data.table::as.data.table(av_history_cln)
  av_fix[, parcel_id := gsub("-", "", parcel_id)]
  # Condo parcel_ids are in "MAJOR-MINOR" dash format; strip dashes so the
  # join key matches av_fix, then restore after.
  panel_condo[, parcel_id := gsub("-", "", parcel_id)]
  av_fix <- av_fix[parcel_id %in% unique(panel_condo$parcel_id),
                   .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
  panel_condo[av_fix, on = .(parcel_id, tax_yr),
              `:=`(appr_land_val = i.appr_land_val,
                   appr_imps_val = i.appr_imps_val)]
  rm(av_fix)
  message("    AV re-join complete.")
}
if ("log_appr_land_val" %in% names(panel_condo) && all(is.na(panel_condo$log_appr_land_val))) {
  panel_condo[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
  panel_condo[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]
  panel_condo[, total_assessed := fifelse(is.na(appr_land_val), 0, appr_land_val) +
                                   fifelse(is.na(appr_imps_val), 0, appr_imps_val)]
  panel_condo[total_assessed > 0, log_total_assessed := log(total_assessed)]
  setorderv(panel_condo, c("parcel_id", "tax_yr"))
  panel_condo[, log_appr_land_val_lag1 := shift(log_appr_land_val, 1L), by = parcel_id]
  panel_condo[, log_appr_land_val_lag2 := shift(log_appr_land_val, 2L), by = parcel_id]
  panel_condo[, log_appr_imps_val_lag1 := shift(log_appr_imps_val, 1L), by = parcel_id]
  panel_condo[, delta_log_land := log_appr_land_val - log_appr_land_val_lag1]
  message("  Computed log AV + lag columns for condo panel")
}


# =============================================================================
# FEATURE SETS
# =============================================================================

# Parcel / location features — drive the land value component of each unit
parcel_features_condo <- c(
  # Location
  "area", "current_zoning_3",
  "dist_to_lightrail_km", "dist_to_public_km", "dist_to_private_km",
  # Lot characteristics (complex-level, prorated to unit)
  "sq_ft_lot", "access", "pcnt_unusable", "lot_depth_factor",
  "water_system", "sewer_system", "street_surface", "topography",
  # Waterfront
  "is_waterfront", "waterfront_footage", "waterfront_location",
  # Adjacent amenities
  "adjacent_golf_fairway", "adjacent_greenbelt",
  # Nuisances
  "nuisance_score", "airport_noise",
  "traffic_noise", "power_lines", "other_nuisances",
  # Hazards
  "seismic_hazard", "landslide_hazard", "steep_slope_hazard",
  "erosion_hazard", "critical_drainage", "hundred_yr_flood_plain",
  "stream", "wetland", "species_of_concern", "ngpe",
  # Permit activity
  "permits_last_1yr", "permits_last_3yr",
  "any_newconst", "years_since_newconst"
)

# Complex-level features — the complex drives land allocation value
complex_features_condo <- c(
  # Scale
  "nbr_units", "log_nbr_units", "nbr_stories_cplx", "nbr_bldgs",
  "avg_unit_size", "log_avg_unit_size",
  # Quality / age — higher quality = higher land residual
  "bldg_quality_cplx", "constr_class_cplx", "condition_cplx",
  "yr_built_cplx", "eff_yr_cplx", "age_cplx", "eff_age_cplx",
  # View / appeal (directly influence land premium)
  "pcnt_with_view", "complex_has_view", "project_location", "project_appeal",
  "appeal_location_score",
  # Amenities
  "has_elevators", "has_security", "has_fireplace", "laundry_type",
  "amenity_score",
  # Flags
  "is_mfte", "is_apt_conversion", "is_highrise", "is_large_complex",
  # Segment
  "complex_type", "condo_land_type"
)

# Unit features that affect land allocation
unit_features_land <- c(
  "unit_floor",        # higher floors → higher land share in some markets
  "is_penthouse",      # penthouse commands land premium
  "view_utilization"   # whether unit captures complex's view premium
)

# NWMLS condo market signal
NWMLS_CONDO_COLS <- c(
  "nwmls_condo_med_price",        "nwmls_condo_psf",
  "nwmls_condo_active_list",      "nwmls_condo_dom",
  "nwmls_condo_med_price_lag1",   "nwmls_condo_med_price_lag2",
  "nwmls_condo_psf_lag1",         "nwmls_condo_psf_lag2",
  "nwmls_condo_active_list_lag1", "nwmls_condo_active_list_lag2",
  "nwmls_condo_med_price_delta",  "nwmls_condo_psf_delta",
  "nwmls_condo_active_list_delta"
)
market_features_condo <- intersect(NWMLS_CONDO_COLS, names(panel_condo))

# Economic macro features — same set as residential
ECON_COLS <- c(
  "econ_employment_thous_yoy_lag1",
  "econ_services_providing_yoy_lag1",
  "econ_population_thous_yoy_lag1",
  "econ_wholesale_and_retail_trade_yoy_lag1",
  "econ_housing_permits_thous_yoy_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_yoy_lag1",
  # "econ_seattle_msa_cpi_u_1982_1984_100_yoy_lag1",  # excluded: pessimistic CPI > baseline inverts scenario ordering
  "econ_employment_thous_lvl_lag1",
  "econ_services_providing_lvl_lag1",
  "econ_population_thous_lvl_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_lvl_lag1"
  # "econ_seattle_msa_cpi_u_1982_1984_100_lvl_lag1"   # excluded: pessimistic CPI > baseline inverts scenario ordering
)
econ_features_condo <- intersect(ECON_COLS, names(panel_condo))
message("  Econ features available: ", length(econ_features_condo))


# Lag features (delta model only)
# lag1 is the reconstruction anchor (not a feature) when predicting delta;
# lag2 retained as a trend feature.
lag_features <- c("log_appr_land_val_lag2")

# Outcomes
OUTCOME_DELTA <- "delta_log_land"
OUTCOME_LEVEL <- "log_appr_land_val"

# =============================================================================
# HELPERS
# =============================================================================

encode_factors <- function(dt, cols) {
  for (col in cols) {
    if (is.character(dt[[col]]) || is.factor(dt[[col]]))
      dt[, (col) := as.integer(as.factor(get(col)))]
  }
  dt
}

build_x_matrix <- function(data_dt, x_cols) {
  missing <- setdiff(x_cols, names(data_dt))
  if (length(missing) > 0)
    message("    Missing features (set to NA): ", paste(missing, collapse = ", "))
  for (col in missing) data_dt[, (col) := NA_real_]
  data_enc <- encode_factors(copy(data_dt[, ..x_cols]), x_cols)
  x_mat    <- as.matrix(data_enc)
  x_mat[!is.finite(x_mat)] <- NA_real_
  x_mat
}

make_lgb_dataset <- function(x_mat, y_vec) {
  lgb.Dataset(data = x_mat, label = y_vec, free_raw_data = FALSE)
}

train_lgb_rolling <- function(dtrain, params, tax_yr_vec,
                               nrounds = 600L, n_folds = 5L,
                               early_stop = 50L) {
  folds <- make_rolling_year_folds(tax_yr_vec, n_folds = n_folds)

  if (is.null(folds)) {
    message("    ⚠  Rolling folds unavailable — falling back to random ", n_folds, "-fold CV")
    cv <- lgb.cv(params = params, data = dtrain, nrounds = nrounds,
                 nfold = n_folds, early_stopping_rounds = early_stop,
                 verbose = -1)
  } else {
    message("    Rolling CV: ", length(folds), " chronological folds")
    cv <- lgb.cv(params = params, data = dtrain, nrounds = nrounds,
                 folds = folds, early_stopping_rounds = early_stop,
                 verbose = -1)
  }

  best_iter <- cv$best_iter
  model     <- lgb.train(params = params, data = dtrain,
                          nrounds = best_iter, verbose = -1)
  list(model = model, best_iter = best_iter, cv = cv,
       fold_type = if (is.null(folds)) "random" else "rolling")
}

# =============================================================================
# BASE DATA FILTER
# =============================================================================

# model_condo flag may be absent from panels built before 02_transfrm_condo.R
# was updated; create it as all-1 if missing so the filter is a no-op.
if (!"model_condo" %in% names(panel_condo))
  panel_condo[, model_condo := 1L]

model_base <- panel_condo[
  !is.na(log_appr_land_val) &
    tax_yr >= 2005 &
    (is.na(model_condo) | model_condo == 1L) &
    (is.na(complex_type) | complex_type != 3L)   # exclude timeshares
]
message("  Condo LAND model — base rows: ", nrow(model_base))

# LightGBM params — condo dataset is larger; slightly lower min_data_in_leaf
params_delta <- list(
  objective        = "regression",
  metric           = "rmse",
  learning_rate    = 0.05,
  num_leaves       = 63L,
  min_data_in_leaf = 15L,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq     = 5L,
  lambda_l1        = 0.1,
  lambda_l2        = 0.1,
  verbose          = -1
)

params_level <- modifyList(params_delta, list(
  num_leaves       = 95L,
  min_data_in_leaf = 10L,
  lambda_l1        = 0.05,
  lambda_l2        = 0.05
))

# =============================================================================
# DELTA MODEL  (unit land value YoY — primary)
# =============================================================================
message("\n  --- Condo LAND DELTA model ---")

delta_x_cols <- unique(c(
  parcel_features_condo, complex_features_condo, unit_features_land,
  market_features_condo,
  grep("_yoy_", econ_features_condo, value = TRUE),
  lag_features
))
delta_x_cols <- intersect(delta_x_cols, names(model_base))

delta_data <- model_base[
  !is.na(log_appr_land_val_lag1) & !is.na(get(OUTCOME_DELTA)) & is.finite(get(OUTCOME_DELTA))
]
message("    Rows (lag present): ", nrow(delta_data))

x_delta  <- build_x_matrix(copy(delta_data), delta_x_cols)
y_delta  <- delta_data[[OUTCOME_DELTA]]
# LightGBM handles NA features natively; only filter on non-finite outcome
fin_rows <- is.finite(y_delta)
x_delta  <- x_delta[fin_rows, ]
y_delta  <- y_delta[fin_rows]
yr_delta <- delta_data$tax_yr[fin_rows]

dtrain_delta <- make_lgb_dataset(x_delta, y_delta)

message("    Training delta CV (rolling folds) ...")
cv_delta <- train_lgb_rolling(dtrain_delta, params_delta, yr_delta,
                               nrounds = 600L, n_folds = 5L, early_stop = 50L)
message("    Best iter: ", cv_delta$best_iter,
        "  [", cv_delta$fold_type, " CV]")

lgb_condo_land_delta_cv <- list(
  model     = cv_delta$model,
  x_cols    = delta_x_cols,
  params    = params_delta,
  best_iter = cv_delta$best_iter,
  fold_type = cv_delta$fold_type
)

delta_cv_path <- file.path(model_dir,
  paste0("lgb_condo_land_delta_cv_", stamp, ".rds"))
delta_dv_path <- file.path(model_dir,
  paste0("dv_condo_land_delta_",     stamp, ".rds"))
saveRDS(lgb_condo_land_delta_cv, delta_cv_path)
saveRDS(delta_x_cols,             delta_dv_path)
saveRDS(delta_data, file.path(cache_dir, "model_data_condo_land_delta.rds"))
message("    ✅ Saved: ", basename(delta_cv_path))

imp <- lgb.importance(cv_delta$model)
message("    Top 10 delta features:")
print(head(imp[, .(Feature, Gain)], 10))

# =============================================================================
# LEVEL MODEL  (unit land value direct — fallback)
# =============================================================================
message("\n  --- Condo LAND LEVEL model ---")

level_x_cols <- unique(c(
  parcel_features_condo, complex_features_condo, unit_features_land,
  market_features_condo,
  grep("_lvl_", econ_features_condo, value = TRUE)
))
level_x_cols <- intersect(level_x_cols, names(model_base))

level_data <- model_base[!is.na(get(OUTCOME_LEVEL))]
message("    Rows: ", nrow(level_data))

x_level  <- build_x_matrix(copy(level_data), level_x_cols)
y_level  <- level_data[[OUTCOME_LEVEL]]
# LightGBM handles NA features natively; only filter on non-finite outcome
fin_rows2 <- is.finite(y_level)
x_level  <- x_level[fin_rows2, ]
y_level  <- y_level[fin_rows2]
yr_level <- level_data$tax_yr[fin_rows2]

dtrain_level <- make_lgb_dataset(x_level, y_level)

message("    Training level CV (rolling folds) ...")
cv_level <- train_lgb_rolling(dtrain_level, params_level, yr_level,
                               nrounds = 600L, n_folds = 5L, early_stop = 50L)
message("    Best iter: ", cv_level$best_iter,
        "  [", cv_level$fold_type, " CV]")

lgb_condo_land_level_cv <- list(
  model     = cv_level$model,
  x_cols    = level_x_cols,
  params    = params_level,
  best_iter = cv_level$best_iter,
  fold_type = cv_level$fold_type
)

level_cv_path <- file.path(model_dir,
  paste0("lgb_condo_land_level_cv_", stamp, ".rds"))
level_dv_path <- file.path(model_dir,
  paste0("dv_condo_land_level_",     stamp, ".rds"))
saveRDS(lgb_condo_land_level_cv, level_cv_path)
saveRDS(level_x_cols,             level_dv_path)
saveRDS(level_data, file.path(cache_dir, "model_data_condo_land_level.rds"))
message("    ✅ Saved: ", basename(level_cv_path))


# =============================================================================
# RMSE SUMMARY + GROUPED FEATURE IMPORTANCE
# =============================================================================

# RMSE
delta_preds <- predict(lgb_condo_land_delta_cv$model, x_delta)
delta_rmse  <- sqrt(mean((delta_preds - y_delta)^2, na.rm = TRUE))
level_preds <- predict(lgb_condo_land_level_cv$model, x_level)
level_rmse  <- sqrt(mean((level_preds - y_level)^2, na.rm = TRUE))
message("\nCondo LAND model RMSE summary:")
# delta_rmse is now in log-change space (like residential)
reconstructed_rmse <- sqrt(mean(((delta_preds + delta_data$log_appr_land_val_lag1) - delta_data$log_appr_land_val)^2, na.rm=TRUE))
message("  Delta CV RMSE (log change)      : ", round(delta_rmse, 4))
message("  Delta CV RMSE (reconstructed)   : ", round(reconstructed_rmse, 4))
message("  Level CV RMSE : ", round(level_rmse, 4))

# Grouped importance
classify_feature <- function(feat) {
  dplyr::case_when(
    grepl("^nwmls_condo", feat) ~ "nwmls",
    grepl("^econ_",       feat) ~ "econ",
    TRUE                        ~ "parcel"
  )
}
for (which_model in c("delta", "level")) {
  cv_obj <- if (which_model == "delta") lgb_condo_land_delta_cv else lgb_condo_land_level_cv
  imp <- lgb.importance(cv_obj$model)
  if (nrow(imp) == 0) next
  imp[, group := classify_feature(Feature)]
  grp <- imp[, .(total_gain = sum(Gain), n_features = .N, avg_gain = mean(Gain)),
             by = group][order(-total_gain)]
  message("\n", toupper(which_model), " model — feature gain by group:")
  print(tibble::as_tibble(grp))
  top5 <- imp[, .SD[order(-Gain)][seq_len(min(5L, .N))], by = group]
  message("\n", toupper(which_model), " model — top features by group:")
  print(tibble::as_tibble(top5[, .(group, feature = Feature, gain = Gain)]))
}

# =============================================================================
# EXPOSE TO GLOBALENV
# =============================================================================
assign("lgb_condo_land_delta_cv",        lgb_condo_land_delta_cv, envir = .GlobalEnv)
assign("dv_condo_land_delta",            delta_x_cols,            envir = .GlobalEnv)
assign("lgb_condo_land_level_cv",        lgb_condo_land_level_cv, envir = .GlobalEnv)
assign("dv_condo_land_level",            level_x_cols,            envir = .GlobalEnv)
assign("model_data_condo_land_delta",    delta_data,              envir = .GlobalEnv)
assign("model_data_condo_land_level",    level_data,              envir = .GlobalEnv)

message("\n03_model_condo_land.R loaded — condo land models trained.")
