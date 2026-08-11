# =============================================================================
# 03_model_comm_land.R
# LightGBM land-value models for COMMERCIAL parcels.
#
# Mirrors 03_model_land.R (residential) in structure and CV method.
# Uses make_rolling_year_folds() from 00_init.R to enforce chronological
# cross-validation — the model never trains on future years to predict past.
#
# Two models trained:
#   DELTA  — predicts log(appr_land_val)  using prior-year lag as anchor.
#             Primary model; used for all parcels with a valid lag.
#   LEVEL  — predicts log(appr_land_val)  directly (no lag required).
#             Fallback for new construction and parcels missing lag history.
#
# Outputs saved to model_dir/:
#   lgb_comm_land_delta_cv_<stamp>.rds   dv_comm_land_delta_<stamp>.rds
#   lgb_comm_land_level_cv_<stamp>.rds   dv_comm_land_level_<stamp>.rds
# Training frames saved to cache_dir/:
#   model_data_comm_land_delta.rds
#   model_data_comm_land_level.rds
#
# GlobalEnv objects exposed (consumed by 06_forecast_*_comm.R):
#   lgb_com_land_delta_cv  dv_com_land_delta
#   lgb_com_land_level_cv  dv_com_land_level
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

# Load panel_tbl_com from cache if it was dropped from memory after Step 2
if (!exists("panel_tbl_com", envir = .GlobalEnv)) {
  .p <- file.path(cache_dir, "panel_tbl_com.rds")
  if (!file.exists(.p))
    stop("panel_tbl_com not found in memory or cache. Run with panel_replicate=TRUE.")
  assign("panel_tbl_com", readRDS(.p), envir = .GlobalEnv)
  message("  Loaded panel_tbl_com from cache.")
  rm(.p)
}
if (!exists("make_rolling_year_folds", envir = .GlobalEnv))
  stop("make_rolling_year_folds() not found. Ensure 00_init.R has been sourced.")

panel_com <- copy(panel_tbl_com)
setDT(panel_com)

# Re-join AV from av_history_cln with normalized (no-dash) IDs if needed
if (all(is.na(panel_com$appr_land_val))) {
  message("  appr_land_val all-NA in panel_tbl_com — re-joining from av_history_cln ...")
  if (!exists("av_history_cln", envir = .GlobalEnv)) {
    av_cache <- file.path(cache_dir, "av_history_cln.rds")
    if (file.exists(av_cache))
      assign("av_history_cln", readRDS(av_cache), envir = .GlobalEnv)
    else
      stop("av_history_cln not found in memory or cache. Re-run with panel_replicate=TRUE.")
  }
  av_fix <- data.table::as.data.table(av_history_cln)
  av_fix[, parcel_id := gsub("-", "", parcel_id)]
  av_fix <- av_fix[parcel_id %in% unique(panel_com$parcel_id),
                   .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
  panel_com[av_fix, on = .(parcel_id, tax_yr),
            `:=`(appr_land_val = i.appr_land_val,
                 appr_imps_val = i.appr_imps_val)]
  rm(av_fix)
  message("    AV re-join complete.")
}
if (all(is.na(panel_com$log_appr_land_val))) {
  panel_com[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
  panel_com[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]
  panel_com[, total_assessed := fifelse(is.na(appr_land_val), 0, appr_land_val) +
                                 fifelse(is.na(appr_imps_val), 0, appr_imps_val)]
  panel_com[total_assessed > 0, log_total_assessed := log(total_assessed)]
  setorderv(panel_com, c("parcel_id", "tax_yr"))
  panel_com[, log_appr_land_val_lag1 := shift(log_appr_land_val, 1L), by = parcel_id]
  panel_com[, log_appr_land_val_lag2 := shift(log_appr_land_val, 2L), by = parcel_id]
  panel_com[, log_appr_imps_val_lag1 := shift(log_appr_imps_val, 1L), by = parcel_id]
  panel_com[, delta_log_land := log_appr_land_val - log_appr_land_val_lag1]
  message("  Computed log AV + lag columns for commercial panel")
}


# =============================================================================
# FEATURE SETS
# =============================================================================

# Land / location features — everything that drives site value independent
# of what is built on it.
land_features_com <- c(
  # Location / zoning
  "area", "current_zoning_3",
  "dist_to_lightrail_km", "dist_to_public_km", "dist_to_private_km",
  # Lot characteristics
  "sq_ft_lot", "unbuildable", "pcnt_unusable", "lot_depth_factor",
  "access", "water_system", "sewer_system", "street_surface", "topography",
  # Waterfront
  "is_waterfront", "waterfront_footage", "waterfront_location",
  # Adjacent amenities
  "adjacent_golf_fairway", "adjacent_greenbelt",
  # Nuisances
  "nuisance_score", "airport_noise",
  "traffic_noise", "power_lines", "other_nuisances",
  # Environmental / hazard constraints on land use
  "seismic_hazard", "landslide_hazard", "steep_slope_hazard",
  "erosion_hazard", "critical_drainage", "hundred_yr_flood_plain",
  "stream", "wetland", "species_of_concern", "ngpe",
  # Land designations
  "contamination", "historic_site",
  # Views (drive land premium directly)
  "mt_rainier", "olympics", "cascades", "territorial",
  "seattle_skyline", "puget_sound", "lake_washington",
  # Permit activity (signals future redevelopment potential)
  "permits_last_1yr", "permits_last_3yr", "permits_last_5yr",
  "any_newconst", "years_since_newconst",
  "log_val_last_3yr", "log_sqft_last_3yr",
  # Property class / use context
  "prop_class_comm", "predominant_use", "n_distinct_uses"
  # tax_yr removed — economic features carry temporal signal
)

# CoStar market columns — present only when xx_costar_to_panel.R has run
COSTAR_COLS <- c(
  "costar_vr_nro",       "costar_vr_mf",       "costar_ar_mf",
  "costar_vr_nro_lag1",  "costar_vr_nro_lag2",
  "costar_vr_mf_lag1",   "costar_vr_mf_lag2",
  "costar_ar_mf_lag1",   "costar_ar_mf_lag2",
  "costar_vr_nro_delta", "costar_vr_mf_delta", "costar_ar_mf_delta"
)
market_features_com <- intersect(COSTAR_COLS, names(panel_com))

# Economic macro features — same set as residential, available after xx_econ_to_panel.R
ECON_COLS <- c(
  # YoY change (delta) versions — for delta model
  "econ_employment_thous_yoy_lag1",
  "econ_services_providing_yoy_lag1",
  "econ_population_thous_yoy_lag1",
  "econ_wholesale_and_retail_trade_yoy_lag1",
  "econ_housing_permits_thous_yoy_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_yoy_lag1",
  "econ_seattle_msa_cpi_u_1982_1984_100_yoy_lag1",
  # Level versions — for level model
  "econ_employment_thous_lvl_lag1",
  "econ_services_providing_lvl_lag1",
  "econ_population_thous_lvl_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_lvl_lag1",
  "econ_seattle_msa_cpi_u_1982_1984_100_lvl_lag1"
)
econ_features_com <- intersect(ECON_COLS, names(panel_com))
message("  Econ features available: ", length(econ_features_com))


# Lag features (delta model only)
# lag1 is the reconstruction anchor (not a feature) when predicting delta;
# lag2 retained as a trend feature.
lag_features <- c("log_appr_land_val_lag2")

# Outcomes
OUTCOME_DELTA <- "delta_log_land"
OUTCOME_LEVEL <- "log_appr_land_val"   # same target; no lag needed for level

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
  available <- intersect(x_cols, names(data_dt))
  missing   <- setdiff(x_cols, names(data_dt))
  if (length(missing) > 0)
    message("    Missing features (set to NA): ", paste(missing, collapse = ", "))
  for (col in missing) data_dt[, (col) := NA_real_]
  data_enc  <- encode_factors(copy(data_dt[, ..x_cols]), x_cols)
  x_mat     <- as.matrix(data_enc)
  x_mat[!is.finite(x_mat)] <- NA_real_
  x_mat
}

make_lgb_dataset <- function(x_mat, y_vec) {
  lgb.Dataset(data = x_mat, label = y_vec, free_raw_data = FALSE)
}

#' Train with rolling-year folds.  Falls back to random nfold if
#' make_rolling_year_folds() returns NULL (too few years of data).
train_lgb_rolling <- function(dtrain, params, tax_yr_vec,
                               nrounds = 800L, n_folds = 5L,
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

model_base <- panel_com[
  !is.na(log_appr_land_val) &
    !is.na(tax_yr) &
    (is.na(model_com) | model_com == 1L)
]
message("  Commercial LAND model — base rows: ", nrow(model_base))

# LightGBM params — land value models
params_delta <- list(
  objective        = "regression",
  metric           = "rmse",
  learning_rate    = 0.05,
  num_leaves       = 63L,
  min_data_in_leaf = 20L,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq     = 5L,
  lambda_l1        = 0.1,
  lambda_l2        = 0.1,
  verbose          = -1
)

params_level <- modifyList(params_delta, list(
  num_leaves       = 95L,
  min_data_in_leaf = 15L,
  lambda_l1        = 0.05,
  lambda_l2        = 0.05
))

# =============================================================================
# DELTA MODEL  (land value YoY — primary)
# =============================================================================
message("\n  --- Commercial LAND DELTA model ---")

delta_x_cols <- unique(c(land_features_com, market_features_com,
                          grep("_yoy_", econ_features_com, value=TRUE),
                          lag_features))
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
                               nrounds = 800L, n_folds = 5L, early_stop = 50L)
message("    Best iter: ", cv_delta$best_iter,
        "  [", cv_delta$fold_type, " CV]")

lgb_com_land_delta_cv <- list(
  model     = cv_delta$model,
  x_cols    = delta_x_cols,
  params    = params_delta,
  best_iter = cv_delta$best_iter,
  fold_type = cv_delta$fold_type
)

# Save
delta_cv_path <- file.path(model_dir,
  paste0("lgb_comm_land_delta_cv_", stamp, ".rds"))
delta_dv_path <- file.path(model_dir,
  paste0("dv_comm_land_delta_",     stamp, ".rds"))
saveRDS(lgb_com_land_delta_cv, delta_cv_path)
saveRDS(delta_x_cols,           delta_dv_path)
saveRDS(delta_data, file.path(cache_dir, "model_data_comm_land_delta.rds"))
message("    ✅ Saved: ", basename(delta_cv_path))

# Feature importance
imp <- lgb.importance(cv_delta$model)
message("    Top 10 delta features:")
print(head(imp[, .(Feature, Gain)], 10))

# =============================================================================
# LEVEL MODEL  (land value direct — fallback)
# =============================================================================
message("\n  --- Commercial LAND LEVEL model ---")

level_x_cols <- unique(c(land_features_com, market_features_com,
                          grep("_lvl_", econ_features_com, value=TRUE)))
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
                               nrounds = 800L, n_folds = 5L, early_stop = 50L)
message("    Best iter: ", cv_level$best_iter,
        "  [", cv_level$fold_type, " CV]")

lgb_com_land_level_cv <- list(
  model     = cv_level$model,
  x_cols    = level_x_cols,
  params    = params_level,
  best_iter = cv_level$best_iter,
  fold_type = cv_level$fold_type
)

level_cv_path <- file.path(model_dir,
  paste0("lgb_comm_land_level_cv_", stamp, ".rds"))
level_dv_path <- file.path(model_dir,
  paste0("dv_comm_land_level_",     stamp, ".rds"))
saveRDS(lgb_com_land_level_cv, level_cv_path)
saveRDS(level_x_cols,           level_dv_path)
saveRDS(level_data, file.path(cache_dir, "model_data_comm_land_level.rds"))
message("    ✅ Saved: ", basename(level_cv_path))

# =============================================================================
# EXPOSE TO GLOBALENV
# =============================================================================
assign("lgb_com_land_delta_cv",        lgb_com_land_delta_cv, envir = .GlobalEnv)
assign("dv_com_land_delta",            delta_x_cols,          envir = .GlobalEnv)
assign("lgb_com_land_level_cv",        lgb_com_land_level_cv, envir = .GlobalEnv)
assign("dv_com_land_level",            level_x_cols,          envir = .GlobalEnv)
assign("model_data_comm_land_delta",   delta_data,            envir = .GlobalEnv)
assign("model_data_comm_land_level",   level_data,            envir = .GlobalEnv)

# ============================================================================
# RMSE summary + grouped feature importance
# ============================================================================
delta_preds <- predict(lgb_com_land_delta_cv$model, x_delta)
delta_rmse  <- sqrt(mean((delta_preds - y_delta)^2, na.rm = TRUE))
level_preds <- predict(lgb_com_land_level_cv$model, x_level)
level_rmse  <- sqrt(mean((level_preds - y_level)^2, na.rm = TRUE))

message("\nCommercial LAND model RMSE summary:")
# delta_rmse is now in log-change space (like residential)
reconstructed_rmse <- sqrt(mean(((delta_preds + delta_data$log_appr_land_val_lag1) - delta_data$log_appr_land_val)^2, na.rm=TRUE))
message("  Delta CV RMSE (log change)      : ", round(delta_rmse, 4))
message("  Delta CV RMSE (reconstructed)   : ", round(reconstructed_rmse, 4))
message("  Level CV RMSE : ", round(level_rmse, 4))

classify_feature <- function(feat) {
  dplyr::case_when(
    grepl("^costar_", feat) ~ "costar",
    grepl("^econ_",   feat) ~ "econ",
    TRUE                    ~ "parcel"
  )
}

for (which_model in c("delta", "level")) {
  cv_obj <- if (which_model == "delta") lgb_com_land_delta_cv else lgb_com_land_level_cv
  imp <- data.table::as.data.table(lgb.importance(cv_obj$model))
  if (nrow(imp) == 0) next
  imp[, group := classify_feature(Feature)]
  grp <- imp[, .(total_gain = sum(Gain), n_features = .N, avg_gain = mean(Gain)),
             by = group][order(-total_gain)]
  message("\n", which_model, " model — feature gain by group:")
  print(tibble::as_tibble(grp))
  top5 <- imp[, .SD[order(-Gain)][seq_len(min(5L, .N))], by = group]
  message("\n", which_model, " model — top features by group:")
  print(tibble::as_tibble(top5[, .(group, feature = Feature, gain = Gain)]))
}

message("\n03_model_comm_land.R loaded — commercial land models trained.")
