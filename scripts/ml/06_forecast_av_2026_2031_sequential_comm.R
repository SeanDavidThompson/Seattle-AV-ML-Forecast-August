# =============================================================================
# 06_forecast_av_2026_2031_sequential_comm.R
# Year-by-year sequential AV forecast for COMMERCIAL parcels, 2026-2031.
#
# Logic (per year t):
#   1. Pull year-t rows from extended panel
#   2. Fill lag columns from t-1 predictions (or historical for t == first year)
#   3. Score LAND with lgb_com_land_delta_cv (delta), fall back to level
#   4. Score IMPR with lgb_com_impr_delta_cv (delta), fall back to level
#   5. Back-transform; store predictions for t+1 lag inputs
#
# Writes:
#   cache/panel_tbl_2006_2031_forecasted_<scenario>_com.rds
#   outputs/parcel_year_panel_2006_2031_forecasted_<scenario>_com.parquet
# =============================================================================

scenario   <- get("scenario",   envir = .GlobalEnv)
cache_dir  <- get("cache_dir",  envir = .GlobalEnv)
output_dir <- get("output_dir", envir = .GlobalEnv)

# ---- Load extended panel ----------------------------------------------------
ext_name  <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_com")
ext_cache <- file.path(cache_dir, paste0(ext_name, ".rds"))

if (exists(ext_name, envir = .GlobalEnv)) {
  panel_ext <- data.table::copy(get(ext_name, envir = .GlobalEnv))
} else if (file.exists(ext_cache)) {
  panel_ext <- readRDS(ext_cache)
  message("  Loaded commercial extended panel from cache.")
} else {
  stop("Extended commercial panel not found. Run 05_extend_panel_2026_2031_comm.R first.")
}

setDT(panel_ext)
setkeyv(panel_ext, c("parcel_id", "tax_yr"))

# ---- Ensure commercial geo area is present (actuals-anchor join key) --------
# The commercial panels are built from CommBldg tables, which carry no Area
# column — so the actuals anchor (joining report geo areas on `area`) matched
# zero parcels.  Join Area from EXTR_Parcel here, dash-format aware.
if (!"area" %in% names(panel_ext) ||
    all(is.na(suppressWarnings(as.integer(as.character(panel_ext$area)))))) {
  .pp <- here::here("data", "kca",
                    get("kca_date_data_extracted", envir = .GlobalEnv),
                    "EXTR_Parcel.csv")
  if (file.exists(.pp)) {
    message("  geo area missing/all-NA in commercial panel — joining Area from EXTR_Parcel ...")
    a_lkp <- data.table::fread(.pp, select = c("Major", "Minor", "Area"))
    a_lkp[, major := stringr::str_pad(trimws(as.character(Major)), 6, "left", "0")]
    a_lkp[, minor := stringr::str_pad(trimws(as.character(Minor)), 4, "left", "0")]
    .use_dash <- any(grepl("-", head(panel_ext$parcel_id, 10)))
    a_lkp[, parcel_id := if (.use_dash) paste0(major, "-", minor)
                         else           paste0(major, minor)]
    a_lkp <- unique(
      a_lkp[, .(parcel_id,
                area_extr = suppressWarnings(as.integer(Area)))],
      by = "parcel_id")
    if ("area" %in% names(panel_ext)) panel_ext[, area := NULL]
    panel_ext[a_lkp, on = "parcel_id", area := i.area_extr]
    message("  geo area joined: ",
            scales::comma(sum(!is.na(panel_ext$area))), " of ",
            scales::comma(nrow(panel_ext)), " rows non-NA | areas: ",
            paste(sort(unique(stats::na.omit(panel_ext$area))), collapse = ", "))
    rm(a_lkp)
  } else {
    message("  \u26a0\ufe0f  EXTR_Parcel.csv not found — actuals anchor will not fire")
  }
}

# ---- Ensure spec_area is present (specialty-vs-geographic rate routing) -----
# The KCA geographic district reports ("Geo Area" tables) are computed on the
# NON-specialty population: their published totals reconcile to parcels with
# spec_area == 0, not to every parcel in the area.  Specialty parcels (Major
# Office 280, Major Retail 250, Warehouses 500, Hotels 160, ...) are valued by
# a separate specialty appraiser and reported separately.  The routing below
# needs spec_area to keep those two populations apart.
if (!"spec_area" %in% names(panel_ext) ||
    all(is.na(suppressWarnings(as.integer(as.character(panel_ext$spec_area)))))) {
  .pp2 <- here::here("data", "kca",
                     get("kca_date_data_extracted", envir = .GlobalEnv),
                     "EXTR_Parcel.csv")
  if (file.exists(.pp2)) {
    message("  spec_area missing/all-NA in commercial panel - joining SpecArea from EXTR_Parcel ...")
    s_lkp <- data.table::fread(.pp2, select = c("Major", "Minor", "SpecArea"))
    s_lkp[, major := stringr::str_pad(trimws(as.character(Major)), 6, "left", "0")]
    s_lkp[, minor := stringr::str_pad(trimws(as.character(Minor)), 4, "left", "0")]
    .use_dash2 <- any(grepl("-", head(panel_ext$parcel_id, 10)))
    s_lkp[, parcel_id := if (.use_dash2) paste0(major, "-", minor)
                         else            paste0(major, minor)]
    s_lkp <- unique(
      s_lkp[, .(parcel_id,
                spec_area_extr = suppressWarnings(as.integer(SpecArea)))],
      by = "parcel_id")
    if ("spec_area" %in% names(panel_ext)) panel_ext[, spec_area := NULL]
    panel_ext[s_lkp, on = "parcel_id", spec_area := i.spec_area_extr]
    message("  spec_area joined: ",
            scales::comma(sum(!is.na(panel_ext$spec_area) & panel_ext$spec_area > 0)),
            " specialty rows of ", scales::comma(nrow(panel_ext)),
            " | specialty areas present: ",
            paste(sort(unique(panel_ext[!is.na(spec_area) & spec_area > 0, spec_area])),
                  collapse = ", "))
    rm(s_lkp)
  } else {
    warning("EXTR_Parcel.csv not found - spec_area unavailable. Geographic ",
            "actuals cannot be restricted to the non-specialty population.",
            call. = FALSE)
    panel_ext[, spec_area := NA_integer_]
  }
}

# ---- Load models (support both new split names and legacy aliases) ----------
resolve_cv <- function(...) {
  # Return first cv object found among the supplied name candidates
  for (nm in c(...)) {
    if (exists(nm, envir = .GlobalEnv))
      return(get(nm, envir = .GlobalEnv))
  }
  NULL
}

land_delta_cv <- resolve_cv("lgb_com_land_delta_cv", "lgb_com_delta_cv")
land_level_cv <- resolve_cv("lgb_com_land_level_cv", "lgb_com_level_cv")
impr_delta_cv <- resolve_cv("lgb_com_impr_delta_cv")
impr_level_cv <- resolve_cv("lgb_com_impr_level_cv")

stopifnot(!is.null(land_delta_cv) || !is.null(land_level_cv))

has_land_delta <- !is.null(land_delta_cv)
has_land_level <- !is.null(land_level_cv)
has_impr_delta <- !is.null(impr_delta_cv)
has_impr_level <- !is.null(impr_level_cv)
has_impr       <- has_impr_delta || has_impr_level

# ---- Helper: safe predict ---------------------------------------------------
safe_predict <- function(cv_obj, data_dt) {
  x_cols  <- cv_obj$x_cols
  model   <- cv_obj$model
  dt      <- data.table::copy(data_dt)

  # Add any missing feature columns as NA
  for (col in setdiff(x_cols, names(dt)))
    dt[, (col) := NA_real_]

  # Encode character/factor columns to integer
  for (col in x_cols) {
    v <- dt[[col]]
    if (is.character(v) || is.factor(v))
      dt[, (col) := as.integer(as.factor(v))]
  }

  x_mat <- as.matrix(dt[, x_cols, with = FALSE])
  x_mat[!is.finite(x_mat)] <- NA_real_
  predict(model, x_mat)
}

# ---- Determine forecast years -----------------------------------------------
# Per-year cap on model-predicted log-deltas.  Training data contains
# redevelopment jumps (e.g. placeholder $1K improvements -> $50M), so
# uncapped delta predictions compound explosively over the sequential
# horizon (observed: medical subgroup reaching $264T by 2031).  log(1.5)
# bounds any single year to [-33%, +50%]; override via forecast_dlog_cap.
DLOG_CAP <- get0("forecast_dlog_cap", envir = .GlobalEnv,
                 ifnotfound = log(1.5))
message("  delta cap: predicted per-year dlog clamped to +/- ",
        round(DLOG_CAP, 3))

.fcast_start <- get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2026L)
last_obs_yr <- as.integer(.fcast_start) - 1L
fcst_years  <- seq(as.integer(.fcast_start),
                   get0("forecast_end", envir = .GlobalEnv, ifnotfound = 2031L))
fcst_years  <- fcst_years[fcst_years %in% unique(panel_ext$tax_yr)]

message("  Forecasting commercial AV for years: ",
        paste(fcst_years, collapse = ", "))

# ---- Initialise lag tracker from last observed year -------------------------
prev_preds <- panel_ext[tax_yr == last_obs_yr,
                         .(parcel_id,
                           pred_log_land = log_appr_land_val,
                           pred_log_imps = log_appr_imps_val,
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

  # Reset prediction columns for this year — guarantees the anchor / delta /
  # level precedence below is driven only by what this pass fills in (stale
  # values from a previously-forecasted cached panel would otherwise block
  # the is.na() gating).
  yr_data[, (pred_cols) := NA_real_]

  # -- Fill lag columns from previous year predictions -----------------------
  lag_dt <- prev_preds[, .(
    parcel_id,
    log_appr_land_val_lag1 = pred_log_land,
    log_appr_imps_val_lag1 = pred_log_imps
  )]
  # Drop stale lag columns before merge (only those present — avoids the
  # "column does not exist to remove" warning spam, 1 per subgroup-year)
  .stale <- intersect(c("log_appr_land_val_lag1", "log_appr_imps_val_lag1"),
                      names(yr_data))
  if (length(.stale)) yr_data[, (.stale) := NULL]
  yr_data <- merge(yr_data, lag_dt, by = "parcel_id", all.x = TRUE)

  # lag2 for land: pull from panel_ext year t-2 predictions if available
  if (yr > last_obs_yr + 1L) {
    lag2_yr <- yr - 2L
    lag2_dt <- panel_ext[tax_yr == lag2_yr,
                          .(parcel_id,
                            log_appr_land_val_lag2 = pred_log_land_val)]
    if ("log_appr_land_val_lag2" %in% names(yr_data))
      yr_data[, "log_appr_land_val_lag2" := NULL]
    yr_data <- merge(yr_data, lag2_dt, by = "parcel_id", all.x = TRUE)
  }

  # ---- ACTUALS ANCHOR: reported growth instead of ML ----------------------
  # Two distinct sources of reported growth, applied in precedence order:
  #
  #   1. SPECIALTY reports (280 Major Office, 250 Major Retail, 500 Warehouses,
  #      160 Hotels, ...).  Countywide populations valued by a specialty
  #      appraiser.  These are the correct source for any parcel with a
  #      spec_area.
  #   2. GEOGRAPHIC district reports (Central/North/South "Geo Area" tables).
  #      These are computed on the NON-specialty population - the published
  #      Area 30 total reconciles to parcels with spec_area == 0 - so they must
  #      NOT be applied to specialty parcels.
  #
  # A specialty parcel whose specialty report has not been published yet does
  # NOT fall through to the geographic rate.  It is left unanchored and handled
  # by `specialty_actuals_policy`.  Silently inheriting the geographic rate is
  # the defect this block exists to prevent: in the 2026 cycle it applied Area
  # 30's -31.04% (a non-specialty CBD figure) to every downtown major office
  # parcel.
  yr_data[, dlog_actual := NA_real_]
  yr_data[, rate_source := NA_character_]

  if (!"spec_area" %in% names(yr_data)) {
    warning("spec_area absent from the commercial panel for year ", yr,
            " - geographic actuals cannot be restricted to the non-specialty ",
            "population.", call. = FALSE)
    yr_data[, spec_area := NA_integer_]
  }
  yr_data[, is_specialty := !is.na(spec_area) & spec_area > 0L]

  .geo_scope <- if (exists("geo_actuals_scope", envir = .GlobalEnv))
    get("geo_actuals_scope", envir = .GlobalEnv) else "nonspecialty"
  .spec_pol  <- if (exists("specialty_actuals_policy", envir = .GlobalEnv))
    get("specialty_actuals_policy", envir = .GlobalEnv) else "model"

  if (exists("area_report_actuals", envir = .GlobalEnv)) {
    act_raw <- data.table::as.data.table(
      get("area_report_actuals", envir = .GlobalEnv))
    if (!"report_kind" %in% names(act_raw)) act_raw[, report_kind := "geo"]
    if (!"spec_area"   %in% names(act_raw)) act_raw[, spec_area   := NA_integer_]

    # Reports for assessment year A describe the A -> A+1 tax-roll change
    # (1/1/A revalue posts to the A+1 tax roll), so they anchor tax_yr A+1.
    act_yr <- act_raw[basis == "population" & assessment_yr + 1L == yr]

    # ---- 1. Specialty rates -------------------------------------------------
    spec_act <- act_yr[report_kind == "specialty" & !is.na(spec_area),
                       .(spec_area_join = as.integer(spec_area),
                         dlog_spec      = log1p(pct_change))]
    spec_act <- unique(spec_act[is.finite(dlog_spec)], by = "spec_area_join")
    if (nrow(spec_act) > 0) {
      yr_data[spec_act, on = .(spec_area = spec_area_join),
              dlog_actual := i.dlog_spec]
      yr_data[!is.na(dlog_actual), rate_source := "specialty_report"]
    }

    # ---- 2. Geographic rates - non-specialty parcels only -------------------
    geo_act <- act_yr[report_kind == "geo" & prop_type == "com",
                      .(area_join = suppressWarnings(as.integer(area)),
                        dlog_geo  = log1p(pct_change))]
    geo_act <- unique(geo_act[!is.na(area_join) & is.finite(dlog_geo)],
                      by = "area_join")

    if (nrow(geo_act) > 0 && "area" %in% names(yr_data)) {
      yr_data[, area_join_tmp :=
                suppressWarnings(as.integer(as.character(area)))]
      yr_data[, dlog_geo_tmp := NA_real_]
      yr_data[geo_act, on = .(area_join_tmp = area_join),
              dlog_geo_tmp := i.dlog_geo]

      elig_geo <- is.na(yr_data$dlog_actual) & !is.na(yr_data$dlog_geo_tmp) &
        (.geo_scope == "all" | !yr_data$is_specialty)
      if (any(elig_geo))
        yr_data[elig_geo, `:=`(dlog_actual = dlog_geo_tmp,
                               rate_source = "geo_report")]
      yr_data[, c("area_join_tmp", "dlog_geo_tmp") := NULL]
    }

    # ---- 3. Specialty parcels with no published rate ------------------------
    unanchored <- yr_data$is_specialty & is.na(yr_data$dlog_actual)
    if (any(unanchored)) {
      if (.spec_pol == "hold") {
        yr_data[unanchored, `:=`(dlog_actual = 0,
                                 rate_source = "specialty_hold_flat")]
      } else if (.spec_pol == "prior") {
        prior_act <- act_raw[report_kind == "specialty" &
                               basis == "population" &
                               !is.na(spec_area) &
                               assessment_yr + 1L < yr]
        if (nrow(prior_act) > 0) {
          data.table::setorder(prior_act, spec_area, -assessment_yr)
          prior_act <- unique(prior_act, by = "spec_area")[
            , .(spec_area_join = as.integer(spec_area),
                dlog_prior     = log1p(pct_change))]
          yr_data[, dlog_prior_tmp := NA_real_]
          yr_data[prior_act, on = .(spec_area = spec_area_join),
                  dlog_prior_tmp := i.dlog_prior]
          use_prior <- unanchored & !is.na(yr_data$dlog_prior_tmp)
          if (any(use_prior))
            yr_data[use_prior, `:=`(dlog_actual = dlog_prior_tmp,
                                    rate_source = "specialty_prior_year")]
          yr_data[, dlog_prior_tmp := NULL]
        }
      }
      # .spec_pol == "model": leave NA - the ML / CoStar blocks below fill it.
    }

    # ---- 4. Apply the anchor ------------------------------------------------
    anchor_land <- !is.na(yr_data$dlog_actual) &
                   !is.na(yr_data$log_appr_land_val_lag1)
    if (any(anchor_land))
      yr_data[anchor_land,
              pred_log_land_val := log_appr_land_val_lag1 + dlog_actual]

    anchor_impr <- !is.na(yr_data$dlog_actual) &
                   !is.na(yr_data$log_appr_imps_val_lag1)
    if (any(anchor_impr))
      yr_data[anchor_impr,
              pred_log_imps_val := log_appr_imps_val_lag1 + dlog_actual]

    # ---- 5. Guardrail -------------------------------------------------------
    # A specialty parcel must never carry a geographic rate unless explicitly
    # asked for via geo_actuals_scope = "all".
    n_bad <- sum(yr_data$is_specialty &
                   yr_data$rate_source %in% "geo_report", na.rm = TRUE)
    if (n_bad > 0 && .geo_scope != "all")
      stop("Rate routing failure in year ", yr, ": ", n_bad,
           " specialty parcels were assigned a geographic area rate. ",
           "The geographic reports exclude specialty parcels; applying them ",
           "here double-counts a decline measured with these parcels removed.")

    # ---- 6. Coverage report -------------------------------------------------
    yr_data[, av_lag_tmp :=
              data.table::fifelse(is.na(log_appr_land_val_lag1), 0,
                                  exp(log_appr_land_val_lag1)) +
              data.table::fifelse(is.na(log_appr_imps_val_lag1), 0,
                                  exp(log_appr_imps_val_lag1))]
    cov <- yr_data[, .(parcels  = .N,
                       av_prior = sum(av_lag_tmp, na.rm = TRUE)),
                   by = .(is_specialty,
                          rate_source = data.table::fifelse(
                            is.na(rate_source), "model", rate_source))]
    cov[, tax_yr := yr]
    cov[, subgroup := if (exists("com_subgroup_key", envir = .GlobalEnv))
                        get("com_subgroup_key", envir = .GlobalEnv)
                      else NA_character_]
    data.table::setorder(cov, -av_prior)
    for (i in seq_len(nrow(cov)))
      message(sprintf("    rate source %-22s | specialty=%-5s | %9s parcels | prior AV $%.2fB",
                      cov$rate_source[i], cov$is_specialty[i],
                      scales::comma(cov$parcels[i]),
                      cov$av_prior[i] / 1e9))
    prev_cov <- if (exists("actuals_rate_coverage", envir = .GlobalEnv))
      get("actuals_rate_coverage", envir = .GlobalEnv) else NULL
    assign("actuals_rate_coverage",
           data.table::rbindlist(list(prev_cov, cov), fill = TRUE),
           envir = .GlobalEnv)
    yr_data[, av_lag_tmp := NULL]
  }

  # ---- LAND: delta model (requires lag1; skips actuals-anchored rows) -------
  if (has_land_delta) {
    eligible <- !is.na(yr_data$log_appr_land_val_lag1) &
                is.na(yr_data$pred_log_land_val)
    if (any(eligible)) {
      # Delta model predicts log-change; add lag1 anchor to reconstruct log level
      # Delta model predicts log-change; clamp to +/- DLOG_CAP before adding
      # the lag1 anchor (unbounded deltas compound explosively).
      d_land <- safe_predict(land_delta_cv, yr_data[eligible])
      d_land <- pmin(pmax(d_land, -DLOG_CAP), DLOG_CAP)
      yr_data[eligible, pred_log_land_val := log_appr_land_val_lag1 + d_land]
    }
  }

  # LAND: level fallback for parcels without lag
  if (has_land_level) {
    need_level <- is.na(yr_data$pred_log_land_val)
    if (any(need_level)) {
      yr_data[need_level,
              pred_log_land_val := safe_predict(land_level_cv, .SD),
              .SDcols = names(yr_data)]
    }
  }

  # ---- IMPR: delta model (skips actuals-anchored rows) ----------------------
  if (has_impr_delta) {
    eligible_impr <- !is.na(yr_data$log_appr_imps_val_lag1) &
                     is.na(yr_data$pred_log_imps_val)
    if (any(eligible_impr)) {
      # Delta model predicts log-change; add lag1 anchor to reconstruct log level
      # Delta model predicts log-change; clamp to +/- DLOG_CAP before adding
      # the lag1 anchor (unbounded deltas compound explosively).
      d_imps <- safe_predict(impr_delta_cv, yr_data[eligible_impr])
      d_imps <- pmin(pmax(d_imps, -DLOG_CAP), DLOG_CAP)
      yr_data[eligible_impr, pred_log_imps_val := log_appr_imps_val_lag1 + d_imps]
    }
  }

  # IMPR: level fallback
  if (has_impr_level) {
    need_impr_level <- is.na(yr_data$pred_log_imps_val)
    if (any(need_impr_level)) {
      yr_data[need_impr_level,
              pred_log_imps_val := safe_predict(impr_level_cv, .SD),
              .SDcols = names(yr_data)]
    }
  }

  # IMPR: last-resort ratio heuristic if no impr model available
  if (!has_impr && has_land_delta) {
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
  yr_data[, dlog_actual := NULL]
  for (.c in intersect(c("rate_source", "is_specialty"), names(yr_data)))
    yr_data[, (.c) := NULL]
  yr_data[, pred_appr_land_val  := exp(pred_log_land_val)]
  yr_data[, pred_appr_imps_val  := exp(pred_log_imps_val)]
  yr_data[, pred_total_assessed := pred_appr_land_val + pred_appr_imps_val]
  yr_data[, pred_log_total      := log(pmax(pred_total_assessed, 1))]

  # ---- Write predictions back into full panel -------------------------------
  panel_ext[tax_yr == yr,
            (pred_cols) := yr_data[, pred_cols, with = FALSE]]

  # ---- Update lag tracker ---------------------------------------------------
  prev_preds <- yr_data[, .(parcel_id,
                              pred_log_land  = pred_log_land_val,
                              pred_log_imps  = pred_log_imps_val,
                              pred_log_total = pred_log_total)]

  message("    Year ", yr, ": ",
          sum(!is.na(yr_data$pred_appr_land_val)), " parcels scored.")
}

# ============================================================================
# SAVE OUTPUTS
# ============================================================================
setkeyv(panel_ext, c("parcel_id", "tax_yr"))

cache_out <- file.path(
  cache_dir,
  paste0("panel_tbl_2006_2031_forecasted_", scenario, "_com.rds")
)
saveRDS(panel_ext, cache_out)
message("  \U1f4be cached: ", basename(cache_out))

parquet_out <- file.path(
  output_dir,
  paste0("parcel_year_panel_2006_2031_forecasted_", scenario, "_com.parquet")
)
tryCatch({
  arrow::write_parquet(panel_ext, parquet_out)
  message("  \U1f4be parquet: ", basename(parquet_out))
}, error = function(e) {
  message("  \u26a0\ufe0f  Parquet write failed: ", e$message)
  saveRDS(panel_ext, sub("\\.parquet$", ".rds", parquet_out))
  message("  \U1f4be saved as RDS instead.")
})

assign("panel_tbl_forecasted_com", panel_ext, envir = .GlobalEnv)
message("\n06_forecast_av_2026_2031_sequential_comm.R loaded (scenario = ",
        scenario, ")")
