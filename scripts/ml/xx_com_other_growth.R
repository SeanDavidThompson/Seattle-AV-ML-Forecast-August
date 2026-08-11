# =============================================================================
# xx_com_other_growth.R  —  Residual commercial growth (replaces flat carry-forward)
# =============================================================================
# What this replaces
# ------------------
# main_ml.R currently forecasts com_other with:
#
#     co_dt[, pred_total_assessed := zoo::na.locf(.obs_total, na.rm = FALSE),
#           by = parcel_id]
#
# i.e. last observed AV held flat for every forecast year.  That freezes a
# large slice of commercial AV at zero growth and, just as importantly, makes
# it scenario-invariant — it contributes nothing to the baseline/optimistic/
# pessimistic spread.
#
# What this does instead
# ----------------------
# Precedence per parcel-year:
#
#   1. KCA GEOGRAPHIC AREA REPORT actual, for the anchor year only, for
#      non-specialty parcels in an area with a published commercial rate.
#      Published actuals beat any modelled or blended rate.
#   2. A growth rate from `com_other_growth_method`:
#        "com_weighted" (default) — value-weighted mean dlog across the six
#             modelled subgroup forecasts for that year.  Weights are each
#             subgroup's own forecast AV in the prior year, so the residual
#             grows like commercial as a whole.
#        "blend"  — fixed subgroup weights from `com_other_blend_weights`.
#        "proxy"  — a single subgroup's rate (`com_other_proxy`).
#        "flat"   — reproduces the current carry-forward exactly.
#   3. Flat, if the chosen source is unavailable for that year (fallback).
#
# Land and improvement are grown separately and recombined; parcels with no
# improvement value stay land-only.  Rates compound off the PRIOR FORECAST
# year, not off the 2026 base.
#
# Inputs  : panel_tbl_forecasted_<key> for the six subgroups (in .GlobalEnv or
#           cache), area_report_actuals, the com_other extended input panel.
# Output  : a data.table with pred_appr_land_val / pred_appr_imps_val /
#           pred_total_assessed populated across the horizon, plus
#           co_rate_source for audit.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(here)
})

message("Running xx_com_other_growth.R ...")

# -----------------------------------------------------------------------------
# 1. Subgroup growth rates by year
# -----------------------------------------------------------------------------
# Reads each subgroup's forecast panel and computes total AV by tax_yr, then
# the year-over-year dlog.  Uses forecast AV (pred_total_assessed) where it
# exists and observed AV before the forecast boundary.
com_subgroup_growth <- function(scenario,
                                cache_dir,
                                keys = get("COM_SUBGROUP_KEYS",
                                           envir = .GlobalEnv)) {
  out <- list()
  for (key in keys) {
    nm <- paste0("panel_tbl_forecasted_", key)
    dt <- NULL
    if (exists(nm, envir = .GlobalEnv)) {
      dt <- data.table::as.data.table(get(nm, envir = .GlobalEnv))
    } else {
      p <- file.path(cache_dir,
                     paste0("panel_tbl_2006_2031_forecasted_",
                            scenario, "_", key, ".rds"))
      if (file.exists(p)) dt <- data.table::as.data.table(readRDS(p))
    }
    if (is.null(dt) || !nrow(dt)) {
      message("  \u26a0\ufe0f  no forecast panel for ", key, " — excluded from weights")
      next
    }

    dt[, .tot := NA_real_]
    if ("pred_total_assessed" %in% names(dt))
      dt[!is.na(pred_total_assessed), .tot := as.numeric(pred_total_assessed)]
    if (all(c("appr_land_val", "appr_imps_val") %in% names(dt)))
      dt[is.na(.tot),
         .tot := fifelse(is.na(appr_land_val), 0, as.numeric(appr_land_val)) +
                 fifelse(is.na(appr_imps_val), 0, as.numeric(appr_imps_val))]

    agg <- dt[!is.na(.tot) & .tot > 0,
              .(av = sum(.tot, na.rm = TRUE)), by = tax_yr]
    if (!nrow(agg)) next
    data.table::setorder(agg, tax_yr)
    agg[, subgroup := key]
    agg[, av_lag := data.table::shift(av, 1L)]
    agg[, dlog := log(av / av_lag)]
    out[[key]] <- agg[is.finite(dlog), .(subgroup, tax_yr, av, av_lag, dlog)]
    dt[, .tot := NULL]
  }
  if (!length(out)) return(data.table::data.table())
  data.table::rbindlist(out, use.names = TRUE)
}

# -----------------------------------------------------------------------------
# 2. Collapse subgroup rates into one residual rate per year
# -----------------------------------------------------------------------------
com_other_rate_by_year <- function(sg_growth,
                                   method  = "com_weighted",
                                   weights = NULL,
                                   proxy   = NULL,
                                   cap     = 0.15) {
  if (!nrow(sg_growth)) return(data.table::data.table())

  res <- switch(
    method,
    com_weighted = sg_growth[
      , .(dlog_other = stats::weighted.mean(dlog, w = av_lag, na.rm = TRUE),
          basis      = "value_weighted_com"),
      by = tax_yr],

    blend = {
      stopifnot(!is.null(weights), length(weights) > 0)
      w <- data.table::data.table(subgroup = names(weights),
                                  wt       = as.numeric(weights))
      w[, wt := wt / sum(wt)]
      j <- merge(sg_growth, w, by = "subgroup")
      miss <- setdiff(names(weights), unique(j$subgroup))
      if (length(miss))
        message("  \u26a0\ufe0f  blend weights reference missing subgroups: ",
                paste(miss, collapse = ", "), " — renormalising")
      j[, .(dlog_other = sum(dlog * wt) / sum(wt),
            basis      = "fixed_blend"), by = tax_yr]
    },

    proxy = {
      stopifnot(!is.null(proxy))
      sg_growth[subgroup == proxy,
                .(dlog_other = dlog, basis = paste0("proxy_", proxy)),
                by = tax_yr]
    },

    flat = sg_growth[, .(dlog_other = 0, basis = "flat"), by = tax_yr],

    stop("com_other_growth_method must be one of: ",
         "com_weighted, blend, proxy, flat")
  )

  res <- unique(res, by = "tax_yr")

  # Guardrail: the residual bucket should never move more than the modelled
  # segments plausibly can.  Cap and warn rather than silently shipping it.
  hot <- abs(res$dlog_other) > cap
  if (any(hot, na.rm = TRUE)) {
    message("  \u26a0\ufe0f  residual rate exceeded \u00b1", round(100 * cap, 1),
            "% in ", sum(hot, na.rm = TRUE), " year(s) — capped")
    print(res[hot])
    res[hot, dlog_other := sign(dlog_other) * cap]
    res[hot, basis := paste0(basis, "_capped")]
  }
  res[]
}

# -----------------------------------------------------------------------------
# 3. Apply to the com_other panel
# -----------------------------------------------------------------------------
forecast_com_other <- function(co_dt,
                               forecast_start,
                               forecast_end,
                               rate_tbl,
                               use_kca_actuals = TRUE,
                               geo_scope       = "nonspecialty") {

  co_dt <- data.table::copy(data.table::as.data.table(co_dt))
  data.table::setkeyv(co_dt, c("parcel_id", "tax_yr"))

  # ---- Observed land / improvement, carried to the forecast boundary --------
  for (v in c("appr_land_val", "appr_imps_val"))
    if (!v %in% names(co_dt)) co_dt[, (v) := NA_real_]

  co_dt[, `:=`(.obs_land = as.numeric(appr_land_val),
               .obs_imps = as.numeric(appr_imps_val))]
  co_dt[, `:=`(.base_land = zoo::na.locf(.obs_land, na.rm = FALSE),
               .base_imps = zoo::na.locf(.obs_imps, na.rm = FALSE)),
        by = parcel_id]

  # ---- Total-only fallback --------------------------------------------------
  # The com_other extend panel does not always carry appr_land_val /
  # appr_imps_val: Step 4b runs the AV re-join only for the six subgroups, so
  # com_other can reach here with the split columns absent or all-NA.  Without
  # this fallback every parcel produces NA and the whole residual bucket
  # forecasts to $0 - which is exactly what the pre-2026-08-02 carry-forward
  # guarded against with its own total_assessed fallback.
  co_dt[, .obs_total := NA_real_]
  co_dt[!is.na(.obs_land) | !is.na(.obs_imps),
        .obs_total := fifelse(is.na(.obs_land), 0, .obs_land) +
                      fifelse(is.na(.obs_imps), 0, .obs_imps)]
  if ("total_assessed" %in% names(co_dt))
    co_dt[is.na(.obs_total) & !is.na(total_assessed) & total_assessed > 0,
          .obs_total := as.numeric(total_assessed)]
  if ("log_total_assessed" %in% names(co_dt))
    co_dt[is.na(.obs_total) & is.finite(log_total_assessed),
          .obs_total := exp(as.numeric(log_total_assessed))]
  co_dt[, .base_total := zoo::na.locf(.obs_total, na.rm = FALSE),
        by = parcel_id]

  # TRUE when land/improvement are available and can be grown separately;
  # FALSE means we only have a total and must grow that.
  co_dt[, .split_ok := !is.na(.base_land) | !is.na(.base_imps)]
  n_split <- co_dt[tax_yr >= forecast_start & .split_ok,
                   data.table::uniqueN(parcel_id)]
  n_tot   <- co_dt[tax_yr >= forecast_start & !.split_ok & !is.na(.base_total),
                   data.table::uniqueN(parcel_id)]
  n_none  <- co_dt[tax_yr >= forecast_start & !.split_ok & is.na(.base_total),
                   data.table::uniqueN(parcel_id)]
  message("  com_other base values: ", format(n_split, big.mark = ","),
          " land/imps | ", format(n_tot, big.mark = ","),
          " total-only | ", format(n_none, big.mark = ","), " unusable")
  if (n_split + n_tot == 0)
    warning("com_other: no usable base AV in the extended panel - the ",
            "residual bucket will forecast to $0. Check that ",
            "panel_tbl_retro_com_other carries appr_land_val / ",
            "appr_imps_val / total_assessed.", call. = FALSE)

  # ---- spec_area for the geo-actuals scope ---------------------------------
  if (!"spec_area" %in% names(co_dt)) co_dt[, spec_area := NA_integer_]
  co_dt[, .is_specialty := !is.na(spec_area) & spec_area > 0L]

  # ---- KCA geographic actuals, anchor year only ----------------------------
  geo <- data.table::data.table()
  if (isTRUE(use_kca_actuals) &&
      exists("area_report_actuals", envir = .GlobalEnv)) {
    act <- data.table::as.data.table(get("area_report_actuals",
                                         envir = .GlobalEnv))
    if (!"report_kind" %in% names(act)) act[, report_kind := "geo"]
    geo <- act[report_kind == "geo" & prop_type == "com" &
                 basis == "population",
               .(area_join = suppressWarnings(as.integer(area)),
                 tax_yr    = assessment_yr + 1L,
                 dlog_geo  = log1p(pct_change))]
    geo <- unique(geo[!is.na(area_join) & is.finite(dlog_geo)],
                  by = c("area_join", "tax_yr"))
  }

  co_dt[, co_rate_source := NA_character_]
  co_dt[, .dlog := NA_real_]

  # The commercial extend panel frequently lacks a usable `area`; the subgroup
  # forecast script joins it from EXTR_Parcel for exactly this reason, and
  # without it the geo anchor silently matches nothing.
  if (nrow(geo) &&
      (!"area" %in% names(co_dt) || all(is.na(co_dt$area)))) {
    kd <- get0("kca_date_data_extracted", envir = .GlobalEnv)
    p_path <- if (!is.null(kd))
      here::here("data", "kca", kd, "EXTR_Parcel.csv") else NA_character_
    if (!is.na(p_path) && file.exists(p_path)) {
      message("  com_other: area missing - joining from EXTR_Parcel ...")
      ar <- data.table::fread(p_path, select = c("Major", "Minor", "Area"),
                              showProgress = FALSE)
      data.table::setnames(ar, c("Major", "Minor", "Area"),
                           c("major", "minor", "area_src"))
      use_dash <- any(grepl("-", utils::head(co_dt$parcel_id, 10L)))
      ar[, parcel_id := paste0(
        stringr::str_pad(trimws(as.character(major)), 6, "left", "0"),
        if (use_dash) "-" else "",
        stringr::str_pad(trimws(as.character(minor)), 4, "left", "0"))]
      ar <- unique(ar[, .(parcel_id, area_src)], by = "parcel_id")
      co_dt[, area := NULL]
      co_dt[ar, on = "parcel_id", area := i.area_src]
      message("    area joined for ",
              format(co_dt[!is.na(area), data.table::uniqueN(parcel_id)],
                     big.mark = ","), " parcels")
      rm(ar)
    }
  }

  if (nrow(geo) && "area" %in% names(co_dt)) {
    co_dt[, .area_join := suppressWarnings(as.integer(as.character(area)))]
    co_dt[geo, on = .(.area_join = area_join, tax_yr = tax_yr),
          .dlog_geo := i.dlog_geo]
    elig <- !is.na(co_dt$.dlog_geo) &
      (geo_scope == "all" | !co_dt$.is_specialty)
    if (any(elig, na.rm = TRUE))
      co_dt[which(elig), `:=`(.dlog = .dlog_geo,
                              co_rate_source = "geo_report")]
    co_dt[, c(".area_join", ".dlog_geo") := NULL]
    message("  KCA geo actuals anchor ",
            format(sum(!is.na(co_dt$.dlog)), big.mark = ","),
            " com_other parcel-years")
  }

  # ---- Modelled residual rate for everything else --------------------------
  if (nrow(rate_tbl)) {
    co_dt[rate_tbl, on = "tax_yr", `:=`(.dlog_mod = i.dlog_other,
                                        .basis    = i.basis)]
    fill <- is.na(co_dt$.dlog) & !is.na(co_dt$.dlog_mod) &
      co_dt$tax_yr >= forecast_start
    if (any(fill, na.rm = TRUE))
      co_dt[which(fill), `:=`(.dlog = .dlog_mod, co_rate_source = .basis)]
    co_dt[, c(".dlog_mod", ".basis") := NULL]
  }

  # ---- Anything still unrated holds flat -----------------------------------
  co_dt[tax_yr >= forecast_start & is.na(.dlog),
        `:=`(.dlog = 0, co_rate_source = "flat_fallback")]
  co_dt[tax_yr < forecast_start, .dlog := 0]

  # ---- Compound forward -----------------------------------------------------
  data.table::setorder(co_dt, parcel_id, tax_yr)
  co_dt[, .cum := cumsum(fifelse(tax_yr >= forecast_start & is.finite(.dlog),
                                 .dlog, 0)), by = parcel_id]

  co_dt[, pred_appr_land_val := fifelse(
    tax_yr >= forecast_start & !is.na(.base_land),
    .base_land * exp(.cum), .obs_land)]
  co_dt[, pred_appr_imps_val := fifelse(
    tax_yr >= forecast_start & !is.na(.base_imps),
    .base_imps * exp(.cum), .obs_imps)]

  co_dt[, pred_total_assessed := NA_real_]
  # Parcels with a land/improvement split: recombine the two grown components.
  co_dt[.split_ok == TRUE,
        pred_total_assessed :=
          fifelse(is.na(pred_appr_land_val), 0, pred_appr_land_val) +
          fifelse(is.na(pred_appr_imps_val), 0, pred_appr_imps_val)]
  co_dt[.split_ok == TRUE &
          is.na(pred_appr_land_val) & is.na(pred_appr_imps_val),
        pred_total_assessed := NA_real_]
  # Total-only parcels: grow the total directly.
  co_dt[.split_ok == FALSE & !is.na(.base_total),
        pred_total_assessed := fifelse(tax_yr >= forecast_start,
                                       .base_total * exp(.cum), .obs_total)]

  co_dt[, c(".obs_land", ".obs_imps", ".base_land", ".base_imps",
            ".obs_total", ".base_total", ".split_ok",
            ".dlog", ".cum", ".is_specialty") := NULL]

  # ---- Audit ---------------------------------------------------------------
  aud <- co_dt[tax_yr >= forecast_start,
               .(parcels = data.table::uniqueN(parcel_id),
                 av_B    = round(sum(pred_total_assessed, na.rm = TRUE) / 1e9, 2)),
               by = .(tax_yr, co_rate_source)][order(tax_yr, -av_B)]
  message("\n  --- com_other rate sources ---")
  print(aud)
  assign("com_other_audit", aud, envir = .GlobalEnv)

  co_dt[]
}

# =============================================================================
# Convenience wrapper — called from main_ml.R in place of the na.locf block
# =============================================================================
run_com_other_forecast <- function(co_dt, scenario, cache_dir,
                                   forecast_start, forecast_end) {

  method  <- get0("com_other_growth_method", envir = .GlobalEnv,
                  ifnotfound = "com_weighted")
  weights <- get0("com_other_blend_weights", envir = .GlobalEnv,
                  ifnotfound = NULL)
  proxy   <- get0("com_other_proxy", envir = .GlobalEnv, ifnotfound = NULL)
  cap     <- get0("com_other_rate_cap", envir = .GlobalEnv, ifnotfound = 0.15)
  use_act <- get0("com_other_use_kca_actuals", envir = .GlobalEnv,
                  ifnotfound = TRUE)
  gscope  <- get0("geo_actuals_scope", envir = .GlobalEnv,
                  ifnotfound = "nonspecialty")

  message("  com_other method = ", method)

  sg   <- com_subgroup_growth(scenario, cache_dir)
  rate <- com_other_rate_by_year(sg, method = method, weights = weights,
                                 proxy = proxy, cap = cap)
  if (nrow(rate)) {
    message("  residual growth rate by year:")
    print(rate[tax_yr >= forecast_start,
               .(tax_yr, pct = round(100 * expm1(dlog_other), 2), basis)])
  }
  assign("com_other_rate_tbl", rate, envir = .GlobalEnv)

  forecast_com_other(co_dt, forecast_start, forecast_end, rate,
                     use_kca_actuals = use_act, geo_scope = gscope)
}

assign("com_subgroup_growth",     com_subgroup_growth,     envir = .GlobalEnv)
assign("com_other_rate_by_year",  com_other_rate_by_year,  envir = .GlobalEnv)
assign("forecast_com_other",      forecast_com_other,      envir = .GlobalEnv)
assign("run_com_other_forecast",  run_com_other_forecast,  envir = .GlobalEnv)

message("xx_com_other_growth.R loaded.")
