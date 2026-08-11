# =============================================================================
# qa_trajectory.R — post-forecast trajectory QA, all tracks (baseline)
# -----------------------------------------------------------------------------
# Run AFTER all three tracks have forecasted.  Reads the forecasted caches and
# prints per-year AV totals 2023-2031 with YoY, per-subgroup commercial detail,
# subgroup value shares, and the citywide grand total vs the certificate.
#
# Usage:  source(here::here("scripts","ml","qa_trajectory.R"))
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(scales) })

scen      <- get0("scenario", ifnotfound = "baseline")
cache_dir <- get0("cache_dir", ifnotfound = here::here("data", "cache"))
CERT_2026 <- 308.78   # $B — Seattle TY2026 certified grand total (signed cert)
YRS       <- 2023:2031

num0 <- function(x) if (is.null(x)) NULL else as.numeric(x)

# Row-level best-available total AV:
#   observed appr_* first, else pred_appr_*, else pred_total_assessed,
#   else total_assessed.  NULL-safe: absent columns are skipped entirely
#   (fcoalesce errors on NULL args), and all-absent falls back to NA.
row_total <- function(dt) {
  pick <- function(...) {
    v <- Filter(Negate(is.null),
                lapply(c(...), function(cn) num0(dt[[cn]])))
    if (length(v) == 0) return(rep(NA_real_, nrow(dt)))
    if (length(v) == 1) return(v[[1]])
    do.call(fcoalesce, v)
  }
  land <- pick("appr_land_val", "appr_land_val_filled", "pred_appr_land_val")
  impr <- pick("appr_imps_val", "appr_imps_val_filled", "pred_appr_imps_val")
  li   <- fifelse(is.na(land) & is.na(impr), NA_real_,
                  fcoalesce(land, 0) + fcoalesce(impr, 0))
  fcoalesce(li,
            pick("pred_total_assessed"),
            pick("total_assessed"))
}

yoy_tbl <- function(dt, label, filter_expr = NULL) {
  d <- as.data.table(dt)
  if (!is.null(filter_expr)) d <- d[eval(filter_expr)]
  d[, .tot := row_total(d)]
  s <- d[tax_yr %in% YRS,
         .(total_B = sum(.tot, na.rm = TRUE) / 1e9,
           n = uniqueN(parcel_id),
           n_na = sum(is.na(.tot))),
         keyby = tax_yr]
  s[, yoy_pct := round(100 * (total_B / shift(total_B) - 1), 2)]
  s[, total_B := round(total_B, 3)]
  cat("\n=== ", label, " ===\n", sep = "")
  print(s)
  invisible(s)
}

read_cache <- function(f) {
  p <- file.path(cache_dir, f)
  if (!file.exists(p)) { cat("  (missing: ", f, ")\n", sep = ""); return(NULL) }
  as.data.table(readRDS(p))
}

# ---- RES (exclude small-MF: forecast in BOTH tracks; keep com side) ---------
res <- read_cache(paste0("panel_tbl_2006_2031_forecasted_", scen, "_res.rds"))
res_s <- NULL
if (!is.null(res)) {
  # is_small_mf lives in parcel_res_full but is not carried into the panel —
  # derive it by parcel_id join (dash-insensitive) so the rollup can exclude
  # the ~6.5K parcels that are also forecast in the commercial track.
  if (!"is_small_mf" %in% names(res)) {
    prf <- file.path(cache_dir, "parcel_res_full.rds")
    if (file.exists(prf)) {
      pr <- as.data.table(readRDS(prf))
      if ("is_small_mf" %in% names(pr) && "parcel_id" %in% names(pr)) {
        smf_ids <- unique(gsub("-", "", pr[is_small_mf == 1, parcel_id]))
        res[, is_small_mf := as.integer(gsub("-", "", parcel_id) %chin% smf_ids)]
        cat("\nDerived is_small_mf from parcel_res_full.rds: ",
            comma(res[is_small_mf == 1, uniqueN(parcel_id)]),
            " parcels flagged.\n", sep = "")
      }
      rm(pr)
    }
  }
  if ("is_small_mf" %in% names(res)) {
    n_smf <- res[is_small_mf == 1, uniqueN(parcel_id)]
    cat("\nSmall-MF parcels excluded from res side of rollups: ",
        comma(n_smf), "\n", sep = "")
    res_s <- yoy_tbl(res, "RESIDENTIAL (ex small-MF)",
                     quote(is.na(is_small_mf) | is_small_mf != 1))
  } else {
    cat("\n\u26a0\ufe0f  is_small_mf unavailable (parcel_res_full.rds missing?)",
        "— rollup may double-count ~6.5K parcels also in com.\n")
    res_s <- yoy_tbl(res, "RESIDENTIAL (no small-MF flag!)")
  }
}

# ---- COM (combined, per subgroup, value shares) -----------------------------
com <- read_cache(paste0("panel_tbl_2006_2031_forecasted_", scen, "_com.rds"))
com_s <- NULL
if (!is.null(com)) {
  com_s <- yoy_tbl(com, "COMMERCIAL (all subgroups + other)")
  if ("com_subgroup" %in% names(com)) {
    for (k in sort(unique(com$com_subgroup)))
      yoy_tbl(com[com_subgroup == k], paste0("COM / ", k))
    # Value share by subgroup, 2026 observed vs 2031 forecast
    com[, .tot := row_total(com)]
    sh <- com[tax_yr %in% c(2026, 2031),
              .(total_B = sum(.tot, na.rm = TRUE) / 1e9),
              keyby = .(tax_yr, com_subgroup)]
    sh[, share_pct := round(100 * total_B / sum(total_B), 1), by = tax_yr]
    sh[, total_B := round(total_B, 2)]
    cat("\n=== COM value share by subgroup (2026 vs 2031) ===\n")
    print(dcast(sh, com_subgroup ~ tax_yr,
                value.var = c("total_B", "share_pct")))
    co31 <- sh[tax_yr == 2031 & com_subgroup == "com_other", total_B]
    co26 <- sh[tax_yr == 2026 & com_subgroup == "com_other", total_B]
    if (length(co31) && length(co26) && co26 > 0 && co31 < 0.5 * co26)
      cat("\n\u274c com_other 2031 (", co31, "B) << 2026 (", co26,
          "B): naive carry-forward is NOT carrying values into forecast",
          " years — patch needed.\n", sep = "")
  }
}

# ---- CONDO ------------------------------------------------------------------
condo <- read_cache(paste0("panel_tbl_2006_2031_forecasted_", scen, "_condo.rds"))
condo_s <- NULL
if (!is.null(condo)) {
  # Known condo panel defect: observed-year appr_* are all-NA (combine-script
  # AV join).  Splice observed years from av_history_cln so the trajectory
  # table shows real history instead of $0.
  fs <- get0("forecast_start", ifnotfound = 2027L)
  need_av <- !"appr_land_val" %in% names(condo) ||
    condo[tax_yr < fs, all(is.na(appr_land_val))]
  if (need_av) {
    avp <- file.path(cache_dir, "av_history_cln.rds")
    if (file.exists(avp)) {
      av <- as.data.table(readRDS(avp))
      av[, pid := gsub("-", "", parcel_id)]
      condo[, pid := gsub("-", "", parcel_id)]
      if (!"appr_land_val" %in% names(condo)) condo[, appr_land_val := NA_real_]
      if (!"appr_imps_val" %in% names(condo)) condo[, appr_imps_val := NA_real_]
      condo[av, on = .(pid, tax_yr),
            `:=`(appr_land_val = i.appr_land_val,
                 appr_imps_val = i.appr_imps_val)]
      condo[, pid := NULL]; rm(av)
      cat("\nCondo observed years spliced from av_history_cln",
          "(panel AV all-NA).\n")
    }
  }
  condo_s <- yoy_tbl(condo, "CONDO")
}

# ---- GRAND TOTAL ------------------------------------------------------------
parts <- Filter(Negate(is.null), list(res = res_s, com = com_s, condo = condo_s))
if (length(parts) == 3) {
  g <- rbindlist(lapply(names(parts), function(n)
    parts[[n]][, .(tax_yr, total_B, track = n)]))
  gt <- g[, .(grand_B = round(sum(total_B), 2)), keyby = tax_yr]
  gt[, yoy_pct := round(100 * (grand_B / shift(grand_B) - 1), 2)]
  cat("\n=== GRAND TOTAL (res ex-smallMF + com + condo) ===\n")
  print(gt)
  g26 <- gt[tax_yr == 2026, grand_B]
  if (length(g26))
    cat("\n2026 grand total: $", g26, "B vs certificate $", CERT_2026,
        "B  (gap = ", round(g26 - CERT_2026, 2), "B; expect a shortfall",
        " from parcels outside av_history coverage + exempt/other rolls)\n",
        sep = "")
  cat("\nObserved-year sanity: 2024\u21922025 should be ~ +3.2%,",
      "2025\u21922026 ~ +4.3% (from certified rolls).\n")
  cat("Res 2027 YoY should be negative-ish (area reports: -3 to -6%",
      "for Seattle res areas; res 2027 is nearly all-ML, so compare",
      "direction, not magnitude).\n")
}
