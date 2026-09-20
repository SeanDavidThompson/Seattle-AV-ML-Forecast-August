# test_backtest_scoring.R --------------------------------------------------------
# Synthetic-data checks for the harness functions that do not need the
# pipeline: driver extraction / realization, prediction scoring, growth
# definition, CSV header round-trip, smoke check.
#
#   Rscript scripts/ml/test_backtest_scoring.R
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(here) })
MAIN_ML_DEFINE_ONLY <- TRUE
source(here::here("scripts", "ml", "main_ml.R"))
source(here::here("scripts", "ml", "backtest_harness.R"))

set.seed(7)
ids <- sprintf("%06d%04d", sample(1e5:999999, 60), sample(0:9999, 60))

# ---- 1. driver extraction: constant-within-year + allowlist -----------------
p <- CJ(parcel_id = ids, tax_yr = 2006:2026)
p[, econ_x_yoy_lag1 := tax_yr * 0.01]                 # citywide, allowlisted
p[, sea_pmedesfh_lag12 := 1000 + tax_yr]              # citywide, allowlisted
p[, econ_x_yoy_lag1.y := econ_x_yoy_lag1]             # twin, allowlisted
p[, hi_rolloff_next_val := as.numeric(seq_len(.N))]   # parcel-level
p[, weird_const := 5]                                 # constant, not allowlisted
p[, appr_land_val := runif(.N, 1e5, 1e6)]             # AV: excluded
dr <- suppressMessages(bt_extract_drivers(p, 2023:2026, "t"))
stopifnot(setequal(dr$swapped, c("econ_x_yoy_lag1", "sea_pmedesfh_lag12", "econ_x_yoy_lag1.y")))
stopifnot(identical(dr$frozen, "weird_const"))
stopifnot(nrow(dr$drivers) == 4L, abs(dr$drivers[tax_yr == 2024, econ_x_yoy_lag1] - 20.24) < 1e-9)
cat("PASS  bt_extract_drivers: allowlist + constant-within-year + AV excluded\n")

# ---- 2. realize drivers into an extended panel ------------------------------
tmp <- file.path(tempdir(), "bt_test"); dir.create(tmp, showWarnings = FALSE)
d <- list(cache = tmp, out = tmp)
ext <- CJ(parcel_id = ids, tax_yr = 2006:2026)
ext[, econ_x_yoy_lag1 := fifelse(tax_yr <= 2022, tax_yr * 0.01, NA_real_)]  # extend left NA
ext[, sea_pmedesfh_lag12 := fifelse(tax_yr <= 2022, 1000 + tax_yr, 9999)]   # wrong fcst value
ext[, econ_x_yoy_lag1.y := NA_real_]
ext[, appr_land_val := fifelse(tax_yr <= 2022, 5e5, 1)]                     # must be blanked > T
saveRDS(tibble::as_tibble(ext), file.path(tmp, "panel_tbl_2023_2026_inputs_baseline_res.rds"))
saveRDS(ext, file.path(tmp, "panel_tbl_2023_2026_inputs_baseline_com.rds"))  # legacy generic: skipped
done <- suppressMessages(bt_realize_drivers(d, 2022L, 2026L, "baseline",
                                            list(res = dr)))
stopifnot(nrow(done) == 1L, done$track == "res", done$n_cols_realized == 3L)
x <- as.data.table(readRDS(file.path(tmp, "panel_tbl_2023_2026_inputs_baseline_res.rds")))
stopifnot(all(abs(x[tax_yr == 2025, econ_x_yoy_lag1] - 20.25) < 1e-9))
stopifnot(all(x[tax_yr == 2024, sea_pmedesfh_lag12] == 3024))
stopifnot(all(x[tax_yr == 2022, sea_pmedesfh_lag12] == 3022))          # history untouched
stopifnot(all(is.na(x[tax_yr > 2022, appr_land_val])), all(x[tax_yr <= 2022, appr_land_val] == 5e5))
stopifnot(inherits(readRDS(file.path(tmp, "panel_tbl_2023_2026_inputs_baseline_res.rds")), "tbl_df"))
cat("PASS  bt_realize_drivers: forecast rows realized, history untouched, AV blanked, class kept\n")

# ---- 3. scoring on a synthetic origin ---------------------------------------
# observed: 2022-2026, growth 4% a year; parcel 1-10 created 2024 (no obs <= 2023)
obs <- CJ(parcel_id = ids, tax_yr = 2022:2026)
obs[, obs_total := 4e5 * 1.04^(tax_yr - 2022) * (1 + as.integer(factor(parcel_id)) / 100)]
obs[parcel_id %in% ids[1:10] & tax_yr < 2024, obs_total := NA_real_]
obs[, `:=`(obs_land = obs_total * 0.4, obs_imps = obs_total * 0.6)]
obs <- obs[!is.na(obs_total)]
# predictions for origin 2022: exact 4% growth path from the observed 2022 seed,
# except anchored is exact and unanchored is 2 pp too high
mk_pred <- function(anch) {
  base <- obs[tax_yr == 2022, .(parcel_id, b = obs_total)]
  g <- if (anch) 1.04 else 1.06
  pr <- CJ(parcel_id = ids, tax_yr = 2023:2026)
  pr <- merge(pr, base, by = "parcel_id", all.x = TRUE)
  pr[is.na(b), b := 3e5]                       # imputed seed for the new parcels
  pr[, pred_total := b * g^(tax_yr - 2022)]
  pr[, `:=`(pred_land = pred_total * 0.4, pred_imps = pred_total * 0.6,
            track = fifelse(as.integer(factor(parcel_id)) %% 3 == 0, "condo",
                            fifelse(as.integer(factor(parcel_id)) %% 3 == 1, "res", "apt")),
            method = NA_character_, origin = 2022L, horizon = tax_yr - 2022L,
            anchored = anch)]
  pr[, b := NULL]
  pr
}
pred <- rbindlist(list(mk_pred(TRUE), mk_pred(FALSE)))
pred <- merge(pred, obs, by = c("parcel_id", "tax_yr"), all.x = TRUE)
seed <- obs[, .(parcel_id, origin = tax_yr, seed_total = obs_total)]
pred <- merge(pred, seed, by = c("parcel_id", "origin"), all.x = TRUE)
pred[, seed_observed := !is.na(seed_total) & seed_total > 0][, seed_total := NULL]
pred <- pred[!is.na(obs_total) & obs_total > 0]
pred <- bt_with_rollups(pred)

e <- bt_score_errors(pred)
a1 <- e[track == "all" & component == "total" & pop == "seed_observed" & anchored == TRUE]
stopifnot(all(abs(a1$RMSE_log) < 1e-9), all(abs(a1$bias_pct) < 1e-9))
u1 <- e[track == "all" & component == "total" & pop == "seed_observed" & anchored == FALSE]
stopifnot(all(abs(u1$ME_log - log(1.06 / 1.04) * u1$horizon) < 1e-9))
stopifnot(all(abs(u1$bias_pct - ((1.06 / 1.04)^u1$horizon - 1)) < 1e-9))
a2 <- e[track == "all" & component == "total" & pop == "all_scored" & anchored == TRUE]
stopifnot(a2[horizon == 2, RMSE_log] > 0)          # imputed-seed parcels now count
stopifnot(a2[horizon == 2, n] == a1[horizon == 2, n] + 10L)
cat("PASS  bt_score_errors: exact path scores 0; +2pp path scores log(1.06/1.04)*h; ",
    "imputed seeds only in all_scored\n")

g <- bt_score_growth(pred, obs, 2022L, 2026L)
ga <- g[track == "all" & pop == "seed_observed" & anchored == TRUE]
stopifnot(all(abs(ga$g_act - 0.04) < 1e-9), all(abs(ga$err_pp) < 1e-9))
gu <- g[track == "all" & pop == "seed_observed" & anchored == FALSE]
stopifnot(all(abs(gu$g_pred - 0.06) < 1e-9), all(abs(gu$err_pp - 2) < 1e-9))
stopifnot(all(abs(gu$cum_pred - (1.06^gu$horizon - 1)) < 1e-9))
stopifnot(all(ga$coverage == 1))
gs <- g[track == "all" & pop == "all_scored" & anchored == TRUE & tax_yr == 2025]
stopifnot(gs$n_matched == ga[tax_yr == 2025, n_matched] + 10L)   # av_reconcile population
cat("PASS  bt_score_growth: matched-parcel YoY and cumulative growth, cohort vs all_scored\n")

h <- bt_score_by_horizon(pred)
hh <- h[track == "all" & component == "total" & pop == "seed_observed"]
stopifnot(all(abs(hh[anchored == FALSE, d_ME_log_anch_minus_unanch] +
                    log(1.06 / 1.04) * hh[anchored == FALSE, horizon]) < 1e-9))
cat("PASS  bt_score_by_horizon: anchored - unanchored deltas\n")

gh <- bt_growth_by_horizon(g)
stopifnot(abs(gh[track == "all" & pop == "seed_observed" & anchored == FALSE & horizon == 1, mae_err_pp] - 2) < 1e-9)
ok <- suppressMessages(capture.output(r <- bt_smoke_check(g, 2022L, 0.5, 10)))
stopifnot(isTRUE(r))
ok <- suppressMessages(capture.output(r <- bt_smoke_check(g, 2022L, 0.5, 1)))
stopifnot(isFALSE(r))
cat("PASS  smoke check passes at 10pp, fails at 1pp\n")

# ---- 4. CSV header round trip ----------------------------------------------
hdr <- bt_header_text(2022L, 2026L, "baseline", git_sha = "test")
f <- file.path(tmp, "t.csv")
suppressMessages(bt_write_csv(gh, f, hdr))
first <- readLines(f, n = 1)
stopifnot(startsWith(first, "# CONDITIONAL BACKTEST"))
back <- bt_read_csv(f)
stopifnot(nrow(back) == nrow(gh), setequal(names(back), names(gh)))
cat("PASS  CSV carries the header and reads back with fread\n")

# ---- 5. extraction from forecasted panels in .GlobalEnv ---------------------
panel_tbl_forecasted_res <- tibble::as_tibble(data.table(
  parcel_id = c("123456-0001", "123456-0001"), tax_yr = c(2022L, 2023L),
  appr_land_val = c(1, NA), appr_land_val_filled = c(1, 2), appr_imps_val_filled = c(3, 4),
  total_assessed_filled = c(4, 6), land_method = c("observed", "delta"),
  impr_method = c("observed", "actual")))
panel_tbl_forecasted_com <- data.table(
  parcel_id = "222222-0002", tax_yr = 2023L, com_subgroup = "office", appr_land_val = NA_real_,
  pred_appr_land_val = 10, pred_appr_imps_val = 20, pred_total_assessed = 30)
ex <- bt_extract_predictions(2022L, TRUE, 2026L)
stopifnot(nrow(ex$pred) == 2L, all(ex$pred$horizon == 1L),
          ex$pred[track == "res", method] == "delta/actual",
          ex$pred[track == "office", pred_total] == 30,
          all(ex$pred$parcel_id %in% c("1234560001", "2222220002")))
stopifnot(all(ex$leaks$n_obs_av_in_fc_rows == 0))
panel_tbl_forecasted_com[, appr_land_val := 99]
w <- tryCatch(bt_extract_predictions(2022L, TRUE, 2026L), warning = function(w) conditionMessage(w))
stopifnot(is.character(w), grepl("re-join fired", w))
cat("PASS  bt_extract_predictions + forecast-row AV check\n")

cat("\nAll harness scoring checks passed.\n")
