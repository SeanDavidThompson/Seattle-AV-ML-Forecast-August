# test_backtest_followups.R -----------------------------------------------------
# Synthetic-data checks for the backtest harness follow-ups:
#   B1  one-year horizon (forecast_start == forecast_end) validates, extends
#       and scores (h = 1 only)
#   B2  end-of-run scoring covers every origin in predictions/, not only the
#       current call's, and the header lists the origins scored
#   B3  seed recorded per origin and printed in the header
#   B4  tracks -> run_main_ml(prop_scope = ...)
#   B5  git version falls back to $AV_ML_VERSION / VERSION / "unknown"
#
#   source("scripts/ml/test_backtest_followups.R")
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(here) })
MAIN_ML_DEFINE_ONLY <- TRUE
source(here::here("scripts", "ml", "main_ml.R"))
source(here::here("scripts", "ml", "backtest_harness.R"))

expect_error_msg <- function(expr, pattern) {
  msg <- tryCatch({ force(expr); NA_character_ },
                  error = function(e) conditionMessage(e))
  if (is.na(msg) || !grepl(pattern, msg))
    stop("expected an error matching '", pattern, "', got: ", msg)
  invisible(msg)
}

root <- file.path(tempdir(), paste0("bt_followups_", as.integer(Sys.time())))
dir.create(root, recursive = TRUE)

# ---- B1a. run_main_ml accepts forecast_start == forecast_end -----------------
# An invalid locf_backfill is checked after the horizon, so reaching that
# error means the horizon validation passed without running the pipeline.
expect_error_msg(run_main_ml(forecast_start = 2026, forecast_end = 2026,
                             train_through_year = 2025, locf_backfill = "x"),
                 "^locf_backfill must be")
expect_error_msg(run_main_ml(forecast_start = 2027, forecast_end = 2026),
                 "forecast_start must be a year <= forecast_end")
cat("PASS  run_main_ml: forecast_start == forecast_end accepted; start > end refused\n")

# ---- B1b. condo extend step builds a one-year horizon -----------------------
ids <- sprintf("%06d-%04d", 100000 + 1:5, 1:5)
ext_env_run <- function(hist_through) {
  cache <- file.path(root, paste0("extend_", hist_through))
  dir.create(cache, showWarnings = FALSE)
  saveRDS(data.table(tax_yr = 2024:2027, nwmls_condo_x = 1:4),
          file.path(cache, "nwmls_condo_fcst_annual_baseline.rds"))
  saveRDS(data.table(tax_yr = 2024:2027, econ_y = 11:14),
          file.path(cache, "econ_fcst_annual_baseline.rds"))
  assign("panel_tbl_retro_condo",
         CJ(parcel_id = ids, tax_yr = 2020:hist_through)[
           , `:=`(appr_land_val = 1e5, log_appr_land_val = log(1e5))],
         envir = .GlobalEnv)
  assign("scenario", "baseline", envir = .GlobalEnv)
  assign("cache_dir", cache, envir = .GlobalEnv)
  assign("forecast_start", 2026L, envir = .GlobalEnv)
  assign("forecast_end", 2026L, envir = .GlobalEnv)
  suppressMessages(source(here::here("scripts", "ml",
                                     "05_extend_panel_2026_2031_condo.R"),
                          local = new.env()))
  get("panel_tbl_2026_2026_inputs_baseline_condo", envir = .GlobalEnv)
}
ext <- ext_env_run(2025L)
stopifnot(identical(sort(unique(ext[tax_yr > 2025, tax_yr])), 2026L),
          nrow(ext[tax_yr == 2026]) == length(ids),
          all(is.na(ext[tax_yr == 2026, appr_land_val])),
          all(ext[tax_yr == 2026, econ_y] == 13))
expect_error_msg(ext_env_run(2026L), "already reaches tax_yr 2026")
rm(list = c("panel_tbl_retro_condo", "panel_tbl_2026_2026_inputs_baseline_condo",
            "panel_tbl_2006_2031_inputs_baseline_condo"), envir = .GlobalEnv)
cat("PASS  05_extend condo: one-year horizon extends 2026 only; history at forecast_end refused\n")

# ---- synthetic backtest output tree ------------------------------------------
out_dir  <- file.path(root, "backtest")
prod     <- file.path(root, "prod_cache")
d        <- bt_dirs(out_dir, NA)
for (p in c(prod, d$predictions, d$meta)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

set.seed(11)
pids <- sprintf("%06d%04d", 200000 + 1:40, 1:40)
av <- CJ(parcel_id = pids, tax_yr = 2023:2026)
av[, appr_land_val := 2e5 * 1.05^(tax_yr - 2023) *
       (1 + as.integer(factor(parcel_id)) %% 7 / 50)]
av[, appr_imps_val := 1.5 * appr_land_val]
saveRDS(av, file.path(prod, "av_history_cln.rds"))

# origin T predictions for T+1 .. 2026; anchored exact, unanchored +2 pp
write_origin <- function(T, seed) {
  for (anch in c(TRUE, FALSE)) {
    g <- if (anch) 1.05 else 1.07
    b <- av[tax_yr == T, .(parcel_id, b = appr_land_val + appr_imps_val)]
    pr <- CJ(parcel_id = pids, tax_yr = seq(T + 1L, 2026L))[b, on = "parcel_id"]
    pr[, pred_total := b * g^(tax_yr - T)]
    pr[, `:=`(track = fifelse(seq_len(.N) %% 2 == 0, "res", "office"),
              pred_land = pred_total * 0.4, pred_imps = pred_total * 0.6,
              method = NA_character_, origin = T, horizon = tax_yr - T,
              anchored = anch, b = NULL)]
    saveRDS(pr, file.path(d$predictions, paste0("pred_origin", T, "_",
                                                if (anch) "anchored" else "unanchored",
                                                ".rds")))
  }
  saveRDS(list(origin = T, seed = seed, git_sha = paste0("sha", T),
               tracks = BT_TRACKS),
          file.path(d$meta, paste0("origin_", T, "_config.rds")))
}

# ---- B1c. one-year horizon scores (h = 1 only) -------------------------------
write_origin(2025L, 123)
sc <- suppressMessages(bt_score(out_dir, prod, "all", 2026L, "baseline"))
stopifnot(identical(sc$origins_scored, 2025L),
          all(sc$errors_by_horizon$horizon == 1L),
          all(sc$growth$horizon == 1L), nrow(sc$growth) > 0)
e1 <- sc$errors_by_horizon[track == "all" & component == "total" & pop == "seed_observed"]
stopifnot(abs(e1[anchored == TRUE, RMSE_log]) < 1e-9,
          abs(e1[anchored == FALSE, ME_log] - log(1.07 / 1.05)) < 1e-9,
          "d_ME_log_anch_minus_unanch" %in% names(e1))
g1 <- sc$growth[track == "all" & pop == "seed_observed"]
stopifnot(all(abs(g1[anchored == TRUE, err_pp]) < 1e-9),
          all(abs(g1[anchored == FALSE, err_pp] - 2) < 1e-9))
invisible(suppressMessages(capture.output(r <- bt_smoke_check(sc$growth, 2025L))))
stopifnot(isTRUE(r))
cat("PASS  bt_score: origin 2025 / final_year 2026 scores h = 1 only, smoke check passes\n")

# ---- B2 + B3. later call scores every origin; header lists them + seeds ------
write_origin(2024L, 456)
sc2 <- suppressMessages(run_backtest(origins = 2024L, score_only = TRUE,
                                     out_dir = out_dir, prod_cache_dir = prod,
                                     smoke_min_coverage = 0, smoke_max_err_pp = 100))
stopifnot(identical(sc2$origins_scored, c(2024L, 2025L)))
csv <- file.path(out_dir, "errors_by_origin.csv")
hdr <- grep("^# origins scored", readLines(csv, warn = FALSE), value = TRUE)
stopifnot(length(hdr) == 1L,
          grepl("origins scored = 2024, 2025 ", hdr, fixed = TRUE),
          grepl("seed = 2024: 456, 2025: 123", hdr, fixed = TRUE),
          grepl("git = 2024: sha2024, 2025: sha2025", hdr, fixed = TRUE))
stopifnot(setequal(unique(bt_read_csv(csv)$origin), c(2024L, 2025L)))
sc3 <- suppressMessages(bt_score(out_dir, prod, 2025L, 2026L, "baseline", write = FALSE))
stopifnot(identical(sc3$origins_scored, 2025L),
          setequal(unique(bt_read_csv(csv)$origin), c(2024L, 2025L)))   # write = FALSE
cat("PASS  run_backtest(score_only): tables cover all origins on disk; header lists origins + per-origin seed/git\n")

h <- bt_header_text(2025L, 2026L, "baseline", git_sha = "x", seed = 123,
                    tracks = c("res", "com"))
stopifnot(any(grepl("| tracks = res+com | seed = 123 | git = x", h, fixed = TRUE)))
expect_error_msg(run_backtest(origins = 2025L, seed = NULL, out_dir = out_dir,
                              prod_cache_dir = prod), "^seed must be")
cat("PASS  header prints seed and tracks; a run without a seed is refused\n")

# ---- B4. tracks -> prop_scope -------------------------------------------------
stopifnot(bt_prop_scope("all") == "all",
          bt_prop_scope(c("condo", "res", "com")) == "all",
          bt_prop_scope(c("com", "res")) == "both",
          bt_prop_scope("res") == "res", bt_prop_scope("com") == "com",
          bt_prop_scope("condo") == "condo")
expect_error_msg(bt_prop_scope(c("res", "condo")), "no run_main_ml prop_scope")
expect_error_msg(bt_prop_scope("apt"), "got apt")
cat("PASS  bt_prop_scope: tracks map to prop_scope; unsupported combinations refused\n")

# ---- B5. git fallback ----------------------------------------------------------
nogit <- file.path(root, "not_a_checkout"); dir.create(nogit)
old <- Sys.getenv("AV_ML_VERSION", unset = NA)
Sys.unsetenv("AV_ML_VERSION")
stopifnot(identical(bt_git_sha(nogit), "unknown"))
writeLines("v2026.09-share", file.path(nogit, "VERSION"))
stopifnot(identical(bt_git_sha(nogit), "v2026.09-share"))
Sys.setenv(AV_ML_VERSION = "from-env")
stopifnot(identical(bt_git_sha(nogit), "from-env"))
if (is.na(old)) Sys.unsetenv("AV_ML_VERSION") else Sys.setenv(AV_ML_VERSION = old)
if (nzchar(Sys.which("git")) && dir.exists(here::here(".git")))
  stopifnot(grepl("^[0-9a-f]{4,40}$", bt_git_sha(here::here())))
cat("PASS  bt_git_sha: git, then $AV_ML_VERSION, then VERSION, then \"unknown\"\n")

unlink(root, recursive = TRUE)
cat("\nAll backtest follow-up checks passed.\n")
