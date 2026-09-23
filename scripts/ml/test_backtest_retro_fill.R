# test_backtest_retro_fill.R ---------------------------------------------------
# Synthetic-data check that retro_fill_av() (main_ml.R) reproduces the inline
# Step 4b/4c code it replaced, and that the D3 counts are right.
#
#   Rscript scripts/ml/test_backtest_retro_fill.R
#
# Needs no data under data/.  Sources main_ml.R with MAIN_ML_DEFINE_ONLY so
# nothing runs.
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(zoo); library(here)
})
MAIN_ML_DEFINE_ONLY <- TRUE
source(here::here("scripts", "ml", "main_ml.R"))
stopifnot(exists("retro_fill_av"), exists("run_main_ml"))

# ---- The pre-refactor inline code, verbatim from main_ml.R Step 4b ---------
old_inline_4b <- function(panel_retro_sg, av_history_cln) {
  data.table::setDT(panel_retro_sg)
  data.table::setkeyv(panel_retro_sg, c("parcel_id", "tax_yr"))
  if ("log_appr_land_val" %in% names(panel_retro_sg) &&
      all(is.na(panel_retro_sg$log_appr_land_val))) {
    av_fix <- data.table::as.data.table(av_history_cln)
    av_fix[, parcel_id := gsub("-", "", parcel_id)]
    if (any(grepl("-", utils::head(panel_retro_sg$parcel_id, 10))))
      av_fix[, parcel_id := paste0(substr(parcel_id, 1, 6), "-",
                                   substr(parcel_id, 7, 10))]
    av_fix <- av_fix[parcel_id %in% unique(panel_retro_sg$parcel_id),
                     .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
    panel_retro_sg[av_fix, on = .(parcel_id, tax_yr),
                   `:=`(appr_land_val = i.appr_land_val,
                        appr_imps_val = i.appr_imps_val)]
    panel_retro_sg[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
    panel_retro_sg[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]
    panel_retro_sg[, total_assessed :=
      fifelse(is.na(appr_land_val), 0, appr_land_val) +
      fifelse(is.na(appr_imps_val), 0, appr_imps_val)]
    panel_retro_sg[total_assessed > 0, log_total_assessed := log(total_assessed)]
  }
  for (col in c("log_appr_land_val", "log_appr_imps_val", "log_total_assessed")) {
    if (col %in% names(panel_retro_sg))
      panel_retro_sg[, (col) := zoo::na.locf(zoo::na.locf(get(col), na.rm = FALSE),
                                              fromLast = TRUE,
                                              na.rm = FALSE), by = parcel_id]
  }
  panel_retro_sg
}

# ---- Synthetic panel: dash-format ids, AV all-NA as the panel builder leaves it
set.seed(1)
ids  <- sprintf("%06d-%04d", sample(100000:999999, 40), sample(0:9999, 40))
yrs  <- 2006:2026
mk_panel <- function() {
  p <- CJ(parcel_id = ids, tax_yr = yrs)
  p[, `:=`(appr_land_val = NA_real_, appr_imps_val = NA_real_,
           total_assessed = NA_real_, log_appr_land_val = NA_real_,
           log_appr_imps_val = NA_real_, log_total_assessed = NA_real_,
           some_feature = as.numeric(seq_len(.N)))]
  p
}
# av history: no-dash ids, gaps at random, some parcels start late, some
# parcels have zero improvements
av <- CJ(parcel_id = gsub("-", "", ids), tax_yr = yrs)
av[, appr_land_val := round(runif(.N, 1e5, 2e6))]
av[, appr_imps_val := round(runif(.N, 0, 3e6))]
av[parcel_id %in% unique(parcel_id)[1:5], appr_imps_val := 0]        # land-only
av <- av[!(parcel_id %in% unique(parcel_id)[6:12] & tax_yr < 2015)]  # start late
av <- av[runif(.N) > 0.15]                                            # gaps
av <- av[!(parcel_id %in% unique(parcel_id)[13:15] & tax_yr > 2020)] # end early

# ---- 1. backfill = TRUE reproduces the inline code ------------------------
a <- old_inline_4b(mk_panel(), av)
b <- suppressMessages(retro_fill_av(mk_panel(), cache_dir = tempdir(),
                                    backfill = TRUE, label = "test",
                                    av_hist = av))
setattr(b, "locf_counts", NULL)
setcolorder(b, names(a))
stopifnot(identical(as.data.frame(a), as.data.frame(b)))
cat("PASS  retro_fill_av(backfill = TRUE) == inline Step 4b code\n")

# ---- 2. backfill = FALSE == forward-only fill -------------------------------
c_ <- suppressMessages(retro_fill_av(mk_panel(), cache_dir = tempdir(),
                                     backfill = FALSE, label = "test",
                                     av_hist = av))
counts <- attr(c_, "locf_counts")
ref <- old_inline_4b(mk_panel(), av)            # has backfill applied
fwd <- mk_panel()
# rebuild the re-joined-but-unfilled frame to check the forward pass alone
fwd <- suppressMessages(retro_fill_av(fwd, cache_dir = tempdir(),
                                      backfill = FALSE, av_hist = av))
for (col in c("log_appr_land_val", "log_appr_imps_val", "log_total_assessed")) {
  # forward-only result must equal ref wherever ref's value came from a
  # non-later year, i.e. equal ref except where the backward pass acted
  back_only <- is.na(fwd[[col]]) & !is.na(ref[[col]])
  stopifnot(all(fwd[[col]][!back_only] == ref[[col]][!back_only], na.rm = TRUE))
  stopifnot(all(is.na(fwd[[col]]) == (is.na(ref[[col]]) | back_only)))
  # reported n_backfilled equals the direct count
  stopifnot(counts[column == col, n_backfilled] == sum(back_only))
}
stopifnot(all(counts$backfill_applied == FALSE))
cat("PASS  retro_fill_av(backfill = FALSE) is forward-only; n_backfilled = ",
    paste(counts$n_backfilled, collapse = "/"), " matches direct count\n")

# ---- 3. no leak: values in rows <= T never come from rows > T ---------------
T <- 2022L
trunc <- mk_panel()[tax_yr <= T]
d <- suppressMessages(retro_fill_av(trunc, cache_dir = tempdir(),
                                    backfill = FALSE, av_hist = av))
full <- suppressMessages(retro_fill_av(mk_panel(), cache_dir = tempdir(),
                                       backfill = TRUE, av_hist = av))
# a parcel whose history ends 2020 and is NA 2021-2022: forward fill from 2020
# in both; a parcel that starts 2015: rows 2006-2014 stay NA in d (no later
# value available) but are backfilled in full
late <- sort(unique(gsub("-", "", ids)))[6]   # CJ() sorted av by id
late_d <- paste0(substr(late, 1, 6), "-", substr(late, 7, 10))
stopifnot(all(is.na(d[parcel_id == late_d & tax_yr < 2015, log_appr_land_val])))
stopifnot(all(!is.na(full[parcel_id == late_d & tax_yr < 2015, log_appr_land_val])))
cat("PASS  truncated + forward-only leaves pre-history NA; production backfills it\n")

cat("\nAll retro_fill_av checks passed.\n")
