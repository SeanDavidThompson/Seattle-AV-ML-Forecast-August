# backtest_origin.R -------------------------------------------------------------
# Child-process entry point for one backtest origin.  Launched by
# run_backtest() (backtest_harness.R) as
#
#   Rscript --vanilla scripts/ml/backtest_origin.R <origin_<T>_config.rds>
#
# so that each origin starts with an empty heap and exits when done.  Can also
# be run by hand with a config saved by run_backtest().
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[1]))
  stop("usage: Rscript backtest_origin.R <origin_<T>_config.rds>")
cfg <- readRDS(args[1])

setwd(cfg$repo_root)
options(warn = 1)   # print warnings as they happen, in the log

MAIN_ML_DEFINE_ONLY <- TRUE
source(file.path(cfg$repo_root, "scripts", "ml", "main_ml.R"))
source(file.path(cfg$repo_root, "scripts", "ml", "backtest_harness.R"))
if (!is.null(cfg$kca_date)) CFG$kca_date_data_extracted <- cfg$kca_date

meta <- bt_run_origin(cfg)
quit(save = "no", status = 0L)
