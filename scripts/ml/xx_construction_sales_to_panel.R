# =============================================================================
# xx_construction_sales_to_panel.R  —  TRS construction sales tax as features
# =============================================================================
# Why this matters beyond "more data"
# -----------------------------------
# 05_new_construction_forecast.R already reads the TRS construction sales tax
# forecast, but only to size the aggregate new-construction add-on.  The
# parcel-level ML models never see it.
#
# That is a missed opportunity for a specific reason: the construction series
# is one of the very few inputs that GENUINELY VARIES BY SCENARIO.  The CoStar
# exports are Base Case only, and the largest econ driver in the residential
# improvement model (housing permits) is frozen at last-observed values in
# 05_extend_panel_2026_2031.R.  Adding a scenario-varying construction series
# widens the effective scenario signal instead of leaving it at ~13% of model
# gain, which is the mechanism behind the baseline/optimistic/pessimistic
# inversions.
#
# Source: data/CoStar and SPG/<yyyy-mm>_trs_construction.xlsx, sheet "TRS CON".
#   column E = quarter label (e.g. "2027Q1")
#   column H = baseline, I = pessimistic, J = optimistic
#   Scenario columns are populated only on forecast rows; on actuals rows,
#   column J carries a ratio (~1.0), not a level.  The is_forecast guard below
#   mirrors the fix already in 05_new_construction_forecast.R.
#
# Produces (all citywide, joined on tax_yr):
#   con_sales_lvl            annual sum of quarterly obligations
#   con_sales_yoy            year-over-year growth
#   con_sales_lvl_lag1/2     lagged levels
#   con_sales_yoy_lag1       lagged growth
#   con_sales_lag6q_4q       the 6-quarter-lagged 4-quarter sum — the same
#                            construct the NC forecast uses, i.e. construction
#                            activity that has had time to land on the roll
#   con_sales_lag6q_yoy      its growth rate
#
# Expects `panel_tbl` in scope and `scenario` in .GlobalEnv.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringr)
  library(here)
})

scenario_local <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
message("Running xx_construction_sales_to_panel.R (scenario = ",
        scenario_local, ") ...")

trs_file <- get0("trs_construction_file", envir = .GlobalEnv,
                 ifnotfound = here::here("data", "CoStar and SPG",
                                         "2026-07_trs_construction.xlsx"))

con_predictors <- c(
  "con_sales_lvl", "con_sales_yoy",
  "con_sales_lvl_lag1", "con_sales_lvl_lag2", "con_sales_yoy_lag1",
  "con_sales_lag6q_4q", "con_sales_lag6q_yoy"
)

if (!file.exists(trs_file)) {
  message("  \u26a0\ufe0f  ", basename(trs_file),
          " not found — con_sales_* features skipped")
} else {

  trs_full <- readxl::read_excel(trs_file, sheet = "TRS CON",
                                 col_names = FALSE)
  q <- data.table::as.data.table(trs_full)[
    , .(quarter = as.character(.SD[[5]]),
        baseline    = suppressWarnings(as.numeric(.SD[[8]])),
        pessimistic = suppressWarnings(as.numeric(.SD[[9]])),
        optimistic  = suppressWarnings(as.numeric(.SD[[10]])))]
  q <- q[!is.na(quarter) & str_detect(quarter, "^\\d{4}Q[1-4]$")]

  # Actuals rows: scenario columns are not real levels — force them to baseline.
  q[, is_forecast := !is.na(pessimistic)]
  q[is_forecast == FALSE, `:=`(pessimistic = baseline, optimistic = baseline)]

  bad <- q[baseline > 0 &
             (optimistic / baseline < 0.5 | optimistic / baseline > 2 |
                pessimistic / baseline < 0.5 | pessimistic / baseline > 2)]
  if (nrow(bad)) {
    print(bad)
    stop("xx_construction_sales_to_panel.R: scenario columns out of range vs ",
         "baseline for the quarters above — check TRS CON column layout.")
  }

  val_col <- switch(scenario_local,
                    baseline = "baseline", optimistic = "optimistic",
                    pessimistic = "pessimistic",
                    stop("unknown scenario '", scenario_local, "'"))
  q[, value := get(val_col)]
  q[, `:=`(yr = as.integer(str_sub(quarter, 1, 4)),
           qn = as.integer(str_sub(quarter, 6, 6)))]
  data.table::setorder(q, yr, qn)

  message("  quarters: ", nrow(q), " (", q$quarter[1], " to ",
          q$quarter[nrow(q)], ") | scenario column = ", val_col)

  # ---- 6-quarter-lagged 4-quarter sum ---------------------------------------
  # Tax year N is driven by Q3(N-2) + Q4(N-2) + Q1(N-1) + Q2(N-1), matching
  # 05_new_construction_forecast.R.  Computed on the quarterly series then
  # attributed to the tax year it lands on.
  q[, v_l2 := data.table::shift(value, 2L)]
  q[, v_l3 := data.table::shift(value, 3L)]
  q[, v_l4 := data.table::shift(value, 4L)]
  q[, v_l5 := data.table::shift(value, 5L)]
  lag6 <- q[qn == 1L, .(tax_yr = yr,
                        con_sales_lag6q_4q = v_l2 + v_l3 + v_l4 + v_l5)]
  lag6 <- lag6[is.finite(con_sales_lag6q_4q)]

  # ---- Annual level ---------------------------------------------------------
  ann <- q[, .(con_sales_lvl = sum(value, na.rm = TRUE), nq = .N), by = .(tax_yr = yr)]
  ann <- ann[nq == 4L][, nq := NULL]          # drop part-years at the edges

  con <- merge(ann, lag6, by = "tax_yr", all = TRUE)
  data.table::setorder(con, tax_yr)
  con[, con_sales_yoy       := con_sales_lvl / data.table::shift(con_sales_lvl, 1L) - 1]
  con[, con_sales_lag6q_yoy := con_sales_lag6q_4q /
        data.table::shift(con_sales_lag6q_4q, 1L) - 1]
  con[, con_sales_lvl_lag1  := data.table::shift(con_sales_lvl, 1L)]
  con[, con_sales_lvl_lag2  := data.table::shift(con_sales_lvl, 2L)]
  con[, con_sales_yoy_lag1  := data.table::shift(con_sales_yoy, 1L)]

  con <- con[, c("tax_yr", con_predictors), with = FALSE]

  # ---- Cache (mirrors the econ caches so extend scripts can pick it up) -----
  cache_dir <- get0("cache_dir", envir = .GlobalEnv,
                    ifnotfound = here::here("data", "cache"))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(con, file.path(cache_dir,
                         paste0("con_sales_all_years_", scenario_local, ".rds")))
  saveRDS(con[tax_yr >= 2026],
          file.path(cache_dir,
                    paste0("con_sales_fcst_2026_2031_", scenario_local, ".rds")))
  message("  \U1f4be cached con_sales_all_years_", scenario_local, ".rds")

  # ---- Join ------------------------------------------------------------------
  pt <- data.table::as.data.table(panel_tbl)
  pt[, tax_yr := as.integer(tax_yr)]
  # Drop first so repeated joins can't leave .x/.y twins behind.
  drop_existing <- grep("^con_sales_", names(pt), value = TRUE)
  if (length(drop_existing)) pt[, (drop_existing) := NULL]
  pt <- merge(pt, con, by = "tax_yr", all.x = TRUE)

  fc <- get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2027L)
  covg <- con[tax_yr >= fc & !is.na(con_sales_lvl), .N]
  message("  \u2705 con_sales features joined | forecast years covered: ", covg)
  if (covg == 0)
    message("  \u26a0\ufe0f  no construction series past ", fc,
            " — these features will be dead across the horizon, exactly like ",
            "the cs_off_* block.  Check the TRS CON forecast rows before ",
            "training on them.")

  panel_tbl <- pt
  assign("panel_tbl", pt, envir = .GlobalEnv)
  assign("con_sales_panel", con, envir = .GlobalEnv)
  assign("con_predictors", con_predictors, envir = .GlobalEnv)
  rm(pt, q)
  gc(verbose = FALSE)
}

message("xx_construction_sales_to_panel.R loaded")
