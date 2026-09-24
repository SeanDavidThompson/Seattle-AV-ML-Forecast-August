# =============================================================================
# backtest_harness.R
# Conditional backtest of the AV pipeline: origins 2022-2025, horizons 1-4.
# Design and the decisions behind it: BACKTEST_DESIGN.md.
#
#   MAIN_ML_DEFINE_ONLY <- TRUE
#   source(here::here("scripts", "ml", "main_ml.R"))
#   source(here::here("scripts", "ml", "backtest_harness.R"))
#   run_backtest(origins = 2025)            # smoke test first (one horizon)
#   run_backtest(origins = 2022:2025)       # full triangle
#
# Per origin T (in a child Rscript process, fresh heap):
#   1. stage the production caches into a scratch cache (hard links)
#   2. build the origin's history: retro panels truncated to tax_yr <= T,
#      forward-only locf (D3), production residential retrofit held fixed (D4)
#   3. run_main_ml(train_through_year = T, forecast_start = T+1,
#                  forecast_end = 2026, stop_after = "extend")
#   4. overwrite the forecast-year citywide drivers in the extended panels
#      with realized values (D1)
#   5. run_main_ml(forecast_only = TRUE) twice: anchored to year-T area
#      reports (D5), and unanchored
#   6. extract parcel-year predictions
# The parent scores every origin against av_history_cln and writes the
# error / growth tables to data/outputs/backtest/.
#
# Writes ONLY under data/outputs/backtest/.  Reads the production cache.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

BT_TRACK_KEYS <- function() {
  keys <- get0("COM_SUBGROUP_KEYS", envir = .GlobalEnv,
               ifnotfound = c("apt", "office", "industrial", "retail",
                              "hospitality", "medical"))
  c(keys, "com_other", "condo")
}

# Citywide driver columns that get realized values in forecast-year rows
# (D1).  Anything else constant-within-year is reported and left frozen.
BT_DRIVER_ALLOWLIST <- "^(econ_|sea_|k_|costar_|cs_|con_sales_)"

# AV / prediction columns blanked in forecast-year rows of the extended
# panels (the extend scripts already do this; kept as a guard).
BT_AV_COLS <- c(
  "appr_land_val", "appr_imps_val", "total_assessed",
  "log_appr_land_val", "log_appr_imps_val", "log_total_assessed",
  "appr_land_val_filled", "appr_imps_val_filled", "total_assessed_filled",
  "log_total_assessed_filled", "log_land_filled", "log_impr_filled",
  "pred_log_land", "pred_land_val", "pred_log_impr", "pred_impr_val",
  "pred_log_land_val", "pred_log_imps_val", "pred_appr_land_val",
  "pred_appr_imps_val", "pred_total_assessed", "pred_log_total"
)

# =============================================================================
# Header text (D1 / D4).  Goes to stdout, README_backtest.txt, every log,
# and the top of every metrics CSV as "#" comment lines.
# =============================================================================
bt_header_text <- function(origins = NULL, final_year = 2026L,
                           scenario = "baseline", git_sha = NULL) {
  if (is.null(git_sha))
    git_sha <- tryCatch(
      trimws(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE,
                     stderr = FALSE)),
      error = function(e) "unknown")
  c(
    "CONDITIONAL BACKTEST - NOT AN EX-ANTE FORECAST RECORD.",
    "For each origin year T the parcel models were trained only on assessment",
    "data through tax year T, and the forecast for T+1 onward was seeded from",
    "values observed through T.  But every macro driver - NWMLS housing series,",
    "OERF/econ series, CoStar market series, TRS construction-sales series - was",
    "fed in at its current (2026) vintage for every forecast year, including",
    "revisions.  The result measures how well the parcel models turn a KNOWN",
    "macro path into assessed values.  It does not measure how well the pipeline",
    "would have forecast in year T, when those paths were themselves forecasts.",
    "Real-time accuracy will be worse than these numbers.  Published OEFA",
    "forecasts placed alongside them were genuinely ex-ante.",
    "",
    "RETROFIT HELD FIXED.  Missing historical residential values were imputed",
    "once, by the production retrofit models fit on data through 2026, and those",
    "imputed values are reused for every origin.  The imputed panel is the base",
    "the extend and forecast steps work from, so imputation shapes the forecast",
    "path wherever a parcel's history has gaps, even though the parcel models",
    "were trained on observed values only.  A strict backtest would re-fit the",
    "retrofit through T.  This is a known source of optimism.  Tables with",
    "pop = seed_observed are restricted to parcels with an observed value at the",
    "origin year, where imputation does not touch the seed.",
    "",
    "KCA area-report anchors, where used (anchored = TRUE), are the year-T",
    "reports.  Commercial/condo forward-fill of missing history uses <= T values",
    "only (no backward fill).  Scoring is against observed AV in av_history_cln,",
    "never a filled value.  Growth is Seattle, matched-parcel, existing-parcel",
    "growth: it excludes new construction and parcels retired before 2026.",
    "OEFA's number is King County total roll growth.",
    "",
    paste0("origins = ", if (is.null(origins)) "(see rows)"
           else paste(origins, collapse = ", "),
           " | final_year = ", final_year, " | scenario = ", scenario,
           " | git = ", git_sha,
           " | generated = ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )
}

bt_write_csv <- function(dt, path, header) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(fileext = ".csv")
  data.table::fwrite(dt, tmp)
  writeLines(c(paste0("# ", header), readLines(tmp, warn = FALSE)), path)
  unlink(tmp)
  message("  wrote ", path, " (", nrow(dt), " rows)")
  invisible(path)
}

bt_read_csv <- function(path) {
  n_hdr <- sum(cumprod(startsWith(readLines(path, warn = FALSE), "#")))
  data.table::fread(path, skip = n_hdr)
}

bt_log <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

# =============================================================================
# Directories
# =============================================================================
bt_dirs <- function(out_dir, origin) {
  root <- file.path(out_dir, "work", paste0("origin_", origin))
  d <- list(out = out_dir, root = root,
            cache = file.path(root, "cache"), model = file.path(root, "model"),
            outputs = file.path(root, "outputs"),
            predictions = file.path(out_dir, "predictions"),
            anchors = file.path(out_dir, "anchors"),
            logs = file.path(out_dir, "logs"),
            meta = file.path(out_dir, "meta"))
  d
}

bt_fresh_dirs <- function(d) {
  if (dir.exists(d$root)) unlink(d$root, recursive = TRUE, force = TRUE)
  for (p in c(d$cache, d$model, d$outputs, d$predictions, d$anchors, d$logs,
              d$meta))
    dir.create(p, showWarnings = FALSE, recursive = TRUE)
  invisible(d)
}

# =============================================================================
# Guard: never write outside data/outputs/backtest
# =============================================================================
bt_assert_scratch <- function(path, out_dir) {
  np <- normalizePath(path, winslash = "/", mustWork = FALSE)
  no <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  if (!startsWith(np, no))
    stop("backtest refuses to write outside ", out_dir, ": ", path)
  invisible(TRUE)
}

# =============================================================================
# 1. Stage production caches (hard link, copy fallback)
# =============================================================================
bt_stage_file <- function(from, to, stage_mode = "link") {
  if (file.exists(to)) return("exists")
  if (stage_mode == "link") {
    ok <- suppressWarnings(tryCatch(file.link(from, to),
                                    error = function(e) FALSE))
    if (isTRUE(ok)) return("linked")
  }
  ok <- file.copy(from, to, copy.date = TRUE)
  if (!isTRUE(ok)) stop("could not stage ", from, " -> ", to)
  "copied"
}

bt_stage_inputs <- function(prod_cache, bt_cache, scenario, stage_mode,
                            keys = BT_TRACK_KEYS()) {
  bt_log("staging production caches into ", bt_cache, " (", stage_mode, ")")
  fixed <- c("panel_tbl_res.rds", "panel_tbl_condo.rds",
             "panel_tbl_com_other.rds", "av_history_cln.rds",
             paste0("panel_tbl_", setdiff(keys, c("com_other", "condo")),
                    ".rds"))
  pats <- c(paste0("^econ_.*_", scenario, "\\.rds$"),
            paste0("^nwmls_.*_", scenario, "\\.rds$"),
            "^costar_.*\\.rds$",
            paste0("^con_sales_.*_", scenario, "\\.rds$"))
  by_pat <- unlist(lapply(pats, function(p)
    list.files(prod_cache, pattern = p, full.names = FALSE)))
  files <- unique(c(fixed, by_pat))
  missing <- files[!file.exists(file.path(prod_cache, files))]
  if (length(missing))
    stop("production cache is missing required inputs: ",
         paste(missing, collapse = ", "))
  res <- vapply(files, function(f)
    bt_stage_file(file.path(prod_cache, f), file.path(bt_cache, f),
                  stage_mode), character(1))
  bt_log("  staged ", length(files), " files: ",
         paste(names(table(res)), table(res), collapse = ", "))
  invisible(res)
}

# =============================================================================
# 2. Build the origin's history
# =============================================================================
bt_norm_id <- function(x) gsub("-", "", as.character(x), fixed = TRUE)

# Per-tax_yr values of the citywide (constant-within-year) driver columns for
# the given years.  Returns list(drivers = data.table(tax_yr, ...),
# swapped = chr, frozen = chr).
bt_extract_drivers <- function(dt, years, label = "") {
  dt <- data.table::as.data.table(dt)
  sub <- dt[tax_yr %in% years]
  if (nrow(sub) == 0)
    return(list(drivers = data.table(tax_yr = integer(0)),
                swapped = character(0), frozen = character(0)))
  num <- names(sub)[vapply(sub, is.numeric, logical(1))]
  num <- setdiff(num, c("tax_yr", BT_AV_COLS))
  const <- num[vapply(num, function(cn) {
    x <- sub[[cn]]
    if (all(is.na(x))) return(FALSE)
    all(sub[, .(u = data.table::uniqueN(get(cn), na.rm = TRUE)), by = tax_yr]$u <= 1L)
  }, logical(1))]
  swapped <- const[grepl(BT_DRIVER_ALLOWLIST, const)]
  frozen  <- setdiff(const, swapped)
  drivers <- sub[, lapply(.SD, function(x) { y <- x[!is.na(x)]; if (length(y)) y[1] else NA_real_ }),
                 by = tax_yr, .SDcols = swapped]
  data.table::setorder(drivers, tax_yr)
  bt_log("  ", label, ": ", length(swapped), " citywide driver column(s) to realize",
         if (length(frozen)) paste0("; ", length(frozen),
                                    " constant-within-year column(s) left frozen: ",
                                    paste(utils::head(frozen, 12), collapse = ", "),
                                    if (length(frozen) > 12) " ..." else "") else "")
  list(drivers = drivers, swapped = swapped, frozen = frozen)
}

bt_build_history <- function(origin, prod_cache, d, final_year, av_hist,
                             keys = BT_TRACK_KEYS()) {
  yrs_fc <- seq(origin + 1L, final_year)
  drivers <- list(); locf <- list(); n_rows <- list()

  # ---- residential: production retrofit, truncated (D4 held fixed) ---------
  bt_log("history: res (production panel_tbl_retro_res, rows <= ", origin, ")")
  r <- data.table::as.data.table(readRDS(file.path(prod_cache, "panel_tbl_retro_res.rds")))
  drivers$res <- bt_extract_drivers(r, yrs_fc, "res")
  r <- r[tax_yr <= origin]
  n_rows$res <- nrow(r)
  saveRDS(tibble::as_tibble(r), file.path(d$cache, "panel_tbl_retro_res.rds"))
  rm(r); gc(verbose = FALSE)

  # ---- commercial subgroups, com_other, condo: raw panel -> truncate ->
  #      re-join AV -> forward-only locf (D3) -----------------------------------
  for (key in keys) {
    src <- file.path(prod_cache, paste0("panel_tbl_", key, ".rds"))
    bt_log("history: ", key)
    p <- data.table::as.data.table(readRDS(src))
    drivers[[key]] <- bt_extract_drivers(p, yrs_fc, key)
    p <- p[tax_yr <= origin]
    p <- retro_fill_av(p, cache_dir = d$cache, backfill = FALSE, label = key,
                       av_hist = av_hist)
    cnt <- attr(p, "locf_counts"); data.table::setattr(p, "locf_counts", NULL)
    if (!is.null(cnt) && nrow(cnt)) { cnt[, track := key]; locf[[key]] <- cnt }
    n_rows[[key]] <- nrow(p)
    saveRDS(p, file.path(d$cache, paste0("panel_tbl_retro_", key, ".rds")))
    rm(p); gc(verbose = FALSE)
  }
  locf <- data.table::rbindlist(locf, fill = TRUE)
  if (nrow(locf)) locf[, `:=`(origin = origin, share_backfilled = n_backfilled / n_rows)]
  saveRDS(drivers, file.path(d$cache, "realized_drivers.rds"))
  list(drivers = drivers, locf = locf, n_rows = n_rows)
}

# =============================================================================
# 4. Realize the drivers in the extended panels
# =============================================================================
bt_track_of_ext_file <- function(f, scenario, keys = BT_TRACK_KEYS()) {
  b <- sub("\\.rds$", "", basename(f))
  suf <- sub(paste0("^panel_tbl_.*_inputs_", scenario), "", b)
  suf <- sub("^_", "", suf)
  if (suf == "" || suf == "res") return("res")
  if (suf %in% keys) return(suf)
  NA_character_   # legacy "_com" generic copy: not read by Step 6
}

bt_realize_drivers <- function(d, origin, final_year, scenario, drivers) {
  files <- list.files(d$cache, pattern = paste0("^panel_tbl_.*_inputs_", scenario,
                                                 ".*\\.rds$"),
                      full.names = TRUE)
  if (!length(files)) stop("no extended panels found in ", d$cache)
  yrs_fc <- seq(origin + 1L, final_year)
  done <- list()
  for (f in files) {
    trk <- bt_track_of_ext_file(f, scenario)
    if (is.na(trk)) { bt_log("realize: skip ", basename(f), " (legacy generic copy)"); next }
    drv <- drivers[[trk]]$drivers
    x <- readRDS(f)
    was_dt <- data.table::is.data.table(x)
    x <- data.table::as.data.table(x)
    if (!"tax_yr" %in% names(x)) stop("no tax_yr in ", f)
    cols <- intersect(setdiff(names(drv), "tax_yr"), names(x))
    idx <- which(x$tax_yr %in% yrs_fc)
    if (!length(idx)) stop("extended panel ", basename(f), " has no rows for ",
                           paste(yrs_fc, collapse = ","))
    for (cn in cols) {
      v <- drv[[cn]][match(x$tax_yr[idx], drv$tax_yr)]
      if (!identical(typeof(x[[cn]]), typeof(v))) {
        data.table::set(x, j = cn, value = as.numeric(x[[cn]]))
        v <- as.numeric(v)
      }
      data.table::set(x, i = idx, j = cn, value = v)
    }
    for (cn in intersect(BT_AV_COLS, names(x)))
      data.table::set(x, i = idx, j = cn, value = NA_real_)
    bt_assert_scratch(f, d$out)
    saveRDS(if (was_dt) x else tibble::as_tibble(x), f)
    done[[basename(f)]] <- data.table(file = basename(f), track = trk,
                                      n_cols_realized = length(cols),
                                      n_rows_fc = length(idx))
    bt_log("realize: ", basename(f), " (", trk, ") ", length(cols),
           " cols x ", length(idx), " rows")
    rm(x); gc(verbose = FALSE)
  }
  data.table::rbindlist(done)
}

# =============================================================================
# 6. Extract predictions from the forecasted panels in .GlobalEnv
# =============================================================================
bt_extract_predictions <- function(origin, anchored, final_year) {
  yrs_fc <- seq(origin + 1L, final_year)
  out <- list(); leaks <- list()
  # Subset to the forecast years BEFORE converting, so the multi-GB history
  # rows are never copied.
  g <- function(nm) {
    if (!exists(nm, envir = .GlobalEnv)) return(NULL)
    x <- get(nm, envir = .GlobalEnv)
    data.table::as.data.table(x[x$tax_yr %in% yrs_fc, , drop = FALSE])
  }

  res <- g("panel_tbl_forecasted_res")
  if (!is.null(res)) {
    r <- res
    leaks$res <- r[, .(track = "res", n_fc_rows = .N,
                       n_obs_av_in_fc_rows = sum(!is.na(appr_land_val)))]
    out$res <- r[, .(parcel_id = bt_norm_id(parcel_id), track = "res", tax_yr,
                     pred_land  = as.numeric(appr_land_val_filled),
                     pred_imps  = as.numeric(appr_imps_val_filled),
                     pred_total = as.numeric(total_assessed_filled),
                     method = paste0(fifelse(is.na(land_method), "na", land_method), "/",
                                     fifelse(is.na(impr_method), "na", impr_method)))]
    rm(r)
  }
  com <- g("panel_tbl_forecasted_com")
  if (!is.null(com)) {
    r <- com
    if (!"com_subgroup" %in% names(r)) r[, com_subgroup := "com"]
    leaks$com <- r[, .(n_fc_rows = .N,
                       n_obs_av_in_fc_rows = sum(!is.na(appr_land_val))),
                   by = .(track = com_subgroup)]
    out$com <- r[, .(parcel_id = bt_norm_id(parcel_id), track = com_subgroup, tax_yr,
                     pred_land  = as.numeric(pred_appr_land_val),
                     pred_imps  = as.numeric(pred_appr_imps_val),
                     pred_total = as.numeric(pred_total_assessed),
                     method = NA_character_)]
    rm(r)
  }
  condo <- g("panel_tbl_forecasted_condo")
  if (!is.null(condo)) {
    r <- condo
    leaks$condo <- r[, .(track = "condo", n_fc_rows = .N,
                         n_obs_av_in_fc_rows = sum(!is.na(appr_land_val)))]
    out$condo <- r[, .(parcel_id = bt_norm_id(parcel_id), track = "condo", tax_yr,
                       pred_land  = as.numeric(pred_appr_land_val),
                       pred_imps  = as.numeric(pred_appr_imps_val),
                       pred_total = as.numeric(pred_total_assessed),
                       method = NA_character_)]
    rm(r)
  }
  pred <- data.table::rbindlist(out, use.names = TRUE)
  if (!nrow(pred)) stop("no forecasted panels found in .GlobalEnv after Step 6")
  pred[, `:=`(origin = origin, horizon = tax_yr - origin, anchored = anchored)]
  leaks <- data.table::rbindlist(leaks, use.names = TRUE, fill = TRUE)
  # §7 of the design: forecast-year rows must not carry observed AV.  A
  # non-zero count means a defensive re-join fired inside a forecast script.
  if (nrow(leaks) && any(leaks$n_obs_av_in_fc_rows > 0))
    warning("origin ", origin, " anchored=", anchored,
            ": observed appr_land_val present in forecast-year rows: ",
            paste0(leaks$track, "=", leaks$n_obs_av_in_fc_rows, collapse = ", "),
            " - a defensive AV re-join fired inside Step 6; see BACKTEST_DESIGN.md §7",
            call. = FALSE)
  list(pred = pred, leaks = leaks)
}

# =============================================================================
# One origin (runs in a child process)
# =============================================================================
bt_check_area_reports <- function(origins, area_reports_root) {
  bad <- character(0)
  for (.yr in origins) {
    dd <- file.path(area_reports_root, as.character(.yr))
    # Reports sit in <year>/residential/ and <year>/commercial/
    n  <- if (dir.exists(dd)) length(list.files(dd, pattern = "\\.pdf$", recursive = TRUE,
                                                ignore.case = TRUE)) else 0L
    if (n == 0L) bad <- c(bad, paste0(.yr, " (", dd, ")"))
  }
  if (length(bad))
    stop("anchored backtest needs year-T area reports; none found for: ",
         paste(bad, collapse = "; "),
         "\nDownload them into data/kca/area_reports/<year>/ or pass a ",
         "subset of `origins`.", call. = FALSE)
  invisible(TRUE)
}

bt_run_origin <- function(cfg) {
  origin <- as.integer(cfg$origin)
  t0 <- Sys.time()
  hdr <- bt_header_text(origin, cfg$final_year, cfg$scenario)
  message(paste(hdr, collapse = "\n"))
  message("\n=== backtest origin ", origin, " : forecast ", origin + 1L, "-",
          cfg$final_year, " | anchored modes: ",
          paste(cfg$anchored, collapse = ", "), " ===\n")

  d <- bt_dirs(cfg$out_dir, origin)
  bt_assert_scratch(d$root, cfg$out_dir)
  bt_fresh_dirs(d)

  if (isTRUE(any(cfg$anchored)))
    bt_check_area_reports(origin, cfg$area_reports_root)

  # ---- 1. stage --------------------------------------------------------------
  bt_stage_inputs(cfg$prod_cache, d$cache, cfg$scenario, cfg$stage_mode)

  # ---- 2. history -------------------------------------------------------------
  av_hist <- data.table::as.data.table(readRDS(file.path(cfg$prod_cache,
                                                         "av_history_cln.rds")))
  hist <- bt_build_history(origin, cfg$prod_cache, d, cfg$final_year, av_hist)
  rm(av_hist); gc(verbose = FALSE)
  if (nrow(hist$locf))
    bt_log("locf backfill (would-have-filled) by track: ",
           paste(hist$locf[column == "log_total_assessed",
                           paste0(track, "=", n_backfilled)], collapse = ", "))

  # ---- 3. pass 1: train + retro(load) + extend -------------------------------
  bt_log("pass 1: run_main_ml(train_through_year = ", origin, ", stop_after = 'extend')")
  run_main_ml(prop_scope = "all", scenario = cfg$scenario,
              panel_replicate = FALSE, model_replicate = TRUE,
              retrofit_replicate = FALSE, extend_replicate = TRUE,
              forecast_only = FALSE, diagnostics_replicate = FALSE,
              train_through_year = origin, locf_backfill = FALSE,
              forecast_start = origin + 1L, forecast_end = cfg$final_year,
              use_area_actuals = FALSE,
              cache_dir = d$cache, model_dir = d$model, output_dir = d$outputs,
              stop_after = "extend")

  # ---- 4. realize drivers -----------------------------------------------------
  realized <- bt_realize_drivers(d, origin, cfg$final_year, cfg$scenario,
                                 hist$drivers)

  # ---- 5/6. pass 2 per anchor mode --------------------------------------------
  meta <- list(origin = origin, locf = hist$locf, realized = realized,
               drivers_swapped = lapply(hist$drivers, `[[`, "swapped"),
               drivers_frozen  = lapply(hist$drivers, `[[`, "frozen"),
               n_rows_history = hist$n_rows, anchor_coverage = list(),
               leaks = list(), timing = list())
  for (anch in cfg$anchored) {
    tag <- if (anch) "anchored" else "unanchored"
    bt_log("pass 2 (", tag, "): run_main_ml(forecast_only = TRUE)")
    t1 <- Sys.time()
    if (exists("actuals_rate_coverage", envir = .GlobalEnv))
      rm("actuals_rate_coverage", envir = .GlobalEnv)
    run_main_ml(prop_scope = "all", scenario = cfg$scenario,
                panel_replicate = FALSE, model_replicate = FALSE,
                retrofit_replicate = FALSE, extend_replicate = FALSE,
                forecast_only = TRUE, diagnostics_replicate = FALSE,
                train_through_year = origin, locf_backfill = FALSE,
                forecast_start = origin + 1L, forecast_end = cfg$final_year,
                use_area_actuals = anch, area_reports_year = origin,
                require_area_actuals = anch,
                cache_dir = d$cache, model_dir = d$model, output_dir = d$outputs)
    ex <- bt_extract_predictions(origin, anch, cfg$final_year)
    pf <- file.path(d$predictions, paste0("pred_origin", origin, "_", tag, ".rds"))
    saveRDS(ex$pred, pf)
    bt_log("  ", nrow(ex$pred), " parcel-year predictions -> ", basename(pf))
    meta$leaks[[tag]] <- ex$leaks
    if (anch) {
      if (exists("actuals_rate_coverage", envir = .GlobalEnv)) {
        arc <- data.table::as.data.table(get("actuals_rate_coverage", envir = .GlobalEnv))
        arc[, `:=`(origin = origin, anchored = TRUE)]
        meta$anchor_coverage$com <- arc
      }
      meta$anchor_coverage$res <- ex$pred[track == "res",
                                          .(n = .N), by = .(origin, tax_yr, method)]
      for (f in list.files(d$outputs, pattern = paste0("^area_report_actuals_",
                                                       origin, "\\.csv$"),
                           full.names = TRUE))
        file.copy(f, file.path(d$anchors, basename(f)), overwrite = TRUE)
    }
    meta$timing[[tag]] <- as.numeric(difftime(Sys.time(), t1, units = "mins"))
    for (nm in c("panel_tbl_forecasted_res", "panel_tbl_forecasted_com",
                 "panel_tbl_forecasted_condo",
                 paste0("panel_tbl_forecasted_", BT_TRACK_KEYS())))
      if (exists(nm, envir = .GlobalEnv)) rm(list = nm, envir = .GlobalEnv)
    gc(verbose = FALSE)
    rm(ex)
  }

  meta$elapsed_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  saveRDS(meta, file.path(d$meta, paste0("origin_", origin, "_meta.rds")))
  if (!isTRUE(cfg$keep_work)) {
    bt_assert_scratch(d$root, cfg$out_dir)
    unlink(d$root, recursive = TRUE, force = TRUE)
    bt_log("scratch removed: ", d$root)
  }
  bt_log("origin ", origin, " done in ", round(meta$elapsed_min, 1), " min")
  invisible(meta)
}

# =============================================================================
# Scoring (parent process)
# =============================================================================
bt_load_actuals <- function(prod_cache) {
  av <- data.table::as.data.table(readRDS(file.path(prod_cache, "av_history_cln.rds")))
  av <- av[, .(parcel_id = bt_norm_id(parcel_id), tax_yr = as.integer(tax_yr),
               obs_land = as.numeric(appr_land_val),
               obs_imps = as.numeric(appr_imps_val))]
  av[, obs_total := fifelse(is.na(obs_land), 0, obs_land) +
                    fifelse(is.na(obs_imps), 0, obs_imps)]
  unique(av, by = c("parcel_id", "tax_yr"))
}

bt_metrics <- function(pred, obs) {
  ok <- is.finite(pred) & is.finite(obs) & pred > 0 & obs > 0
  pred <- pred[ok]; obs <- obs[ok]
  if (!length(pred))
    return(list(n = 0L, obs_sum = 0, RMSE_log = NA_real_, MAE_log = NA_real_,
                ME_log = NA_real_, WAPE = NA_real_, bias_pct = NA_real_))
  e <- log(pred) - log(obs)
  list(n = length(e), obs_sum = sum(obs),
       RMSE_log = sqrt(mean(e^2)), MAE_log = mean(abs(e)), ME_log = mean(e),
       WAPE = sum(abs(pred - obs)) / sum(obs),
       bias_pct = sum(pred) / sum(obs) - 1)
}

# Add track roll-ups: com = subgroups + com_other; all = everything.
bt_with_rollups <- function(pred) {
  keys <- BT_TRACK_KEYS()
  com <- pred[track %in% setdiff(keys, "condo")][, track := "com"]
  all <- data.table::copy(pred)[, track := "all"]
  data.table::rbindlist(list(pred, com, all), use.names = TRUE)
}

bt_score_errors <- function(pred) {
  cells <- list()
  for (comp in c("total", "land", "imps")) {
    pc <- paste0("pred_", comp); oc <- paste0("obs_", comp)
    for (pop in c("seed_observed", "all_scored")) {
      sub <- if (pop == "seed_observed") pred[seed_observed == TRUE] else pred
      m <- sub[, bt_metrics(get(pc), get(oc)),
               by = .(origin, horizon, tax_yr, track, anchored)]
      m[, `:=`(component = comp, pop = pop)]
      cells[[paste(comp, pop)]] <- m
    }
  }
  out <- data.table::rbindlist(cells)
  data.table::setcolorder(out, c("origin", "horizon", "tax_yr", "track", "anchored",
                                 "component", "pop"))
  out[order(component, pop, track, anchored, origin, horizon)]
}

bt_score_by_horizon <- function(pred) {
  cells <- list()
  for (comp in c("total", "land", "imps")) {
    pc <- paste0("pred_", comp); oc <- paste0("obs_", comp)
    for (pop in c("seed_observed", "all_scored")) {
      sub <- if (pop == "seed_observed") pred[seed_observed == TRUE] else pred
      m <- sub[, c(bt_metrics(get(pc), get(oc)),
                   list(n_origins = data.table::uniqueN(origin))),
               by = .(horizon, track, anchored)]
      m[, `:=`(component = comp, pop = pop)]
      cells[[paste(comp, pop)]] <- m
    }
  }
  out <- data.table::rbindlist(cells)
  # anchored - unanchored deltas on the same cell
  w <- data.table::dcast(out[, .(horizon, track, component, pop, anchored,
                                 RMSE_log, MAE_log, ME_log, WAPE, bias_pct)],
                         horizon + track + component + pop ~ anchored,
                         value.var = c("RMSE_log", "MAE_log", "ME_log", "WAPE", "bias_pct"))
  if (all(c("RMSE_log_TRUE", "RMSE_log_FALSE") %in% names(w))) {
    for (mm in c("RMSE_log", "MAE_log", "ME_log", "WAPE", "bias_pct"))
      w[, (paste0("d_", mm, "_anch_minus_unanch")) :=
          get(paste0(mm, "_TRUE")) - get(paste0(mm, "_FALSE"))]
    dcols <- c("horizon", "track", "component", "pop",
               grep("^d_", names(w), value = TRUE))
    out <- merge(out, w[, ..dcols], by = c("horizon", "track", "component", "pop"),
                 all.x = TRUE)
  }
  data.table::setcolorder(out, c("horizon", "track", "anchored", "component", "pop"))
  out[order(component, pop, track, anchored, horizon)]
}

# Matched-parcel year-over-year growth, the av_reconcile_certified.R
# definition: growth for year y over parcels with positive AV in both y-1
# and y, where AV is observed for <= T and predicted for > T.  The actual
# uses observed values on the same parcel set.  pop = seed_observed keeps
# only parcels with observed AV at T (the existing-parcel cohort);
# all_scored is the literal av_reconcile population.
bt_score_growth <- function(pred, obs, origins, final_year) {
  rows <- list()
  for (.T in origins) {
    p <- pred[origin == .T]
    if (!nrow(p)) next
    base <- obs[tax_yr == .T & obs_total > 0, .(parcel_id, obs_total)]
    for (pop in c("seed_observed", "all_scored")) {
      pp <- if (pop == "seed_observed") p[seed_observed == TRUE] else p
      for (anch in unique(pp$anchored)) {
        pa <- pp[anchored == anch]
        for (trk in unique(pa$track)) {
          pt <- pa[track == trk]
          ids <- unique(pt$parcel_id)
          # predicted path: observed at T, predicted after
          path_pred <- data.table::rbindlist(list(
            base[parcel_id %in% ids, .(parcel_id, tax_yr = .T, av = obs_total)],
            pt[, .(parcel_id, tax_yr, av = pred_total)]))
          path_obs <- data.table::rbindlist(list(
            base[parcel_id %in% ids, .(parcel_id, tax_yr = .T, av = obs_total)],
            pt[, .(parcel_id, tax_yr, av = obs_total)]))
          for (y in seq(.T + 1L, final_year)) {
            m <- merge(path_pred[tax_yr == y - 1L & av > 0, .(parcel_id, p0 = av)],
                       path_pred[tax_yr == y & av > 0, .(parcel_id, p1 = av)],
                       by = "parcel_id")
            m <- merge(m, path_obs[tax_yr == y - 1L & av > 0, .(parcel_id, o0 = av)],
                       by = "parcel_id")
            m <- merge(m, path_obs[tax_yr == y & av > 0, .(parcel_id, o1 = av)],
                       by = "parcel_id")
            cm <- merge(base[parcel_id %in% ids, .(parcel_id, b = obs_total)],
                        path_pred[tax_yr == y & av > 0, .(parcel_id, p1 = av)],
                        by = "parcel_id")
            cm <- merge(cm, path_obs[tax_yr == y & av > 0, .(parcel_id, o1 = av)],
                        by = "parcel_id")
            n_cohort <- if (pop == "seed_observed")
              length(intersect(ids, base$parcel_id)) else length(ids)
            g_pred <- if (nrow(m)) sum(m$p1) / sum(m$p0) - 1 else NA_real_
            g_act  <- if (nrow(m)) sum(m$o1) / sum(m$o0) - 1 else NA_real_
            rows[[length(rows) + 1L]] <- data.table(
              origin = .T, tax_yr = y, horizon = y - .T, track = trk,
              anchored = anch, pop = pop,
              g_pred = g_pred, g_act = g_act, err_pp = 100 * (g_pred - g_act),
              cum_pred = if (nrow(cm)) sum(cm$p1) / sum(cm$b) - 1 else NA_real_,
              cum_act  = if (nrow(cm)) sum(cm$o1) / sum(cm$b) - 1 else NA_real_,
              n_matched = nrow(m), n_cohort = n_cohort,
              coverage = if (n_cohort) nrow(cm) / n_cohort else NA_real_,
              av_base = if (nrow(m)) sum(m$o0) else NA_real_)
          }
        }
      }
    }
  }
  g <- data.table::rbindlist(rows)
  g[, cum_err_pp := 100 * (cum_pred - cum_act)]
  g[order(pop, track, anchored, origin, tax_yr)]
}

bt_growth_by_horizon <- function(g) {
  g[, .(n_origins = .N,
        mean_err_pp = mean(err_pp, na.rm = TRUE),
        mae_err_pp  = mean(abs(err_pp), na.rm = TRUE),
        mean_cum_err_pp = mean(cum_err_pp, na.rm = TRUE),
        mae_cum_err_pp  = mean(abs(cum_err_pp), na.rm = TRUE),
        mean_g_pred = mean(g_pred, na.rm = TRUE),
        mean_g_act  = mean(g_act, na.rm = TRUE)),
    by = .(horizon, track, anchored, pop)][order(pop, track, anchored, horizon)]
}

bt_seed_coverage <- function(pred) {
  pred[, .(n = .N, n_seed_observed = sum(seed_observed),
           share_rows_imputed_seed = mean(!seed_observed),
           share_av_imputed_seed =
             sum(fifelse(seed_observed, 0, obs_total), na.rm = TRUE) /
             sum(obs_total, na.rm = TRUE)),
       by = .(origin, horizon, track, anchored)][order(track, anchored, origin, horizon)]
}

bt_score <- function(out_dir, prod_cache, origins, final_year, scenario) {
  d <- bt_dirs(out_dir, origins[1])
  files <- list.files(d$predictions, pattern = "^pred_origin[0-9]+_(un)?anchored\\.rds$",
                      full.names = TRUE)
  files <- files[as.integer(sub("^pred_origin([0-9]+)_.*$", "\\1", basename(files))) %in% origins]
  if (!length(files)) stop("no prediction files under ", d$predictions)
  bt_log("scoring ", length(files), " prediction file(s)")
  obs <- bt_load_actuals(prod_cache)
  pred <- data.table::rbindlist(lapply(files, readRDS), use.names = TRUE)
  pred <- merge(pred, obs, by = c("parcel_id", "tax_yr"), all.x = TRUE)
  seed <- obs[, .(parcel_id, origin = tax_yr, seed_total = obs_total)]
  pred <- merge(pred, seed, by = c("parcel_id", "origin"), all.x = TRUE)
  pred[, seed_observed := !is.na(seed_total) & seed_total > 0]
  pred[, seed_total := NULL]
  # scored = has an observed positive total at the target year
  pred <- pred[!is.na(obs_total) & obs_total > 0]
  pred <- bt_with_rollups(pred)

  hdr <- bt_header_text(sort(unique(pred$origin)), final_year, scenario)
  errors_by_cell    <- bt_score_errors(pred)
  errors_by_horizon <- bt_score_by_horizon(pred)
  errors_by_origin  <- {
    cells <- list()
    for (pop in c("seed_observed", "all_scored")) {
      sub <- if (pop == "seed_observed") pred[seed_observed == TRUE] else pred
      m <- sub[, bt_metrics(pred_total, obs_total), by = .(origin, track, anchored)]
      m[, `:=`(component = "total", pop = pop)]
      cells[[pop]] <- m
    }
    data.table::rbindlist(cells)[order(pop, track, anchored, origin)]
  }
  growth <- bt_score_growth(pred, obs, sort(unique(pred$origin)), final_year)
  growth_h <- bt_growth_by_horizon(growth)
  seedcov <- bt_seed_coverage(pred)

  # meta: locf counts, anchor coverage, leaks
  metas <- lapply(list.files(d$meta, pattern = "_meta\\.rds$", full.names = TRUE), readRDS)
  metas <- metas[vapply(metas, function(m) m$origin %in% origins, logical(1))]
  locf <- data.table::rbindlist(lapply(metas, `[[`, "locf"), fill = TRUE)
  anch_com <- data.table::rbindlist(lapply(metas, function(m) m$anchor_coverage$com), fill = TRUE)
  anch_res <- data.table::rbindlist(lapply(metas, function(m) m$anchor_coverage$res), fill = TRUE)
  leaks <- data.table::rbindlist(lapply(metas, function(m)
    data.table::rbindlist(lapply(names(m$leaks), function(tag)
      data.table::copy(m$leaks[[tag]])[, `:=`(origin = m$origin, mode = tag)]), fill = TRUE)),
    fill = TRUE)

  bt_write_csv(errors_by_horizon, file.path(out_dir, "errors_by_horizon.csv"), hdr)
  bt_write_csv(errors_by_cell,    file.path(out_dir, "errors_by_cell.csv"), hdr)
  bt_write_csv(errors_by_origin,  file.path(out_dir, "errors_by_origin.csv"), hdr)
  bt_write_csv(growth,            file.path(out_dir, "growth_by_year.csv"), hdr)
  bt_write_csv(growth_h,          file.path(out_dir, "growth_by_horizon.csv"), hdr)
  bt_write_csv(seedcov,           file.path(out_dir, "seed_coverage.csv"), hdr)
  if (nrow(locf))     bt_write_csv(locf,     file.path(out_dir, "locf_backfill_counts.csv"), hdr)
  if (nrow(anch_com)) bt_write_csv(anch_com, file.path(out_dir, "anchor_coverage_com.csv"), hdr)
  if (nrow(anch_res)) bt_write_csv(anch_res, file.path(out_dir, "anchor_coverage_res.csv"), hdr)
  if (nrow(leaks))    bt_write_csv(leaks,    file.path(out_dir, "forecast_row_av_check.csv"), hdr)
  writeLines(hdr, file.path(out_dir, "README_backtest.txt"))

  invisible(list(errors_by_horizon = errors_by_horizon, errors_by_cell = errors_by_cell,
                 errors_by_origin = errors_by_origin, growth = growth,
                 growth_by_horizon = growth_h, seed_coverage = seedcov,
                 locf = locf, anchor_coverage_com = anch_com,
                 anchor_coverage_res = anch_res, leaks = leaks, pred_scored = pred))
}

# =============================================================================
# Smoke / bracket check on one origin's h = 1 citywide growth
# =============================================================================
bt_smoke_check <- function(growth, origin, min_coverage = 0.5, max_err_pp = 10) {
  .o <- as.integer(origin)
  g <- growth[origin == .o & horizon == 1L & track == "all" &
                pop == "seed_observed"]
  message("\n--- smoke check: origin ", origin, ", h = 1, track = all, ",
          "pop = seed_observed ---")
  if (!nrow(g)) { message("  no rows"); return(FALSE) }
  print(g[, .(anchored, g_pred = round(100 * g_pred, 2), g_act = round(100 * g_act, 2),
              err_pp = round(err_pp, 2), coverage = round(coverage, 3), n_matched)])
  ok <- all(g$coverage >= min_coverage, na.rm = TRUE) &&
        all(abs(g$err_pp) <= max_err_pp, na.rm = TRUE)
  lo <- min(g$g_pred); hi <- max(g$g_pred); act <- g$g_act[1]
  message("  observed growth ", round(100 * act, 2), "% | forecasts span ",
          round(100 * lo, 2), "% .. ", round(100 * hi, 2), "% ",
          if (nrow(g) > 1 && lo <= act && act <= hi) "(brackets observed)"
          else "(does not bracket observed)")
  message("  ", if (ok) "PASS" else "FAIL",
          " (coverage >= ", min_coverage, ", |err| <= ", max_err_pp, " pp)")
  ok
}

# =============================================================================
# Driver
# =============================================================================
run_backtest <- function(origins        = 2022:2025,
                         anchored       = c(TRUE, FALSE),
                         final_year     = 2026L,
                         scenario       = "baseline",
                         out_dir        = here::here("data", "outputs", "backtest"),
                         prod_cache_dir = if (exists("CFG", envir = .GlobalEnv))
                                            CFG$cache_dir
                                          else here::here("data", "cache"),
                         area_reports_root = here::here("data", "kca", "area_reports"),
                         stage_mode     = c("link", "copy"),
                         run_in_process = FALSE,
                         keep_work      = FALSE,
                         score_only     = FALSE,
                         smoke_stop     = TRUE,
                         smoke_min_coverage = 0.5,
                         smoke_max_err_pp   = 10) {
  stage_mode <- match.arg(stage_mode)
  origins <- sort(as.integer(origins), decreasing = TRUE)   # smoke origin first
  final_year <- as.integer(final_year)
  if (any(origins >= final_year)) stop("origins must be < final_year")
  anchored <- unique(as.logical(anchored))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  hdr <- bt_header_text(origins, final_year, scenario)
  message(paste(hdr, collapse = "\n"), "\n")
  writeLines(hdr, file.path(out_dir, "README_backtest.txt"))

  if (!score_only) {
    if (!exists("run_main_ml", envir = .GlobalEnv) ||
        !exists("retro_fill_av", envir = .GlobalEnv))
      stop("source main_ml.R with MAIN_ML_DEFINE_ONLY <- TRUE first")
    if (!dir.exists(prod_cache_dir)) stop("production cache not found: ", prod_cache_dir)
    if (any(anchored)) bt_check_area_reports(origins, area_reports_root)

    repo_root <- here::here()
    for (.T in origins) {
      cfg <- list(origin = .T, anchored = anchored, final_year = final_year,
                  scenario = scenario, out_dir = out_dir,
                  prod_cache = prod_cache_dir, area_reports_root = area_reports_root,
                  stage_mode = stage_mode, keep_work = keep_work,
                  repo_root = repo_root,
                  kca_date = if (exists("CFG", envir = .GlobalEnv))
                    CFG$kca_date_data_extracted else NULL)
      d <- bt_dirs(out_dir, .T)
      for (p in c(d$logs, d$meta, d$predictions)) dir.create(p, showWarnings = FALSE, recursive = TRUE)
      cfg_path <- file.path(d$meta, paste0("origin_", .T, "_config.rds"))
      saveRDS(cfg, cfg_path)
      log_path <- file.path(d$logs, paste0("origin_", .T, ".log"))

      if (run_in_process) {
        bt_log("origin ", .T, " in-process (log to console)")
        bt_run_origin(cfg)
        for (nm in setdiff(ls(envir = .GlobalEnv),
                           c("CFG", "COM_SUBGROUPS", "COM_SUBGROUP_KEYS",
                             "COM_PU_EXCLUDE", "run_main_ml", "retro_fill_av",
                             "prep_scenario_caches", "av_fcst_summary",
                             "MAIN_ML_DEFINE_ONLY",
                             ls(envir = .GlobalEnv, pattern = "^bt_|^BT_|^run_backtest$"))))
          rm(list = nm, envir = .GlobalEnv)
        gc(verbose = FALSE)
      } else {
        rscript <- file.path(R.home("bin"),
                             if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
        script <- file.path(repo_root, "scripts", "ml", "backtest_origin.R")
        bt_log("origin ", .T, ": launching child process; log -> ", log_path)
        status <- system2(rscript, c("--vanilla", shQuote(script), shQuote(cfg_path)),
                          stdout = log_path, stderr = log_path)
        if (!identical(as.integer(status), 0L))
          stop("origin ", .T, " failed (exit status ", status, "); see ", log_path)
        bt_log("origin ", .T, " finished; tail of log:")
        message(paste("    ", utils::tail(readLines(log_path, warn = FALSE), 8),
                      collapse = "\n"))
      }

      # smoke check after the first origin
      if (.T == origins[1] && length(origins) > 1) {
        sc <- bt_score(out_dir, prod_cache_dir, .T, final_year, scenario)
        ok <- bt_smoke_check(sc$growth, .T, smoke_min_coverage, smoke_max_err_pp)
        if (!ok && isTRUE(smoke_stop))
          stop("smoke check failed on origin ", .T,
               "; not starting the remaining origins (smoke_stop = TRUE). ",
               "Tables for origin ", .T, " are in ", out_dir)
      }
    }
  }

  sc <- bt_score(out_dir, prod_cache_dir, origins, final_year, scenario)
  if (length(origins) == 1)
    bt_smoke_check(sc$growth, origins[1], smoke_min_coverage, smoke_max_err_pp)

  message("\n=== errors by horizon (total AV, pop = seed_observed, track = all) ===")
  print(sc$errors_by_horizon[track == "all" & component == "total" & pop == "seed_observed",
                             .(horizon, anchored, n_origins, n, RMSE_log = round(RMSE_log, 4),
                               MAE_log = round(MAE_log, 4), ME_log = round(ME_log, 4),
                               WAPE = round(WAPE, 4), bias_pct = round(bias_pct, 4))])
  message("\n=== citywide growth error by horizon (pp, pop = seed_observed, track = all) ===")
  print(sc$growth_by_horizon[track == "all" & pop == "seed_observed",
                             .(horizon, anchored, n_origins,
                               mean_err_pp = round(mean_err_pp, 2),
                               mae_err_pp = round(mae_err_pp, 2),
                               mean_g_pred = round(100 * mean_g_pred, 2),
                               mean_g_act = round(100 * mean_g_act, 2))])
  message("\noutputs: ", out_dir)
  invisible(sc)
}

# =============================================================================
# bt_compare_caches(): §10 of the design — identical() check between two
# cache directories for the deterministic artefacts.
# =============================================================================
bt_compare_caches <- function(a, b,
                              patterns = c("^panel_tbl_retro_.*\\.rds$",
                                           "^model_data_.*\\.rds$",
                                           "^panel_tbl_.*_inputs_.*\\.rds$",
                                           "^panel_tbl_2006_2031_forecasted_.*\\.rds$")) {
  fa <- unlist(lapply(patterns, function(p) list.files(a, pattern = p)))
  fb <- unlist(lapply(patterns, function(p) list.files(b, pattern = p)))
  common <- intersect(fa, fb)
  out <- data.table::rbindlist(lapply(common, function(f) {
    x <- readRDS(file.path(a, f)); y <- readRDS(file.path(b, f))
    data.table(file = f, identical = identical(x, y),
               all_equal = isTRUE(all.equal(x, y, check.attributes = FALSE)))
  }))
  message("only in a: ", paste(setdiff(fa, fb), collapse = ", "))
  message("only in b: ", paste(setdiff(fb, fa), collapse = ", "))
  print(out)
  invisible(out)
}
