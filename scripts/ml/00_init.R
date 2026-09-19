# 00_init.R — global initialisation for run_main_ml()

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(here)
  library(data.table)
  library(lubridate)
  library(zoo)
  library(stringr)
  library(slider)
  library(caret)
  library(lightgbm)
  library(doParallel)
  library(foreach)
  library(sf)
  library(scales)
  library(readxl)
  library(arrow)
  library(forcats)
})

options(scipen = 999, datatable.print.nrows = 20)

if (!exists("kca_date_data_extracted"))
  stop("kca_date_data_extracted must be set in the driver before sourcing 00_init.R")

# ---- UNC-safe I/O helpers ---------------------------------------------------
unc_norm <- function(path) {
  if (.Platform$OS.type == "windows") gsub("/", "\\\\", path) else path
}

safe_write_csv <- function(x, path, ...) {
  path <- unc_norm(path)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  readr::write_csv(x, con, ...)
}

safe_fwrite <- function(x, file, ...) {
  file <- unc_norm(file)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(x, file = file, ...)
}

safe_saveRDS <- function(object, file, ...) {
  file <- unc_norm(file)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(object, file = file, ...)
}

safe_fread <- function(file, ...) {
  file <- unc_norm(file)
  data.table::fread(file = file, ...)
}

# ---- Parallel cluster -------------------------------------------------------
init_parallel <- function(cores = max(1L, parallel::detectCores() - 1L)) {
  cl <- parallel::makePSOCKcluster(max(1L, cores))
  doParallel::registerDoParallel(cl)
  message("Parallel backend registered with ", cores, " cores.")
  invisible(cl)
}

# ---- Constants --------------------------------------------------------------
levy_code_list <- c("0010","0011","0013","0014","0016","0025","0030","0032")
tax_stat_list  <- c("T", "O")

# ---- Lookup table -----------------------------------------------------------
lookup <- readr::read_csv(
  here("data", "kca", kca_date_data_extracted, "EXTR_LookUp.csv"),
  show_col_types = FALSE
)

# ---- Lookup decoding helpers ------------------------------------------------
decode_lookup_column <- function(data, lookup_tbl, col_name, lu_type, suffix = "_desc") {
  new_col <- paste0(col_name, suffix)
  lu_map <- lookup_tbl %>%
    dplyr::filter(LUType == lu_type) %>%
    dplyr::transmute(LUItem = as.character(LUItem), LUDescription = as.character(LUDescription))
  data %>%
    dplyr::mutate("{col_name}" := as.character(.data[[col_name]])) %>%
    dplyr::left_join(lu_map, by = setNames("LUItem", col_name)) %>%
    dplyr::mutate("{new_col}" := LUDescription) %>%
    dplyr::select(-LUDescription)
}

decode_lookup_columns <- function(data, lookup_tbl, cols, lu_types, suffix = "_desc") {
  stopifnot(length(cols) == length(lu_types))
  out <- data
  for (i in seq_along(cols))
    out <- decode_lookup_column(out, lookup_tbl, cols[i], lu_types[i], suffix)
  out
}

# ---- Helpers ----------------------------------------------------------------
stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

attr_map <- data.table(
  attribute = c(
    "area","currentzoning","hbuasifvacantdesc","unbuildable","sqftlot","nuisancescore",
    "mtrainier","olympics","cascades","territorial","seattleskyline","pugetsound","lakewashington",
    "seismichazard","landslidehazard","steepslopehazard",
    "trafficnoise","airportnoise","powerlines","othernuisances",
    "contamination","historicsite","log_appr_land_val_lag1"
  ),
  col_name = c(
    "area","current_zoning_3","hbu_as_if_vacant_desc","unbuildable","sq_ft_lot","nuisance_score",
    "mt_rainier","olympics","cascades","territorial","seattle_skyline","puget_sound","lake_washington",
    "seismic_hazard","landslide_hazard","steep_slope_hazard",
    "traffic_noise","airport_noise","power_lines","other_nuisances",
    "contamination","historic_site","log_appr_land_val_lag1"
  )
)
attr_map_noupdate <- attr_map[attribute != "area"]

coerce_to_target <- function(x, target_vec) {
  if (is.numeric(target_vec)) return(as.numeric(x))
  if (is.logical(target_vec)) return(toupper(trimws(x)) %in% c("Y","YES","TRUE","1"))
  as.character(x)
}

apply_facet_changes <- function(panel_dt, chg_long, facet_name) {
  subc <- chg_long[col_name == facet_name, .(parcel_id, eff_yr, new_val = attribute_value)]
  if (nrow(subc) == 0) return(panel_dt)
  setorder(subc, parcel_id, eff_yr)
  subc <- subc[, .SD[.N], by = .(parcel_id, eff_yr)]
  setkey(subc, parcel_id, eff_yr)
  setkey(panel_dt, parcel_id, tax_yr)
  panel_dt <- merge(panel_dt, subc,
                    by.x = c("parcel_id","tax_yr"), by.y = c("parcel_id","eff_yr"),
                    all.x = TRUE)
  panel_dt[!is.na(new_val), (facet_name) := new_val]
  panel_dt[, new_val := NULL]
  setorder(panel_dt, parcel_id, tax_yr)
  panel_dt[, (facet_name) := zoo::na.locf(get(facet_name), na.rm = FALSE), by = parcel_id]
  panel_dt
}

Mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---- LightGBM helpers -------------------------------------------------------
train_lgbm_log_model <- function(df, outcome, nfold = 5, seed = 123,
                                 folds = NULL,
                                 params = list(
                                   objective = "regression", metric = "rmse",
                                   learning_rate = 0.05, num_leaves = 63,
                                   feature_fraction = 0.8, bagging_fraction = 0.8,
                                   bagging_freq = 1, min_data_in_leaf = 50
                                 ),
                                 nrounds = 2000, early_stopping_rounds = 50) {
  set.seed(seed)
  form <- as.formula(paste0(outcome, " ~ ."))
  X <- model.matrix(form, data = df)[, -1, drop = FALSE]
  y <- df[[outcome]]
  if (is.null(folds)) {
    folds <- caret::createFolds(y, k = nfold, list = TRUE, returnTrain = FALSE)
  } else {
    folds <- lapply(folds, as.integer)
  }
  rmse_folds <- numeric(length(folds))
  best_iters <- integer(length(folds))
  for (i in seq_along(folds)) {
    idx_val <- folds[[i]]
    idx_tr  <- setdiff(seq_len(nrow(X)), idx_val)
    dtrain  <- lgb.Dataset(data = X[idx_tr,  , drop = FALSE], label = y[idx_tr])
    dval    <- lgb.Dataset(data = X[idx_val, , drop = FALSE], label = y[idx_val])
    bst <- lgb.train(params = params, data = dtrain, nrounds = nrounds,
                     valids = list(valid = dval),
                     early_stopping_rounds = early_stopping_rounds, verbose = -1)
    pred_val      <- predict(bst, X[idx_val, , drop = FALSE])
    rmse_folds[i] <- sqrt(mean((y[idx_val] - pred_val)^2, na.rm = TRUE))
    best_iters[i] <- bst$best_iter
  }
  cv_rmse        <- mean(rmse_folds)
  best_iter_final <- round(mean(best_iters))
  dtrain_all     <- lgb.Dataset(data = X, label = y)
  final_bst <- lgb.train(params = params, data = dtrain_all,
                          nrounds = best_iter_final, verbose = -1)
  list(model = final_bst, cv_rmse = cv_rmse, best_iter = best_iter_final,
       x_cols = colnames(X), outcome = outcome, folds_used = folds)
}

make_rolling_year_folds <- function(tax_yr_vec, n_folds = 5L, min_train_yrs = 3L) {
  years <- sort(unique(tax_yr_vec))
  n_yrs <- length(years)
  if (n_yrs < min_train_yrs + 1L) {
    warning("make_rolling_year_folds: only ", n_yrs, " distinct years — returning NULL")
    return(NULL)
  }
  first_cut <- min_train_yrs
  last_cut  <- n_yrs - 1L
  if (first_cut >= last_cut) {
    n_folds <- max(1L, last_cut - first_cut + 1L)
  }
  cut_indices <- unique(round(seq(first_cut, last_cut, length.out = n_folds)))
  folds <- vector("list", length(cut_indices))
  for (i in seq_along(cut_indices)) {
    cutpoint_yr <- years[cut_indices[i]]
    val_rows <- which(tax_yr_vec > cutpoint_yr)
    if (length(val_rows) >= 1L) folds[[i]] <- val_rows
  }
  folds <- Filter(Negate(is.null), folds)
  if (length(folds) == 0L) { warning("make_rolling_year_folds: no valid folds"); return(NULL) }
  names(folds) <- paste0("fold_", seq_along(folds))
  folds
}

predict_lgbm_log <- function(lgb_model, dv, feature_names, newdata) {
  X_new <- as.matrix(predict(dv, newdata))
  missing_cols <- setdiff(feature_names, colnames(X_new))
  if (length(missing_cols) > 0)
    X_new <- cbind(X_new, matrix(0, nrow(X_new), length(missing_cols),
                                  dimnames = list(NULL, missing_cols)))
  as.numeric(predict(lgb_model, X_new[, feature_names, drop = FALSE]))
}

rebuild_dv_clean <- function(train_df, outcome, full_rank = TRUE, max_levels = 200) {
  stopifnot(outcome %in% names(train_df))
  x <- train_df[, setdiff(names(train_df), outcome), drop = FALSE]
  drop_cols <- names(x)[sapply(x, function(col) is.factor(col) && length(levels(col)) > max_levels)]
  if (length(drop_cols) > 0) {
    message("Dropping high-cardinality predictors: ", paste(drop_cols, collapse = ", "))
    x <- x[, setdiff(names(x), drop_cols), drop = FALSE]
  }
  for (cn in names(x)) if (is.character(x[[cn]]) || is.logical(x[[cn]])) x[[cn]] <- factor(x[[cn]])
  for (cn in names(x)) if (is.factor(x[[cn]]) && length(levels(x[[cn]])) < 2)
    levels(x[[cn]]) <- c(levels(x[[cn]]), "__DUMMY__")
  caret::dummyVars(~ ., data = x, fullRank = full_rank)
}

prep_for_dummyvars <- function(newdata, dv_obj, train_df = NULL) {
  nd <- as.data.frame(newdata)
  missing <- setdiff(dv_obj$vars, names(nd))
  for (m in missing) nd[[m]] <- NA
  for (v in names(dv_obj$lvls)) {
    if (!v %in% names(nd)) next
    lvls <- dv_obj$lvls[[v]]
    x <- as.character(nd[[v]])
    x[!x %in% lvls] <- NA_character_
    nd[[v]] <- factor(x, levels = lvls)
  }
  if (!is.null(train_df)) {
    lvls_names <- if (!is.null(dv_obj$lvls)) names(dv_obj$lvls) else character(0)
    other_cat  <- setdiff(names(nd), lvls_names)
    other_cat  <- other_cat[sapply(nd[, other_cat, drop=FALSE],
                                   function(x) is.character(x) || is.logical(x))]
    for (v in other_cat) {
      if (!v %in% names(train_df)) next
      tr_lvls <- sort(unique(na.omit(as.character(train_df[[v]]))))
      if (length(tr_lvls) < 2) tr_lvls <- c(tr_lvls, "__DUMMY__")
      x <- as.character(nd[[v]])
      x[!x %in% tr_lvls] <- NA_character_
      nd[[v]] <- factor(x, levels = tr_lvls)
    }
  }
  nd
}

# ---- Res/com partition assertion --------------------------------------------
# Shared by both panel combiners (xx_combine_res_comm_panel.R and
# xx_combine_res_comm_condo_panel.R) so the two cannot drift apart.
#
# The point of the PropType split is that a parcel is forecast exactly once;
# if that breaks, every downstream citywide total double-counts and the
# failure is otherwise silent.
#
#   A. zero parcel_id overlap between res and com          -> HARD STOP
#   B1. every panel parcel is of the expected PropType     -> HARD STOP
#   B2. res + com counts reach the Seattle levy-code
#       population of EXTR_Parcel, by PropType             -> REPORTED
#
# A and B1 are correctness violations: they mean a filter leaked, which is the
# class of bug this partition exists to prevent. B2 is attrition -- parcels
# that never reached the panel, most often dropped upstream for want of AV
# history -- which is worth knowing about but is not a reason to kill a run.
# The missing ids are left in res_com_reconcile_gap for inspection.
#
# Condo units are reported but NOT added into B: condo UNITS are not rows in
# EXTR_Parcel (the complex Major is), so they are not part of that population
# and including them would guarantee a false mismatch.
assert_res_com_partition <- function(panel_res, panel_com, panel_condo = NULL,
                                     levy_codes = NULL, kca_date = NULL) {
  nd <- function(x) gsub("-", "", as.character(x), fixed = TRUE)
  res_ids   <- unique(nd(panel_res$parcel_id))
  com_ids   <- unique(nd(panel_com$parcel_id))
  condo_ids <- if (is.null(panel_condo)) character(0)
               else unique(nd(panel_condo$parcel_id))

  # ---- A. overlap ------------------------------------------------------------
  dup_ids <- intersect(res_ids, com_ids)
  message("  [assert] res parcels: ", length(res_ids),
          " | com parcels: ", length(com_ids),
          " | condo units: ", length(condo_ids))
  if (length(dup_ids) > 0)
    stop("Res/com parcel duplication: ", length(dup_ids),
         " parcel_id(s) are in BOTH panels and would be forecast twice. ",
         "First 10: ", paste(utils::head(dup_ids, 10), collapse = ", "),
         "\n  Check the PropType filters in 02_transfrm.R and ",
         "01_import_comm.R — they must partition EXTR_Parcel, not overlap.")
  message("  [assert] res/com parcel_id overlap: 0 — OK")

  # ---- B. reconciliation to the Seattle levy-code population -----------------
  if (is.null(levy_codes))
    levy_codes <- get0("levy_code_list", envir = .GlobalEnv)
  if (is.null(kca_date))
    kca_date <- get0("kca_date_data_extracted", envir = .GlobalEnv)
  pp <- if (is.null(kca_date)) NA_character_
        else here::here("data", "kca", kca_date, "EXTR_Parcel.csv")

  # Not a soft failure: 01_import_comm.R reads this same file to build the
  # commercial universe, so if it is unreachable here the run is already
  # inconsistent and the reconciliation must not be quietly skipped.
  if (is.null(levy_codes) || is.na(pp) || !file.exists(pp))
    stop("Cannot reconcile panel counts: levy_code_list or EXTR_Parcel.csv ",
         "unavailable (looked for: ", pp, "). The res/com partition is ",
         "therefore unverified — refusing to continue.")

  xp <- data.table::fread(pp, encoding = "Latin-1")
  data.table::setnames(xp, names(xp), tolower(names(xp)))
  xp[, .pid  := paste0(stringr::str_pad(trimws(as.character(major)), 6, "left", "0"),
                       stringr::str_pad(trimws(as.character(minor)), 4, "left", "0"))]
  xp[, .levy := stringr::str_pad(trimws(as.character(levycode)), 4, "left", "0")]
  xp[, .pt   := toupper(trimws(as.character(proptype)))]
  sea <- unique(xp[.levy %chin% levy_codes], by = ".pid")
  exp_r <- sea[.pt == "R", .N]
  exp_c <- sea[.pt == "C", .N]

  message("  [assert] Seattle levy-code EXTR_Parcel population: ",
          nrow(sea), " parcels (PropType R ", exp_r, " | C ", exp_c, ")")

  # Panel rows must be a SUBSET of the expected class: anything else means a
  # filter let the wrong PropType through.
  stray_r <- setdiff(res_ids, sea[.pt == "R", .pid])
  stray_c <- setdiff(com_ids, sea[.pt == "C", .pid])
  if (length(stray_r) > 0 || length(stray_c) > 0)
    stop("Panel/PropType mismatch: ", length(stray_r),
         " residential parcel(s) are not Seattle PropType R and ",
         length(stray_c), " commercial parcel(s) are not Seattle PropType C.",
         "\n  res e.g.: ", paste(utils::head(stray_r, 5), collapse = ", "),
         "\n  com e.g.: ", paste(utils::head(stray_c, 5), collapse = ", "))

  # B1 above guarantees the panels are subsets of the expected class, so the
  # shortfall is exactly the set difference.
  miss_r_ids <- setdiff(sea[.pt == "R", .pid], res_ids)
  miss_c_ids <- setdiff(sea[.pt == "C", .pid], com_ids)
  message("  [assert] res ", length(res_ids), " / ", exp_r,
          " (missing ", length(miss_r_ids), ") | com ", length(com_ids),
          " / ", exp_c, " (missing ", length(miss_c_ids), ")")

  gap <- list(
    expected_res = exp_r,            expected_com = exp_c,
    panel_res    = length(res_ids),  panel_com    = length(com_ids),
    res_missing  = miss_r_ids,       com_missing  = miss_c_ids
  )
  assign("res_com_reconcile_gap", gap, envir = .GlobalEnv)

  if (length(miss_r_ids) > 0L || length(miss_c_ids) > 0L) {
    message("  [assert] GAP (reported, not fatal): ", length(miss_r_ids),
            " of ", exp_r, " PropType R and ", length(miss_c_ids), " of ",
            exp_c, " PropType C Seattle parcels never reached the panel.")
    if (length(miss_r_ids) > 0L)
      message("    res e.g.: ",
              paste(utils::head(miss_r_ids, 5), collapse = ", "))
    if (length(miss_c_ids) > 0L)
      message("    com e.g.: ",
              paste(utils::head(miss_c_ids, 5), collapse = ", "))
    message("    Most likely dropped by the AV-history join in ",
            "xx_combine_parcel_history_changes.R or by a transform filter. ",
            "Full id vectors: res_com_reconcile_gap")
    warning("Panel counts do not reach the Seattle levy-code population: ",
            "res short ", length(miss_r_ids), ", com short ",
            length(miss_c_ids), ". Not fatal — see res_com_reconcile_gap.",
            call. = FALSE)
  } else {
    message("  [assert] res + com reconcile to the Seattle population — OK")
  }
  invisible(gap)
}

message("00_init.R loaded. the cake is still a lie.")
