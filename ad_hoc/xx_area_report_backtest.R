# ============================================================================
# xx_area_report_backtest.R
# Backtest: TY2027 model growth vs. KCA 2026 area revalue reports
# ============================================================================
#
# Question: in the first forecast year (TY2026 -> TY2027), where did the AV ML
# pipeline land close to the assessor's published revalue results, where did
# it miss, and how many dollars does each miss carry?
#
# Tax-year convention: TY2027 = assessed Jan 1 2026 = the 2026 assessment-year
# area reports (data/kca/area_reports/2026).
#
# ---------------------------------------------------------------------------
# COMPARISON MODES
#   pure_model   Forecast panels from a run with use_area_actuals = FALSE.
#                The only true out-of-sample test for areas that have reports.
#   model_rows   Anchored (production) run, restricted to parcels whose TY2027
#                rate came from the model: specialty parcels, and any parcels
#                in reported areas that did not pick up a report rate.
#   anchor_qa    Anchored run, parcels whose TY2027 rate came from a report.
#                These should reproduce the published rate almost exactly;
#                a gap here is an anchoring-mechanics bug, not model error.
#
# Anchored vs model rows: the res panel has no rate-source column and the com
# panel leaves model rows blank, so a parcel counts as anchored when it is
# declared "report" OR its TY2027 value equals TY2026 x (1 + its cell's report
# rate) within anchor_tol_*. Reports downloaded after the production run
# therefore land in model_rows automatically: they are true out-of-sample cells.
#
# Condo cells: one per Specialty 700 report (area_report_import.R with the condo
# parser), covering the neighborhoods in its nbhds column. The panel column that
# holds the condo neighborhood is picked by overlap with those codes and printed;
# override with CFG_BT[['panel_cols']][['condo_nbhd']]. Condo reports carry land,
# improvements and total, and a unit count (count_ratio = panel units / report units).
#
# Coverage check: each cell carries coverage_ratio (res: panel mean AV per
# parcel / report mean AV; com: panel matched AV / report total AV). Geo cells
# outside 0.67-1.5x are flagged; those areas are not being compared like for like.
#
# Without an unanchored run, the geographic cells are mostly anchored, so the
# model_rows view is thin. For the full test:
#
#   BT_DEFINE_ONLY <- TRUE
#   source(here::here("scripts", "ml", "xx_area_report_backtest.R"))
#   bt_stash_panels(here::here("data", "backtest", "anchored"))    # keep production panels
#   run_main_ml(use_area_actuals = FALSE, ...)                     # same args as production otherwise
#   bt_stash_panels(here::here("data", "backtest", "unanchored"))
#   bt_stash_panels(here::here("data", "cache"),                   # restore production panels
#                   src = here::here("data", "backtest", "anchored"))
#   run_area_backtest()
#
# Both runs write the same cache file names, which is why the stash/restore
# steps matter. The backtest folders sit outside data/cache so the recursive
# cache searches in other scripts never pick them up.
#
# ---------------------------------------------------------------------------
# GROWTH DEFINITION
#   Matched-parcel growth (parcels with AV > 0 in both years), the same
#   definition av_reconcile_certified.R uses, so new construction and the
#   TY2026 coverage gap do not contaminate the rates.
#   Commercial geographic cells use non-specialty parcels only, matching
#   geo_actuals_scope = "nonspecialty" and the area report total-AV tables.
#   Specialty reports are countywide; the panel is Seattle-only. Read those
#   comparisons as direction and magnitude checks, not exact ties.
#
# Contains no dollar-sign characters (regex ends use \\z), so it survives
# copy/paste through chat tools that strip them.
#
# OUTPUTS (data/outputs/area_backtest/)
#   area_backtest_cells.csv          one row per report cell x mode (primary scenario)
#   area_backtest_cells_all_scen.csv every scenario
#   area_backtest_summary.csv        value-weighted accuracy by mode x cell type x component
#   area_backtest_dollars.csv        dollar miss and AV coverage by mode x cell type
#   area_backtest_citywide.csv       matched TY2027 growth by run x scenario x track
#   area_backtest_unmatched_reports.csv   report cells with no matching parcels
#   area_backtest_actuals_duplicates.csv  duplicate report keys (first kept)
#   01_pred_vs_actual.png            model vs actual, every mode
#   02_cell_dumbbell.png             actual -> model by cell, scenario range
#   03_dollar_miss.png               dollars attributable to each cell's miss
#   04_component_error.png           land / improvements / total
#   05_accuracy_summary.png          value-weighted bias and MAE
#   06_citywide_context.png          track-level TY2027 growth vs worksheet
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(here)
})

# ── CONFIG ──────────────────────────────────────────────────────────────────

CFG_BT <- list(
  report_year      = 2026L,
  base_yr          = 2026L,
  target_yr        = 2027L,
  scenarios        = c("baseline", "optimistic", "pessimistic"),
  primary_scenario = "baseline",

  cache_dir        = here("data", "cache"),
  anchored_dir     = here("data", "cache"),                      # or data/backtest/anchored
  unanchored_dir   = here("data", "backtest", "unanchored"),     # optional
  panel_pattern    = "panel_tbl_2006_2031_forecasted_%s_%s.rds", # scenario, track
  tracks_cells     = c("res", "com", "condo"),
  tracks_context   = c("res", "com", "condo"),

  actuals_path     = NULL,   # NULL: area_report_actuals in .GlobalEnv, else newest cached rds
  output_dir       = here("data", "outputs", "area_backtest"),

  res_basis        = "population",   # res reports carry Sales and Population rows
  com_geo_scope    = "nonspecialty", # "nonspecialty" or "all"

  # Prelim levy worksheet 09.10.2026 vs 2026 certified table, excluding new
  # construction (line B) and state-assessed value (line D):
  # (295.701 - 1.106 - 1.538) / (308.778 - 1.538) - 1. Includes personal property.
  worksheet_existing_growth = -0.0462,

  label_top_n      = 12L,
  anchor_tol_rate  = 0.0005,  # parcel counts as anchored if within 0.05pp of its report rate
  anchor_tol_usd   = 1000,    # ...or within 1,000 dollars (assessor rounding)
  anchor_cell_share = 0.9,    # ...and at least this share of its cell hits the rate too
  min_cell_parcels = 3L,      # cells smaller than this, or below min_cell_av, stay in the CSV
  min_cell_av      = 25e6,    #   but are left out of summaries and charts (single-parcel noise)

  # KCA EXTR_Parcel.csv: supplies the condo neighborhood (SpecSubArea where
  # SpecArea = 700, joined on Major) and the PropType / SpecArea / PresentUse
  # breakdown for flagged residential cells. NULL = search data/kca, then data/.
  extr_parcel_path = NULL,
  pct_units        = "auto",  # "auto", "percent", or "fraction"

  # Column overrides for area_report_actuals. NULL = auto-detect.
  # If the printed mapping is wrong, set the exact name here.
  act_cols = list(
    area = NULL, prop_type = NULL, report_kind = NULL, basis = NULL,
    spec_area = NULL, spec_sub = NULL, year = NULL,
    pct_total = NULL, pct_land = NULL, pct_imps = NULL,
    prev_total = NULL, cur_total = NULL,
    prev_land = NULL, cur_land = NULL,
    prev_imps = NULL, cur_imps = NULL
  ),

  # Column overrides for the forecast panels. NULL = auto-detect.
  # condo_nbhd: panel column holding the condo neighborhood (35, 40, ...). NULL =
  # pick the candidate column that best overlaps the report neighborhoods.
  panel_cols = list(area = NULL, spec_area = NULL, spec_sub = NULL, rate_source = NULL,
                    condo_nbhd = NULL)
)

MODE_LABELS <- c(
  pure_model     = "Pure model (unanchored run)",
  model_rows     = "Model-sourced parcels (anchored run)",
  anchored_mixed = "Anchored run (rate source unknown)",
  anchor_qa      = "Anchored parcels (QA)"
)

TYPE_COLOURS <- c(
  "Residential area"     = "#27ae60",
  "Condo area"           = "#2980b9",
  "Commercial area"      = "#e67e22",
  "Commercial specialty" = "#8e44ad"
)

# ── 0. HELPERS ──────────────────────────────────────────────────────────────

pick_col <- function(nms, override = NULL, patterns = character(), exclude = character()) {
  if (!is.null(override)) {
    if (!override %in% nms) stop("Column override not found in data: ", override)
    return(override)
  }
  cand <- setdiff(nms, exclude)
  for (p in patterns) {
    hit <- grep(p, cand, value = TRUE, ignore.case = TRUE, perl = TRUE)
    if (length(hit)) return(hit[1])
  }
  NA_character_
}

col_num <- function(dt, col) {
  if (is.na(col) || !col %in% names(dt)) return(rep(NA_real_, nrow(dt)))
  suppressWarnings(as.numeric(dt[[col]]))
}

col_chr <- function(dt, col) {
  if (is.na(col) || !col %in% names(dt)) return(rep(NA_character_, nrow(dt)))
  as.character(dt[[col]])
}

to_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[%,+[:space:]]", "", as.character(x))))
}

# "030", "30", 30 and 30.0 all become "30"; blanks and zero become NA
norm_key <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "0")] <- NA_character_
  num <- suppressWarnings(as.numeric(x))
  fifelse(!is.na(num), as.character(num), x)
}

pos_na <- function(x) fifelse(!is.na(x) & x > 0, x, NA_real_)

# Left-pad all-digit ids with zeros ("160" -> "000160")
pad0 <- function(x, w) {
  x <- trimws(as.character(x))
  short <- !is.na(x) & nchar(x) < w & grepl("^[0-9]+\\z", x, perl = TRUE)
  x[short] <- paste0(strrep("0", w - nchar(x[short])), x[short])
  x
}

load_extr_parcel <- function() {
  path <- CFG_BT[["extr_parcel_path"]]
  if (is.null(path)) {
    for (d in c(here("data", "kca"), here("data"))) {
      if (!dir.exists(d)) next
      hits <- list.files(d, pattern = "^EXTR_Parcel.*[.]csv", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
      hits <- hits[!grepl("backtest", hits, fixed = TRUE)]
      if (length(hits)) {
        path <- hits[order(file.info(hits)[["mtime"]], decreasing = TRUE)][1]
        break
      }
    }
  }
  if (is.null(path) || !file.exists(path)) {
    message("  EXTR_Parcel.csv not found (set CFG_BT[['extr_parcel_path']]); ",
            "condo neighborhoods and res composition will be skipped.")
    return(NULL)
  }
  hdr  <- names(fread(path, nrows = 0))
  want <- c("Major", "Minor", "PropType", "Area", "SubArea", "SpecArea", "SpecSubArea", "PresentUse")
  have <- hdr[tolower(hdr) %in% tolower(want)]
  xp <- fread(path, select = have, colClasses = "character", showProgress = FALSE)
  setnames(xp, have, want[match(tolower(have), tolower(want))])
  for (cn in setdiff(want, names(xp))) set(xp, j = cn, value = NA_character_)
  xp[, `:=`(Major = pad0(Major, 6), Minor = pad0(Minor, 4))]
  xp[, pid := paste0(Major, Minor)]
  xp[, SpecArea := norm_key(SpecArea)]
  xp[, SpecSubArea := norm_key(SpecSubArea)]
  message(sprintf("  EXTR_Parcel: %s rows from %s", format(nrow(xp), big.mark = ","), path))
  xp
}

ratio_g <- function(a0, a1) {
  ok <- !is.na(a0) & !is.na(a1)
  if (!any(ok)) return(NA_real_)
  s0 <- sum(a0[ok])
  if (s0 <= 0) return(NA_real_)
  sum(a1[ok]) / s0 - 1
}

out_path <- function(f) file.path(CFG_BT[["output_dir"]], f)

bt_theme <- function() {
  base <- theme_minimal(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          plot.title.position = "plot",
          plot.title         = element_text(face = "bold"),
          legend.position    = "bottom",
          strip.text         = element_text(face = "bold"))
  if (exists("theme_av", envir = .GlobalEnv)) {
    ta <- get("theme_av", envir = .GlobalEnv)
    out <- tryCatch(if (is.function(ta)) ta() else ta, error = function(e) NULL)
    if (inherits(out, "theme")) return(out + theme(legend.position = "bottom"))
  }
  base
}

bt_save <- function(plot, file, width, height) {
  ggsave(out_path(file), plot, width = width, height = min(height, 45),
         dpi = 150, bg = "white", limitsize = FALSE)
  message("  Wrote ", file)
}

# Copy forecasted panels between folders (stash before an unanchored run,
# restore afterwards). src defaults to the live cache.
bt_stash_panels <- function(dest, src = CFG_BT[["cache_dir"]], overwrite = TRUE) {
  fs <- list.files(src, pattern = "^panel_tbl_.*_forecasted_.*[.]rds", full.names = TRUE)
  if (!length(fs)) stop("No forecasted panels found in ", src)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  ok <- file.copy(fs, dest, overwrite = overwrite, copy.date = TRUE)
  message("Copied ", sum(ok), " of ", length(fs), " panels: ", src, " -> ", dest)
  invisible(fs[ok])
}

# ── 1. AREA REPORT ACTUALS ──────────────────────────────────────────────────

as_actuals_dt <- function(obj) {
  if (is.data.frame(obj)) return(as.data.table(obj))
  if (is.list(obj)) {
    if ("area_report_actuals" %in% names(obj)) return(as.data.table(obj[["area_report_actuals"]]))
    dfs <- Filter(is.data.frame, obj)
    if (length(dfs)) return(as.data.table(dfs[[1]]))
  }
  stop("Could not find a data frame of area report actuals in the cached object.")
}

load_actuals_raw <- function() {
  if (!is.null(CFG_BT[["actuals_path"]])) {
    message("  Actuals: ", CFG_BT[["actuals_path"]])
    return(as_actuals_dt(readRDS(CFG_BT[["actuals_path"]])))
  }
  if (exists("area_report_actuals", envir = .GlobalEnv)) {
    message("  Actuals: area_report_actuals from .GlobalEnv")
    return(copy(as.data.table(get("area_report_actuals", envir = .GlobalEnv))))
  }
  hits <- list.files(here("data"), pattern = "area_report.*[.]rds", recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  hits <- hits[!grepl("backtest", hits, fixed = TRUE)]
  if (!length(hits)) {
    stop("area_report_actuals not found. Run Step 0 (area_report_import.R), ",
         "or set CFG_BT[['actuals_path']].")
  }
  yr_hit <- hits[grepl(as.character(CFG_BT[["report_year"]]), basename(hits))]
  if (length(yr_hit)) hits <- yr_hit
  hits <- hits[order(file.info(hits)[["mtime"]], decreasing = TRUE)]
  message("  Actuals: ", hits[1])
  as_actuals_dt(readRDS(hits[1]))
}

std_actuals <- function(raw) {
  nms <- names(raw)
  ov  <- CFG_BT[["act_cols"]]
  lvl <- function(side, comp) {
    s <- if (side == "prev") "(prev|prior|old|base|previous)" else "(cur|curr|current|new)"
    c(sprintf("^%s_?%s", s, comp), sprintf("%s_?%s\\z", comp, s))
  }

  col <- list()
  col[["area"]]        <- pick_col(nms, ov[["area"]],        c("^area\\z", "^geo_area\\z", "^area_(num|no|id)\\z"))
  col[["prop_type"]]   <- pick_col(nms, ov[["prop_type"]],   c("^prop_type\\z", "^track\\z", "^property_type\\z"))
  col[["report_kind"]] <- pick_col(nms, ov[["report_kind"]], c("^report_kind\\z", "^kind\\z"))
  col[["basis"]]       <- pick_col(nms, ov[["basis"]],       c("^basis\\z"))
  col[["spec_area"]]   <- pick_col(nms, ov[["spec_area"]],   c("^spec_area\\z", "^specialty(_area)?\\z"))
  col[["spec_sub"]]    <- pick_col(nms, ov[["spec_sub"]],    c("^spec_sub", "^specialty_sub"))
  col[["year"]]        <- pick_col(nms, ov[["year"]],
                                   c("^area_reports?_year\\z", "^report_year\\z",
                                     "^assess(ment)?_(yr|year)\\z", "^year\\z"))
  col[["pct_land"]]    <- pick_col(nms, ov[["pct_land"]],
                                   c("land.*pct|pct.*land", "land.*(growth|rate)|(growth|rate).*land"))
  col[["pct_imps"]]    <- pick_col(nms, ov[["pct_imps"]],
                                   c("imp.*pct|pct.*imp", "imp.*(growth|rate)|(growth|rate).*imp"))
  col[["pct_total"]]   <- pick_col(nms, ov[["pct_total"]],
                                   c("tot.*pct|pct.*tot", "tot.*(growth|rate)|(growth|rate).*tot",
                                     "^(pct_chg|pct_change|chg_pct|pct|growth|rate|gyy)\\z"),
                                   exclude = c(col[["pct_land"]], col[["pct_imps"]]))
  col[["report_id"]] <- pick_col(nms, NULL, c("^report_id\\z"))
  col[["nbhds"]]     <- pick_col(nms, NULL, c("^nbhds?\\z", "^neighbou?rhoods?\\z"))
  col[["area_name"]] <- pick_col(nms, NULL, c("^area_name\\z"))
  col[["n_parcels"]] <- pick_col(nms, NULL, c("^n_parcels\\z"))
  for (side in c("prev", "cur")) {
    col[[paste0(side, "_total")]] <- pick_col(nms, ov[[paste0(side, "_total")]], lvl(side, "(total|tot|av)"))
    col[[paste0(side, "_land")]]  <- pick_col(nms, ov[[paste0(side, "_land")]],  lvl(side, "land"))
    col[[paste0(side, "_imps")]]  <- pick_col(nms, ov[[paste0(side, "_imps")]],  lvl(side, "imp[a-z]*"))
  }

  message("\n  Actuals columns available: ", paste(nms, collapse = ", "))
  message("  Detected mapping (override in CFG_BT[['act_cols']] if wrong):")
  for (k in names(col)) message(sprintf("    %-12s -> %s", k, col[[k]]))

  if (is.na(col[["area"]]) && is.na(col[["spec_area"]])) {
    stop("No area or spec_area column detected in the actuals.")
  }

  act <- data.table(
    track     = tolower(col_chr(raw, col[["prop_type"]])),
    kind_raw  = tolower(col_chr(raw, col[["report_kind"]])),
    basis     = tolower(col_chr(raw, col[["basis"]])),
    year      = to_num(col_chr(raw, col[["year"]])),
    area      = norm_key(col_chr(raw, col[["area"]])),
    spec_area = norm_key(col_chr(raw, col[["spec_area"]])),
    spec_sub  = norm_key(col_chr(raw, col[["spec_sub"]])),
    act_total = to_num(col_chr(raw, col[["pct_total"]])),
    act_land  = to_num(col_chr(raw, col[["pct_land"]])),
    act_imps  = to_num(col_chr(raw, col[["pct_imps"]])),
    report_id = col_chr(raw, col[["report_id"]]),
    nbhds     = gsub("[[:space:]]", "", col_chr(raw, col[["nbhds"]])),
    area_name = col_chr(raw, col[["area_name"]]),
    act_n     = to_num(col_chr(raw, col[["n_parcels"]]))
  )

  # Percent vs fraction
  pct_vals <- abs(c(act[["act_total"]], act[["act_land"]], act[["act_imps"]]))
  pct_vals <- pct_vals[!is.na(pct_vals)]
  units <- CFG_BT[["pct_units"]]
  if (identical(units, "auto")) units <- if (length(pct_vals) && max(pct_vals) > 1) "percent" else "fraction"
  if (identical(units, "percent")) {
    act[, `:=`(act_total = act_total / 100, act_land = act_land / 100, act_imps = act_imps / 100)]
  }
  message("  Rate units: ", units)

  # Fall back to level columns where a rate is missing
  lv <- function(k) to_num(col_chr(raw, col[[k]]))
  p_tot <- lv("prev_total"); c_tot <- lv("cur_total")
  p_lnd <- lv("prev_land");  c_lnd <- lv("cur_land")
  p_imp <- lv("prev_imps");  c_imp <- lv("cur_imps")
  p_tot <- fcoalesce(p_tot, fifelse(is.na(p_lnd) | is.na(p_imp), NA_real_, p_lnd + p_imp))
  c_tot <- fcoalesce(c_tot, fifelse(is.na(c_lnd) | is.na(c_imp), NA_real_, c_lnd + c_imp))
  from_lv <- function(p, cc) fifelse(!is.na(p) & p > 0 & !is.na(cc), cc / p - 1, NA_real_)
  act[, `:=`(act_total = fcoalesce(act_total, from_lv(p_tot, c_tot)),
             act_land  = fcoalesce(act_land,  from_lv(p_lnd, c_lnd)),
             act_imps  = fcoalesce(act_imps,  from_lv(p_imp, c_imp)))]
  # Prior-year report AV: MEAN per parcel on res reports, TOTAL on com reports
  act[, act_prev := p_tot]

  # Normalise track and kind
  act[grepl("condo", track) | grepl("condo", kind_raw), track := "condo"]
  act[grepl("^res", track), track := "res"]
  act[grepl("^com", track), track := "com"]
  act[, kind := fcase(
    track == "condo", "condo",
    grepl("spec", kind_raw), "specialty",
    grepl("geo", kind_raw), "geo",
    !is.na(spec_area), "specialty",
    default = "geo")]

  # Filters
  n0 <- nrow(act)
  if (!all(is.na(act[["year"]]))) act <- act[is.na(year) | year == CFG_BT[["report_year"]]]
  act <- act[track %in% c("res", "com", "condo")]
  rb  <- substr(tolower(CFG_BT[["res_basis"]]), 1, 3)
  act <- act[!(track %in% c("res", "condo") & !is.na(basis) & basis != "" &
                 substr(basis, 1, 3) != rb)]
  act <- act[!(is.na(act_total) & is.na(act_land) & is.na(act_imps))]

  act[, `:=`(
    level = fcase(kind == "geo", "area",
                  kind == "condo", "condo_area",
                  !is.na(spec_sub), "spec_sub",
                  default = "spec"),
    key   = fifelse(kind == "geo", area,
                    fifelse(kind == "condo", fcoalesce(report_id, nbhds),
                            fifelse(!is.na(spec_sub), paste0(spec_area, "|", spec_sub), spec_area))))]
  act <- act[!is.na(key)]

  dups <- act[, .N, by = .(track, kind, key)][N > 1]
  if (nrow(dups)) {
    warning(nrow(dups), " duplicate report keys; keeping the first row of each. ",
            "See area_backtest_actuals_duplicates.csv")
    fwrite(act[dups, on = .(track, kind, key)], out_path("area_backtest_actuals_duplicates.csv"))
  }
  act <- unique(act, by = c("track", "kind", "key"))

  message(sprintf("  Actuals kept: %d of %d rows  (res areas %d | com areas %d | specialty %d | condo areas %d)",
                  nrow(act), n0,
                  act[track == "res" & kind == "geo", .N],
                  act[track == "com" & kind == "geo", .N],
                  act[kind == "specialty", .N],
                  act[kind == "condo", .N]))
  if (n0 > 0 && !any(act[["track"]] == "condo")) {
    message("  NOTE: no condo reports in the actuals. Re-run area_report_import.R with the ",
            "condo parser if 700_* PDFs are in the report folder.")
  }
  act[, .(track, kind, level, key, act_total, act_land, act_imps, act_prev,
          act_n, nbhds, area_name)]
}

# ── 2. FORECAST PANELS -> MATCHED PARCELS ───────────────────────────────────

# Long map of condo neighborhood -> report key
condo_nbhd_map <- function(act) {
  a <- act[track == "condo" & !is.na(nbhds) & nbhds != ""]
  if (!nrow(a)) return(data.table(nbhd_k = character(), key = character()))
  unique(a[, .(nbhd_k = norm_key(unlist(strsplit(nbhds, ",", fixed = TRUE)))), by = key],
         by = "nbhd_k")
}

build_matched <- function(path, trk, act = NULL, xp = NULL) {
  if (!file.exists(path)) {
    message("  SKIP (missing): ", path)
    return(NULL)
  }
  message("  Loading ", basename(path), "  [", basename(dirname(path)), "]")
  y0 <- CFG_BT[["base_yr"]]
  y1 <- CFG_BT[["target_yr"]]

  p <- readRDS(path)
  setDT(p)
  p <- p[tax_yr %in% c(y0, y1)]
  nms <- names(p)
  pc  <- CFG_BT[["panel_cols"]]

  id_c   <- pick_col(nms, NULL, c("^parcel_id\\z", "^pin\\z", "^parcel\\z", "^major_?minor\\z"))
  if (is.na(id_c)) stop("No parcel id column found in ", basename(path))
  area_c <- pick_col(nms, pc[["area"]],        c("^area\\z", "^geo_area\\z", "^kca_area\\z"))
  spec_c <- pick_col(nms, pc[["spec_area"]],   c("^spec_area\\z", "^specarea\\z"))
  ssub_c <- pick_col(nms, pc[["spec_sub"]],    c("^spec_sub\\z", "^spec_?sub"))
  src_c  <- pick_col(nms, pc[["rate_source"]], c("^co_rate_source\\z", "rate_source"))
  grp_c  <- pick_col(nms, NULL,                c("^com_subgroup\\z", "^subgroup\\z"))

  # Condo neighborhood column: candidates scored by overlap with report nbhds
  nb_cands <- character(0)
  if (trk == "condo") {
    nb_cands <- if (!is.null(pc[["condo_nbhd"]])) pc[["condo_nbhd"]] else
      grep("nbhd|neighbo|sub_?area|spec_?sub|^area\\z|condo.*(area|nbhd)",
           nms, value = TRUE, ignore.case = TRUE, perl = TRUE)
  }

  is_fc <- p[["tax_yr"]] == y1

  land_a <- col_num(p, "appr_land_val"); land_f <- col_num(p, "appr_land_val_filled")
  land_p <- col_num(p, "pred_appr_land_val")
  imps_a <- col_num(p, "appr_imps_val"); imps_f <- col_num(p, "appr_imps_val_filled")
  imps_p <- col_num(p, "pred_appr_imps_val")
  tot_a  <- col_num(p, "total_assessed"); tot_p <- col_num(p, "pred_total_assessed")

  # Forecast-year appr_* are zero-filled, so only positive values count there.
  # Order otherwise mirrors load_matched_gyy(): appr, filled, pred.
  land <- fifelse(is_fc, fcoalesce(pos_na(land_a), land_f, land_p), fcoalesce(land_a, land_f, land_p))
  imps <- fifelse(is_fc, fcoalesce(pos_na(imps_a), imps_f, imps_p), fcoalesce(imps_a, imps_f, imps_p))
  tot  <- fifelse(is.na(land) & is.na(imps), NA_real_, fcoalesce(land, 0) + fcoalesce(imps, 0))
  tot_fb <- fifelse(is_fc, pos_na(tot_p), pos_na(tot_a))
  tot  <- fifelse(is.na(tot) | tot <= 0, tot_fb, tot)

  s <- data.table(
    pid      = gsub("-", "", as.character(p[[id_c]]), fixed = TRUE),
    tax_yr   = p[["tax_yr"]],
    area_k   = norm_key(col_chr(p, area_c)),
    spec_k   = norm_key(col_chr(p, spec_c)),
    ssub_k   = norm_key(col_chr(p, ssub_c)),
    subgroup = col_chr(p, grp_c),
    src      = col_chr(p, src_c),
    land = land, imps = imps, tot = tot
  )
  for (cn in nb_cands) set(s, j = paste0("nbc__", cn), value = norm_key(col_chr(p, cn)))
  if (trk == "condo" && !is.null(xp) && is.null(pc[["condo_nbhd"]])) {
    # Condo units are not rows in EXTR_Parcel; the complex (Major) is.
    xm <- xp[, .(kca_spec_sub = SpecSubArea[SpecArea %in% "700"][1],
                 kca_subarea  = norm_key(SubArea[1])), by = Major]
    s[, Major := substr(pad0(pid, 10), 1, 6)]
    s[xm, on = "Major", `:=`(nbc__kca_SpecSubArea = i.kca_spec_sub,
                             nbc__kca_SubArea     = i.kca_subarea)]
    s[, Major := NULL]
  }
  rm(p, land_a, land_f, land_p, imps_a, imps_f, imps_p, tot_a, tot_p, land, imps, tot, tot_fb)
  gc(verbose = FALSE)

  b <- unique(s[tax_yr == y0, .(pid, land0 = land, imps0 = imps, tot0 = tot)], by = "pid")
  nbc <- grep("^nbc__", names(s), value = TRUE)
  f <- unique(s[tax_yr == y1, c("pid", "area_k", "spec_k", "ssub_k", "subgroup", "src",
                                nbc, "land", "imps", "tot"), with = FALSE], by = "pid")
  setnames(f, c("land", "imps", "tot"), c("land1", "imps1", "tot1"))
  n_fc <- nrow(f)
  m <- merge(b, f, by = "pid")
  m <- m[!is.na(tot0) & !is.na(tot1) & tot0 > 0 & tot1 > 0]

  # Land present in one year but not the other (the condo cache has all-NA
  # appr_land_val) would turn a component gap into fake growth: compare
  # improvements only for those parcels.
  mm <- m[xor(is.na(land0), is.na(land1)) & !is.na(imps0) & !is.na(imps1) & imps0 > 0]
  if (nrow(mm)) {
    m[xor(is.na(land0), is.na(land1)) & !is.na(imps0) & !is.na(imps1) & imps0 > 0,
      `:=`(tot0 = imps0, tot1 = imps1, land0 = NA_real_, land1 = NA_real_)]
    message(sprintf("    %s parcels have land in only one year: compared on improvements only",
                    format(nrow(mm), big.mark = ",")))
  }

  # Condo: choose the neighborhood column and map parcels to report cells
  if (trk == "condo") {
    m[, key := NA_character_]
    nb_map <- if (is.null(act)) NULL else condo_nbhd_map(act)
    if (length(nbc) && !is.null(nb_map) && nrow(nb_map)) {
      score <- vapply(nbc, function(cn) sum(m[[cn]] %in% nb_map[["nbhd_k"]]), numeric(1))
      message("    condo nbhd column candidates (parcels matching report nbhds): ",
              paste0(sub("^nbc__", "", nbc), "=", format(score, big.mark = ",", trim = TRUE),
                     collapse = ", "))
      best <- nbc[which.max(score)]
      if (max(score) > 0) {
        m[, nbhd_k := m[[best]]]
        m[nb_map, on = "nbhd_k", key := i.key]
        message("    using ", sub("^nbc__", "", best), " as the condo neighborhood column",
                " (override: CFG_BT[['panel_cols']][['condo_nbhd']])")
      } else {
        warning("No condo panel column matches any report neighborhood in ", basename(path),
                ". Set CFG_BT[['panel_cols']][['condo_nbhd']].")
      }
    } else if (!is.null(nb_map) && nrow(nb_map)) {
      warning("No candidate neighborhood column in ", basename(path),
              ". Set CFG_BT[['panel_cols']][['condo_nbhd']].")
    }
  }
  if (length(nbc)) m[, (nbc) := NULL]
  m[, is_spec := !is.na(spec_k)]
  m[, src_class := fcase(
    is.na(src) | src == "", "unknown",
    grepl("report|actual", src, ignore.case = TRUE), "report",
    grepl("^model|^ml\\z|lgb", src, ignore.case = TRUE, perl = TRUE), "model",
    default = "other")]

  message(sprintf("    matched %s of %s %s parcels | area=%s spec=%s src=%s",
                  format(nrow(m), big.mark = ","), format(n_fc, big.mark = ","), trk,
                  area_c, spec_c, src_c))
  if (!is.na(src_c)) {
    sv <- m[, .N, by = src][order(-N)][seq_len(min(.N, 8))]
    message("    rate-source values: ",
            paste0(fifelse(is.na(sv[["src"]]) | sv[["src"]] == "", "<blank>", sv[["src"]]),
                   "=", format(sv[["N"]], big.mark = ",", trim = TRUE), collapse = ", "))
  }
  if (trk %in% CFG_BT[["tracks_cells"]] && is.na(area_c)) {
    warning("No area column detected in ", basename(path), " - set CFG_BT[['panel_cols']][['area']]")
  }
  m
}

# The panels do not reliably label anchored rows (res has no rate-source column;
# com leaves model rows blank). So a parcel counts as anchored when it is
# declared as report-sourced, or when its TY2027 value equals TY2026 value
# times (1 + its cell's report rate) within tolerance. Everything else that is
# not an explicit hold/prior policy row is model-sourced.
classify_source <- function(m, trk, act) {
  tol_rel <- CFG_BT[["anchor_tol_rate"]]
  tol_usd <- CFG_BT[["anchor_tol_usd"]]
  m[, rate := NA_real_]
  geo <- act[track == trk & kind == "geo", .(area_k = key, rate_i = act_total)]
  if (nrow(geo) && trk != "condo") {
    m[, rate_geo := NA_real_]
    m[geo, on = "area_k", rate_geo := i.rate_i]
    use_geo <- trk == "res" | !identical(CFG_BT[["com_geo_scope"]], "nonspecialty")
    m[, rate := fifelse(is_spec == FALSE | use_geo, rate_geo, NA_real_)]
    m[, rate_geo := NULL]
  }
  if (trk == "com") {
    spc <- act[track == "com" & kind == "specialty" & level == "spec",
               .(spec_k = key, rate_i = act_total)]
    if (nrow(spc)) {
      m[, rate_spec := NA_real_]
      m[spc, on = "spec_k", rate_spec := i.rate_i]
      m[is_spec == TRUE & !is.na(rate_spec), rate := rate_spec]
      m[, rate_spec := NULL]
    }
  }
  if (trk == "condo" && "key" %in% names(m)) {
    cr <- act[track == "condo", .(key, rate_i = act_total)]
    if (nrow(cr)) m[cr, on = "key", rate := i.rate_i]
  }
  m[, hits_rate := !is.na(rate) &
        abs(tot1 - tot0 * (1 + rate)) <= pmax(tol_rel * tot0, tol_usd)]
  # Only trust a hit when most of its cell hits too; otherwise an ML parcel that
  # happens to land near the published rate would be mislabelled as anchored.
  added_key <- !"key" %in% names(m)
  if (added_key) m[, key := NA_character_]
  m[, rate_cell := if (trk == "condo") paste0("c", key)
                   else fifelse(is_spec == TRUE & trk == "com", paste0("s", spec_k), paste0("a", area_k))]
  if (added_key) m[, key := NULL]
  m[, cell_hit_share := mean(hits_rate), by = rate_cell]
  m[, hits_rate := hits_rate & cell_hit_share >= CFG_BT[["anchor_cell_share"]]]
  m[, src_detail := fcase(
    src_class == "report", "declared report",
    hits_rate == TRUE,     "inferred report",
    src_class == "other",  "other (hold/prior)",
    default = "model")]
  m[, src_class := fcase(
    src_detail %in% c("declared report", "inferred report"), "report",
    src_detail == "model", "model",
    default = "other")]
  tab <- m[, .(n = .N, av_bn = sum(tot0) / 1e9), by = src_detail][order(-av_bn)]
  message("    source classes: ",
          paste0(tab[["src_detail"]], " ", format(tab[["n"]], big.mark = ",", trim = TRUE),
                 " (", sprintf("%.1fB", tab[["av_bn"]]), ")", collapse = " | "))
  m[, c("rate", "hits_rate", "rate_cell", "cell_hit_share") := NULL]
  m
}

summ_cells <- function(dt, by_cols) {
  if (is.null(dt) || !nrow(dt)) return(NULL)
  dt[, .(n_parcels    = .N,
         av_base      = sum(tot0),
         pred_total   = sum(tot1) / sum(tot0) - 1,
         pred_land    = ratio_g(land0, land1),
         pred_imps    = ratio_g(imps0, imps1),
         share_report = mean(src_class == "report"),
         share_model  = mean(src_class == "model")),
     by = by_cols]
}

make_cells <- function(m, trk) {
  out <- list()
  if (trk == "res") {
    g <- summ_cells(m[!is.na(area_k)], "area_k")
    if (!is.null(g)) {
      g[, `:=`(kind = "geo", level = "area", key = area_k)]
      g[, area_k := NULL]
      out[["geo"]] <- g
    }
  }
  if (trk == "com") {
    g_src <- if (identical(CFG_BT[["com_geo_scope"]], "nonspecialty")) m[is_spec == FALSE] else m
    g <- summ_cells(g_src[!is.na(area_k)], "area_k")
    if (!is.null(g)) {
      g[, `:=`(kind = "geo", level = "area", key = area_k)]
      g[, area_k := NULL]
      out[["geo"]] <- g
    }
    s1 <- summ_cells(m[is_spec == TRUE], "spec_k")
    if (!is.null(s1)) {
      s1[, `:=`(kind = "specialty", level = "spec", key = spec_k)]
      s1[, spec_k := NULL]
      out[["spec"]] <- s1
    }
    s2 <- summ_cells(m[is_spec == TRUE & !is.na(ssub_k)], c("spec_k", "ssub_k"))
    if (!is.null(s2)) {
      s2[, `:=`(kind = "specialty", level = "spec_sub", key = paste0(spec_k, "|", ssub_k))]
      s2[, c("spec_k", "ssub_k") := NULL]
      out[["spec_sub"]] <- s2
    }
  }
  if (trk == "condo" && "key" %in% names(m)) {
    g <- summ_cells(m[!is.na(key)], "key")
    if (!is.null(g)) {
      g[, `:=`(kind = "condo", level = "condo_area")]
      out[["condo"]] <- g
    }
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

# ── 3. MAIN ─────────────────────────────────────────────────────────────────

run_area_backtest <- function() {
  message("\n============================================")
  message("xx_area_report_backtest.R")
  message("============================================")

  dir.create(CFG_BT[["output_dir"]], showWarnings = FALSE, recursive = TRUE)
  prim_sc <- CFG_BT[["primary_scenario"]]

  # 3a. Actuals
  message("\n[1] Area report actuals")
  act <- std_actuals(load_actuals_raw())
  if (!nrow(act)) stop("No usable area report rows after filtering.")

  # 3b. Runs
  run_dirs <- list(anchored = CFG_BT[["anchored_dir"]])
  un <- CFG_BT[["unanchored_dir"]]
  if (!is.null(un) && dir.exists(un) &&
      length(list.files(un, pattern = "_forecasted_.*[.]rds"))) {
    run_dirs[["unanchored"]] <- un
  } else {
    message("\nNOTE: no unanchored run at ", un,
            "\n      Using model-sourced parcels from the anchored run as the test set.",
            "\n      See the header for the stash / use_area_actuals = FALSE workflow.")
  }

  message("\n[2] Forecast panels")
  xp <- load_extr_parcel()
  comp_res <- NULL
  cell_list <- list()
  city_list <- list()
  cov_list  <- list()
  for (rn in names(run_dirs)) {
    for (sc in CFG_BT[["scenarios"]]) {
      for (trk in union(CFG_BT[["tracks_cells"]], CFG_BT[["tracks_context"]])) {
        path <- file.path(run_dirs[[rn]], sprintf(CFG_BT[["panel_pattern"]], sc, trk))
        m <- build_matched(path, trk, act, xp)
        if (is.null(m) || !nrow(m)) next

        if (trk %in% CFG_BT[["tracks_context"]]) {
          cw <- summ_cells(m, NULL)
          cw[, `:=`(run = rn, scenario = sc, track = trk)]
          city_list[[length(city_list) + 1]] <- cw
        }

        if (trk %in% CFG_BT[["tracks_cells"]]) {
          if (rn == names(run_dirs)[1] && sc == prim_sc) {
            if (trk == "res" && !is.null(xp)) {
              # What kind of property sits in each residential area cell
              rc <- merge(m[, .(pid = pad0(pid, 10), area_k, tot0)],
                          xp[, .(pid, PropType, SpecArea, PresentUse, kca_area = Area)],
                          by = "pid", all.x = TRUE)
              comp_res <- rc[, .(n = .N, av_bn = sum(tot0) / 1e9),
                             by = .(area_k, PropType, SpecArea, PresentUse, kca_area)]
              comp_res[, av_share := av_bn / sum(av_bn), by = area_k]
              setorder(comp_res, area_k, -av_bn)
              fwrite(comp_res, out_path("area_backtest_res_composition.csv"))
              rm(rc)
            }
            cv <- make_cells(m, trk)
            if (!is.null(cv) && nrow(cv)) {
              cov_list[[length(cov_list) + 1]] <- cv[, .(track = trk, kind, level, key,
                                                         n_parcels_all = n_parcels,
                                                         av_base_all = av_base)]
            }
          }
          subsets <- if (rn == "unanchored") {
            list(pure_model = m)
          } else {
            m <- classify_source(m, trk, act)
            list(model_rows = m[src_class == "model"],
                 anchor_qa  = m[src_class == "report"])
          }
          for (md in names(subsets)) {
            cl <- make_cells(subsets[[md]], trk)
            if (is.null(cl) || !nrow(cl)) next
            cl[, `:=`(run = rn, mode = md, scenario = sc, track = trk)]
            cell_list[[length(cell_list) + 1]] <- cl
          }
          rm(subsets)
        }
        rm(m)
        gc(verbose = FALSE)
      }
    }
  }

  cells    <- rbindlist(cell_list, use.names = TRUE, fill = TRUE)
  citywide <- rbindlist(city_list, use.names = TRUE, fill = TRUE)
  if (!nrow(cells)) stop("No model cells built. Check panel paths and the column detection above.")

  # 3c. Join to actuals
  message("\n[3] Joining to actuals")
  keys <- c("track", "kind", "level", "key")
  unmatched <- act[!unique(cells[, ..keys]), on = keys]
  fwrite(unmatched, out_path("area_backtest_unmatched_reports.csv"))
  if (nrow(unmatched)) {
    message("  ", nrow(unmatched), " report cells had no matching parcels ",
            "(area_backtest_unmatched_reports.csv)")
  }

  cells <- merge(cells, act, by = keys)
  if (!nrow(cells)) stop("No report cells matched the panel. Compare key formats: ",
                         "panel area/spec_area vs actuals area/spec_area.")

  cells[, `:=`(err_total = pred_total - act_total,
               err_land  = pred_land  - act_land,
               err_imps  = pred_imps  - act_imps)]
  cells[, miss_usd := err_total * av_base]

  # Coverage: how much of the reported area the panel actually sees (all modes combined).
  # Res reports give MEAN AV per improved parcel; com reports give TOTAL AV.
  cov <- unique(rbindlist(cov_list), by = keys)
  if (nrow(cov)) cells <- merge(cells, cov, by = keys, all.x = TRUE)
  cells[, coverage_ratio := fifelse(track %in% c("res", "condo"),
                                    (av_base_all / n_parcels_all) / act_prev,
                                    av_base_all / act_prev)]
  cells[, coverage_basis := fifelse(track %in% c("res", "condo"),
                                    "panel mean AV / report mean AV",
                                    "panel matched AV / report total AV")]
  cells[, count_ratio := n_parcels_all / act_n]   # condo reports give a unit count
  cells[, coverage_flag := kind %in% c("geo", "condo") &
          ((!is.na(coverage_ratio) & (coverage_ratio < 0.67 | coverage_ratio > 1.5)) |
           (!is.na(count_ratio) & (count_ratio < 0.5 | count_ratio > 1.2)))]
  cells[, cell_type := fcase(track == "res", "Residential area",
                             track == "condo", "Condo area",
                             kind == "geo", "Commercial area",
                             default = "Commercial specialty")]
  cells[, cell_label := fifelse(track == "res", paste("Res", key),
                                fifelse(track == "condo", paste("Condo", fcoalesce(area_name, key)),
                                        fifelse(kind == "geo", paste("Com", key),
                                                paste("Spec", gsub("|", " / ", key, fixed = TRUE)))))]
  cells[, actual_scope := fifelse(kind == "specialty", "countywide specialty report", "area report")]
  cells[, mode_lab := factor(MODE_LABELS[mode], levels = MODE_LABELS)]
  cells[, cell_type := factor(cell_type, levels = names(TYPE_COLOURS))]

  # Scenario spread and ordering
  sc_wide <- dcast(cells, mode + track + kind + level + key ~ scenario, value.var = "pred_total")
  sc_have <- intersect(CFG_BT[["scenarios"]], names(sc_wide))
  setnames(sc_wide, sc_have, paste0("pred_total_", sc_have))
  sc_mat <- as.matrix(sc_wide[, paste0("pred_total_", sc_have), with = FALSE])
  sc_wide[, pred_total_min := suppressWarnings(apply(sc_mat, 1, min, na.rm = TRUE))]
  sc_wide[, pred_total_max := suppressWarnings(apply(sc_mat, 1, max, na.rm = TRUE))]
  sc_wide[is.infinite(pred_total_min), pred_total_min := NA_real_]
  sc_wide[is.infinite(pred_total_max), pred_total_max := NA_real_]
  if (all(c("optimistic", "pessimistic") %in% sc_have)) {
    sc_wide[, scen_inverted := get("pred_total_optimistic") < get("pred_total_pessimistic")]
  }

  if (!prim_sc %in% cells[["scenario"]]) stop("Primary scenario '", prim_sc, "' has no cells.")
  prim <- merge(cells[scenario == prim_sc], sc_wide, by = c("mode", keys), all.x = TRUE)

  # Keep spec_sub rows out of totals when a whole-specialty row exists (no double counting)
  prim[, spec_root := fifelse(level == "spec_sub", sub("\\|.*", "", key), NA_character_)]
  has_spec <- unique(prim[level == "spec", .(mode, track, spec_root = key)])
  prim[, tiny := n_parcels < CFG_BT[["min_cell_parcels"]] | av_base < CFG_BT[["min_cell_av"]]]
  prim[, in_summary := !tiny]
  if (nrow(has_spec)) prim[has_spec, on = .(mode, track, spec_root), in_summary := FALSE]

  # Primary view per track: best available test for that track
  mode_pref  <- c("pure_model", "model_rows", "anchored_mixed", "anchor_qa")
  prim_modes <- prim[, .(pmode = intersect(mode_pref, unique(mode))[1]), by = track]
  prim[prim_modes, on = "track", is_primary := mode == i.pmode]
  plab <- paste(unique(MODE_LABELS[prim_modes[["pmode"]]]), collapse = " + ")

  setorder(prim, mode, cell_type, err_total)
  fwrite(prim, out_path("area_backtest_cells.csv"))
  fwrite(cells, out_path("area_backtest_cells_all_scen.csv"))

  # 3d. Summaries
  message("\n[4] Summaries")
  comp_long <- rbindlist(lapply(c("total", "land", "imps"), function(cp) {
    prim[in_summary == TRUE,
         .(mode, mode_lab, is_primary, cell_type, cell_label, av_base, component = cp,
           pred = get(paste0("pred_", cp)), act = get(paste0("act_", cp)))]
  }))[!is.na(pred) & !is.na(act)]
  comp_long[, err := pred - act]
  comp_long[, component := factor(component, levels = c("total", "land", "imps"),
                                  labels = c("Total", "Land", "Improvements"))]

  acc <- function(d) {
    d[, .(n_cells       = .N,
          av_base_bn    = sum(av_base) / 1e9,
          wtd_bias_pp   = 100 * weighted.mean(err, av_base),
          wtd_mae_pp    = 100 * weighted.mean(abs(err), av_base),
          rmse_pp       = 100 * sqrt(mean(err^2)),
          within_1pp    = mean(abs(err) <= 0.01),
          direction_hit = mean(sign(pred) == sign(act)),
          corr          = if (.N >= 3 && sd(pred) > 0 && sd(act) > 0) cor(pred, act) else NA_real_),
      by = .(mode, mode_lab, cell_type, component)]
  }
  summary_dt <- rbind(acc(comp_long),
                      acc(copy(comp_long)[, cell_type := "All reported cells"]))
  setorder(summary_dt, mode, cell_type, component)
  fwrite(summary_dt, out_path("area_backtest_summary.csv"))

  track_base <- citywide[scenario == prim_sc, .(run, track, track_av_base = av_base)]
  dollars <- prim[in_summary == TRUE,
                  .(n_cells  = .N,
                    av_cells = sum(av_base),
                    miss_usd = sum(miss_usd, na.rm = TRUE),
                    over_usd = sum(pmax(miss_usd, 0), na.rm = TRUE),
                    under_usd = sum(pmin(miss_usd, 0), na.rm = TRUE)),
                  by = .(run, mode, track, cell_type)]
  dollars <- merge(dollars, track_base, by = c("run", "track"), all.x = TRUE)
  dollars[, coverage_share := av_cells / track_av_base]
  fwrite(dollars, out_path("area_backtest_dollars.csv"))

  citywide_all <- citywide[, .(n_parcels = sum(n_parcels),
                               av_base   = sum(av_base),
                               pred_total = sum(av_base * (1 + pred_total)) / sum(av_base) - 1),
                           by = .(run, scenario)][, track := "all"]
  citywide <- rbind(citywide, citywide_all, fill = TRUE)
  citywide[, gap_vs_worksheet_pp := 100 * (pred_total - CFG_BT[["worksheet_existing_growth"]])]
  fwrite(citywide, out_path("area_backtest_citywide.csv"))

  # Console readout
  message("\n  Primary view: ", plab, "  (", prim_sc, ")")
  pm_keys <- unique(prim[is_primary == TRUE, .(mode, cell_type)])
  print(summary_dt[pm_keys, on = .(mode, cell_type), nomatch = NULL][component == "Total",
                   .(cell_type, n_cells, av_base_bn = round(av_base_bn, 1),
                     bias_pp = round(wtd_bias_pp, 2), mae_pp = round(wtd_mae_pp, 2),
                     within_1pp = round(within_1pp, 2), direction_hit = round(direction_hit, 2))])
  message("\n  Largest dollar misses:")
  print(prim[is_primary == TRUE & in_summary == TRUE][order(-abs(miss_usd))][
    seq_len(min(.N, 10)),
    .(cell_label, n_parcels, av_base_bn = round(av_base / 1e9, 2),
      actual_pct = round(100 * act_total, 2), model_pct = round(100 * pred_total, 2),
      miss_bn = round(miss_usd / 1e9, 3))])
  cov_bad <- unique(prim[coverage_flag == TRUE,
                         .(cell_label, n_parcels_all, av_base_bn = round(av_base_all / 1e9, 2),
                           report_prev = act_prev, coverage_ratio = round(coverage_ratio, 2),
                           count_ratio = round(count_ratio, 2), coverage_basis)])
  if (nrow(cov_bad)) {
    message("\n  Coverage flags (panel vs report outside 0.67-1.5x):")
    print(cov_bad[order(-abs(log(fcoalesce(coverage_ratio, 1))))])
    if (any(grepl("^Res ", cov_bad[["cell_label"]]) & cov_bad[["coverage_ratio"]] > 3, na.rm = TRUE)) {
      message("  A residential panel mean far above the report mean means the parcels in that\n",
              "  cell are not the report's population: check where the res panel's area code\n",
              "  comes from (a commercial geo area number landing on a res area of the same number).")
      if (!is.null(comp_res)) {
        bad_keys <- sub("^Res ", "", cov_bad[grepl("^Res ", cell_label) & coverage_ratio > 1.5][["cell_label"]])
        message("\n  What is in the flagged residential cells (largest groups by TY2026 AV;",
                " full table in area_backtest_res_composition.csv):")
        print(comp_res[area_k %in% bad_keys][order(area_k, -av_bn)][
          , head(.SD, 6), by = area_k][
          , .(area_k, PropType, SpecArea, PresentUse, kca_area, n,
              av_bn = round(av_bn, 2), av_share = round(av_share, 2))])
      }
    }
  }
  if ("anchor_qa" %in% prim[["mode"]]) {
    qa_bad <- prim[mode == "anchor_qa" & abs(err_total) > 0.001]
    message(sprintf("\n  Anchor QA: %d of %d anchored cells off by more than 0.1pp",
                    nrow(qa_bad), prim[mode == "anchor_qa", .N]))
  }

  # 3e. Charts
  message("\n[5] Charts")
  th <- bt_theme()
  pct_lab <- label_percent(accuracy = 0.1)
  bn_lab  <- label_dollar(accuracy = 0.1, suffix = "B")
  cap <- paste0("Matched-parcel growth TY", CFG_BT[["base_yr"]], " to TY", CFG_BT[["target_yr"]],
                ". Specialty actuals are countywide; model cells are Seattle only.")

  # 01 Model vs actual, all modes
  d1 <- prim[tiny == FALSE & !is.na(pred_total) & !is.na(act_total)]
  if (nrow(d1)) {
    lab1 <- d1[in_summary == TRUE][order(-abs(miss_usd)), head(.SD, CFG_BT[["label_top_n"]]), by = mode_lab]
    lim  <- range(c(d1[["pred_total"]], d1[["act_total"]]), na.rm = TRUE)
    lim  <- lim + c(-1, 1) * max(0.01, diff(lim) * 0.05)
    n_modes <- length(unique(d1[["mode"]]))
    n_col   <- min(n_modes, 2L)
    n_row   <- ceiling(n_modes / n_col)
    p1 <- ggplot(d1, aes(act_total, pred_total)) +
      geom_hline(yintercept = 0, colour = "grey85") +
      geom_vline(xintercept = 0, colour = "grey85") +
      geom_abline(slope = 1, intercept = 0, colour = "grey40") +
      geom_abline(slope = 1, intercept = c(-0.02, 0.02), colour = "grey60", linetype = "dotted") +
      geom_point(aes(size = av_base / 1e9, colour = cell_type), alpha = 0.75) +
      geom_text(data = lab1, aes(label = cell_label), size = 2.7, vjust = -1.1,
                check_overlap = TRUE, colour = "grey20") +
      facet_wrap(~ mode_lab, ncol = n_col) +
      coord_equal(xlim = lim, ylim = lim) +
      scale_x_continuous(labels = pct_lab) +
      scale_y_continuous(labels = pct_lab) +
      scale_colour_manual(values = TYPE_COLOURS, drop = FALSE) +
      scale_size_area(max_size = 10, labels = bn_lab) +
      labs(title = "Model vs KCA area reports: TY2027 growth",
           subtitle = paste0("Solid line = perfect; dotted = +/- 2pp. Scenario: ", prim_sc,
                             ". Labels mark the largest dollar misses."),
           x = "KCA published change", y = "Model change",
           colour = NULL, size = "TY2026 AV", caption = cap) +
      th
    bt_save(p1, "01_pred_vs_actual.png", width = 5.5 * n_col + 1.5, height = 5.2 * n_row + 2)
  }

  # 02 Dumbbell by cell, primary mode
  d2 <- prim[is_primary == TRUE & tiny == FALSE & !is.na(pred_total) & !is.na(act_total)]
  if (nrow(d2)) {
    d2[, cell_label := factor(cell_label, levels = unique(d2[order(err_total)][["cell_label"]]))]
    pts <- rbind(d2[, .(cell_label, cell_type, x = act_total,  what = "KCA actual")],
                 d2[, .(cell_label, cell_type, x = pred_total, what = paste0("Model (", prim_sc, ")"))])
    p2 <- ggplot(d2, aes(y = cell_label)) +
      geom_vline(xintercept = 0, colour = "grey80") +
      geom_linerange(aes(xmin = pred_total_min, xmax = pred_total_max),
                     colour = "#c0392b", alpha = 0.25, linewidth = 3) +
      geom_segment(aes(x = act_total, xend = pred_total, yend = cell_label), colour = "grey55") +
      geom_point(data = pts, aes(x = x, colour = what), size = 2.4) +
      facet_grid(cell_type ~ ., scales = "free_y", space = "free_y") +
      scale_x_continuous(labels = pct_lab) +
      scale_colour_manual(values = c("#2c3e50", "#c0392b")) +
      labs(title = "Where the model landed, cell by cell",
           subtitle = paste0(plab,
                             ". Shaded bar = range across scenarios. Sorted by miss."),
           x = "TY2027 change", y = NULL, colour = NULL, caption = cap) +
      th + theme(strip.text.y = element_text(angle = 0, hjust = 0))
    bt_save(p2, "02_cell_dumbbell.png", width = 10, height = 2.5 + 0.2 * nrow(d2))
  }

  # 03 Dollar miss, primary mode
  d3 <- prim[is_primary == TRUE & in_summary == TRUE & !is.na(miss_usd)]
  if (nrow(d3)) {
    d3 <- d3[order(-abs(miss_usd))][seq_len(min(.N, 30))]
    d3[, direction := fifelse(miss_usd > 0, "Model too high", "Model too low")]
    d3[, cell_label := factor(cell_label, levels = unique(d3[order(miss_usd)][["cell_label"]]))]
    tot_miss <- prim[is_primary == TRUE & in_summary == TRUE, sum(miss_usd, na.rm = TRUE)]
    p3 <- ggplot(d3, aes(x = miss_usd / 1e9, y = cell_label, fill = direction)) +
      geom_col() +
      geom_vline(xintercept = 0, colour = "grey40") +
      facet_grid(cell_type ~ ., scales = "free_y", space = "free_y") +
      scale_x_continuous(labels = bn_lab) +
      scale_fill_manual(values = c("Model too high" = "#c0392b", "Model too low" = "#2980b9")) +
      labs(title = "Dollars attributable to each cell's miss",
           subtitle = paste0(plab, ". (model - actual) x TY2026 AV. Net across covered cells: ",
                             bn_lab(tot_miss / 1e9), ". Top 30 shown."),
           x = NULL, y = NULL, fill = NULL, caption = cap) +
      th + theme(strip.text.y = element_text(angle = 0, hjust = 0))
    bt_save(p3, "03_dollar_miss.png", width = 10, height = 2.5 + 0.22 * nrow(d3))
  }

  # 04 Components, primary mode
  d4 <- comp_long[is_primary == TRUE]
  if (nrow(d4)) {
    p4 <- ggplot(d4, aes(act, pred)) +
      geom_hline(yintercept = 0, colour = "grey85") +
      geom_vline(xintercept = 0, colour = "grey85") +
      geom_abline(slope = 1, intercept = 0, colour = "grey40") +
      geom_blank(aes(x = pred, y = act)) +   # same range on both axes per panel
      geom_point(aes(size = av_base / 1e9, colour = cell_type), alpha = 0.7) +
      facet_wrap(cell_type ~ component, scales = "free", ncol = 3) +
      scale_x_continuous(labels = pct_lab) +
      scale_y_continuous(labels = pct_lab) +
      scale_colour_manual(values = TYPE_COLOURS, guide = "none") +
      scale_size_area(max_size = 7, labels = bn_lab) +
      labs(title = "Land vs improvements: which side missed?",
           subtitle = plab,
           x = "KCA published change", y = "Model change", size = "TY2026 AV", caption = cap) +
      th
    bt_save(p4, "04_component_error.png", width = 11,
            height = 2.2 + 3.2 * length(unique(d4[["cell_type"]])))
  }

  # 05 Accuracy summary, all modes
  d5 <- melt(summary_dt, id.vars = c("mode_lab", "cell_type", "component"),
             measure.vars = c("wtd_bias_pp", "wtd_mae_pp"), variable.name = "metric")
  if (nrow(d5)) {
    d5[, metric := factor(metric, levels = c("wtd_bias_pp", "wtd_mae_pp"),
                          labels = c("Bias (model - actual)", "Mean absolute error"))]
    p5 <- ggplot(d5, aes(component, value / 100, fill = metric)) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_text(aes(label = sprintf("%.1f", value),
                    vjust = ifelse(value >= 0, -0.4, 1.3)),
                position = position_dodge(width = 0.75), size = 2.8) +
      facet_grid(mode_lab ~ cell_type) +
      scale_y_continuous(labels = pct_lab, expand = expansion(mult = 0.15)) +
      scale_fill_manual(values = c("#e67e22", "#7f8c8d")) +
      labs(title = "Value-weighted accuracy (percentage points)",
           subtitle = "Weighted by TY2026 AV of the cell. Positive bias = model ran hot.",
           x = NULL, y = NULL, fill = NULL, caption = cap) +
      th + theme(strip.text.y = element_text(angle = 0, hjust = 0))
    bt_save(p5, "05_accuracy_summary.png", width = 12,
            height = 2 + 2.6 * length(unique(d5[["mode_lab"]])))
  }

  # 06 Citywide context
  d6 <- citywide[, .(pred = pred_total[scenario == prim_sc][1],
                     lo = min(pred_total), hi = max(pred_total)),
                 by = .(run, track)]
  if (nrow(d6)) {
    d6[, track := factor(track, levels = c("res", "com", "condo", "all"),
                         labels = c("Residential", "Commercial", "Condo", "All tracks"))]
    wb <- CFG_BT[["worksheet_existing_growth"]]
    p6 <- ggplot(d6, aes(track, pred, fill = run)) +
      geom_hline(yintercept = 0, colour = "grey60") +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.8), width = 0.25) +
      geom_hline(yintercept = wb, colour = "#c0392b", linetype = "dashed") +
      annotate("text", x = 0.5, y = wb, label = paste0("Prelim worksheet, existing property ", pct_lab(wb)),
               hjust = 0, vjust = -0.5, colour = "#c0392b", size = 3.2) +
      scale_y_continuous(labels = pct_lab) +
      scale_fill_manual(values = c(anchored = "#34495e", unanchored = "#95a5a6")) +
      labs(title = "TY2027 matched-parcel growth by track",
           subtitle = paste0("Bars = ", prim_sc, "; whiskers = scenario range. ",
                             "Worksheet line includes personal property; context, not like-for-like."),
           x = NULL, y = NULL, fill = "Run") +
      th
    bt_save(p6, "06_citywide_context.png", width = 9, height = 6)
  }

  message("\nDone. Outputs in ", CFG_BT[["output_dir"]])
  invisible(list(cells = prim, all_scenarios = cells, summary = summary_dt,
                 dollars = dollars, citywide = citywide, actuals = act))
}

if (!isTRUE(get0("BT_DEFINE_ONLY", envir = .GlobalEnv, ifnotfound = FALSE))) {
  bt_results <- run_area_backtest()
}
