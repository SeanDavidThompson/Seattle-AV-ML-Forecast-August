###############################################################################
# Non-Stationarity Audit -- all commercial subgroup models
#
# Generalizes the apartment diagnosis to every commercial booster.
#
# The apartment failure had THREE signatures at once, and it is the
# combination that matters:
#
#   1. MONOTONE. cs_apt_demand_units rose in nearly every year from 2002
#      (275,809) to 2025 (557,239). A variable that only goes one way is a
#      time index, whatever it is named.
#   2. OUT OF RANGE. Forecast values (579,354-619,897) sat ABOVE every value
#      in training (max 557,239). The model had never seen that region.
#   3. NO SPLITS. All 113 tree thresholds were at or below 531,289, so every
#      forecast year fell in one leaf. The response was a CONSTANT - and the
#      constant was learned from 2024-2025, the two worst target years.
#      Rising demand could not improve the forecast even in principle.
#
# Any one signature alone is usually benign. Signature 3 is the one that
# actually breaks the forecast, so the flags below are ordered by it.
#
# Reads boosters and forecast panels. Writes a CSV. Changes nothing.
#
# STYLE: no $ operator - it gets stripped in transit. Use x[["name"]].
###############################################################################

library(data.table)
library(lightgbm)

CACHE_DIR <- "./data/cache"
MODEL_DIR <- "./data/model"
OUT_DIR   <- "./output/exhibits"
FCST_FROM <- 2027L
HIST_TO   <- 2026L
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

SUBGROUPS <- c("apt", "office", "industrial", "retail", "hospitality", "medical")

say <- function(...) cat("\n", paste0(...), "\n", sep = "")

###############################################################################
# Helpers
###############################################################################

# Locate a booster without assuming a filename. Subgroup models carry the
# subgroup token; the bare lgb_impr_delta_cv_* files are RESIDENTIAL and must
# not be picked up here.
find_booster <- function(sg, target = "impr_delta") {
  pat <- sprintf("^lgb_%s_%s_cv.*\\.rds$", sg, target)
  f <- list.files(MODEL_DIR, pattern = pat, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  f[order(file.mtime(f), decreasing = TRUE)][1]
}

as_booster <- function(obj) {
  if (inherits(obj, "lgb.Booster")) return(obj)
  if (!is.list(obj)) return(NULL)
  for (nm in c("model", "booster"))
    if (inherits(obj[[nm]], "lgb.Booster")) return(obj[[nm]])
  NULL
}

panel_for <- function(sg) {
  p <- file.path(CACHE_DIR,
                 sprintf("panel_tbl_2006_2031_forecasted_baseline_%s.rds", sg))
  if (file.exists(p)) return(p)
  # fall back to the combined commercial panel, filtered by subgroup
  file.path(CACHE_DIR, "panel_tbl_2006_2031_forecasted_baseline_com.rds")
}

# Monotonicity on the yearly series, not the parcel rows. A market variable is
# one value per year, so collapse first or every parcel duplicates the signal.
monotone_share <- function(yearly) {
  v <- yearly[!is.na(yearly)]
  if (length(v) < 4) return(NA_real_)
  d <- diff(v)
  d <- d[d != 0]
  if (!length(d)) return(NA_real_)
  max(mean(d > 0), mean(d < 0))
}

###############################################################################
# Audit one subgroup
###############################################################################

audit_subgroup <- function(sg) {
  bp <- find_booster(sg)
  if (is.null(bp)) {
    cat("  ", sg, ": no impr_delta booster found - skipping\n", sep = "")
    return(NULL)
  }
  obj <- readRDS(bp)
  booster <- as_booster(obj)
  xcols   <- obj[["x_cols"]]
  if (is.null(booster) || is.null(xcols)) {
    cat("  ", sg, ": booster or x_cols missing - skipping\n", sep = "")
    return(NULL)
  }

  pp <- panel_for(sg)
  d <- readRDS(pp)
  if (!is.data.table(d)) setDT(d)
  d[, tax_yr := as.integer(tax_yr)]
  if ("com_subgroup" %in% names(d)) d <- d[com_subgroup == sg]
  if (nrow(d) == 0) {
    cat("  ", sg, ": no rows in panel - skipping\n", sep = "")
    return(NULL)
  }

  cat("\n", sg, ": ", basename(bp), " | ", length(xcols), " features | ",
      basename(pp), "\n", sep = "")

  # Tree thresholds, one pass per booster
  tr <- as.data.table(lgb.model.dt.tree(booster))
  thr <- tr[!is.na(split_feature),
            .(n_splits = .N,
              thr_min  = min(threshold, na.rm = TRUE),
              thr_max  = max(threshold, na.rm = TRUE)),
            by = .(feature = split_feature)]

  # Gain, so we can rank flags by how much they matter
  imp <- as.data.table(lgb.importance(booster))
  setnames(imp, c("Feature", "Gain"), c("feature", "gain"), skip_absent = TRUE)

  present <- intersect(xcols, names(d))

  rows <- rbindlist(lapply(present, function(f) {
    v <- suppressWarnings(as.numeric(d[[f]]))
    if (all(is.na(v))) return(NULL)

    # Collapse to one value per year before judging the shape
    yr <- d[, .(val = mean(suppressWarnings(as.numeric(get(f))), na.rm = TRUE)),
            by = tax_yr][order(tax_yr)]
    yr <- yr[!is.nan(val)]
    hist_v <- yr[tax_yr <= HIST_TO,   val]
    fc_v   <- yr[tax_yr >= FCST_FROM, val]
    if (!length(hist_v) || !length(fc_v)) return(NULL)

    # Parcel-varying features have no meaningful yearly series - skip them.
    # A market variable has one value per year across all parcels.
    parcel_varying <- d[tax_yr == FCST_FROM,
                        data.table::uniqueN(round(suppressWarnings(
                          as.numeric(get(f))), 6))] > 1

    data.table(
      subgroup   = sg,
      feature    = f,
      gain_pct   = 100 * (imp[feature == f, gain][1]),
      parcel_lvl = parcel_varying,
      mono       = monotone_share(yr[["val"]]),
      hist_min   = min(hist_v, na.rm = TRUE),
      hist_max   = max(hist_v, na.rm = TRUE),
      fc_min     = min(fc_v, na.rm = TRUE),
      fc_max     = max(fc_v, na.rm = TRUE),
      n_splits   = thr[feature == f, n_splits][1],
      thr_min    = thr[feature == f, thr_min][1],
      thr_max    = thr[feature == f, thr_max][1]
    )
  }), fill = TRUE)

  if (!nrow(rows)) return(NULL)

  rows[is.na(gain_pct), gain_pct := 0]
  rows[is.na(n_splits), n_splits := 0L]

  # --- the three signatures -------------------------------------------------
  rows[, sig_monotone := !is.na(mono) & mono >= 0.85]
  rows[, sig_out_of_range := fc_min > hist_max | fc_max < hist_min]
  # No split inside the forecast region: every forecast year lands in one leaf
  rows[, sig_no_splits := n_splits > 0 &
         (thr_max < fc_min | thr_min > fc_max)]
  rows[, n_sig := as.integer(sig_monotone) + as.integer(sig_out_of_range) +
         as.integer(sig_no_splits)]

  rows[]
}

###############################################################################
# Run
###############################################################################

say("=== auditing ", length(SUBGROUPS), " commercial impr_delta models ===")
res <- rbindlist(lapply(SUBGROUPS, function(sg) {
  r <- try(audit_subgroup(sg), silent = TRUE)
  if (inherits(r, "try-error")) {
    cat("  ERROR on ", sg, ": ",
        conditionMessage(attr(r, "condition")), "\n", sep = "")
    return(NULL)
  }
  r
}), fill = TRUE)

if (!nrow(res)) stop("nothing audited - check the messages above")

# Market-level series only. Parcel-varying features are building traits and
# cannot be non-stationary in the sense that matters here.
mkt <- res[parcel_lvl == FALSE]

###############################################################################
# Report
###############################################################################

say("=== ALL THREE SIGNATURES (the apartment pattern) ===")
cat("Monotone, forecast outside training range, AND no splits in the\n")
cat("forecast region. These are broken the way cs_apt_demand_units was.\n\n")
f3 <- mkt[n_sig == 3][order(-gain_pct)]
if (nrow(f3)) {
  print(f3[, .(subgroup, feature, gain_pct = round(gain_pct, 2),
               hist_max = signif(hist_max, 4), fc_min = signif(fc_min, 4),
               n_splits, thr_max = signif(thr_max, 4))])
} else {
  cat("None. Apartments may have been the only fully broken case.\n")
}

say("=== NO SPLITS IN THE FORECAST REGION (the one that actually bites) ===")
cat("The model's response is a constant across the whole horizon, whether\n")
cat("or not the variable is monotone.\n\n")
fns <- mkt[sig_no_splits == TRUE][order(-gain_pct)]
print(head(fns[, .(subgroup, feature, gain_pct = round(gain_pct, 2),
                   n_splits, thr_min = signif(thr_min, 4),
                   thr_max = signif(thr_max, 4),
                   fc_min = signif(fc_min, 4), fc_max = signif(fc_max, 4))], 25))

say("=== FORECAST OUTSIDE TRAINING RANGE ===")
for_ <- mkt[sig_out_of_range == TRUE][order(-gain_pct)]
print(head(for_[, .(subgroup, feature, gain_pct = round(gain_pct, 2),
                    hist_min = signif(hist_min, 4),
                    hist_max = signif(hist_max, 4),
                    fc_min = signif(fc_min, 4),
                    fc_max = signif(fc_max, 4))], 25))

say("=== MONOTONE, but splits cover the forecast (watch, do not fix) ===")
mon <- mkt[sig_monotone == TRUE & sig_no_splits == FALSE][order(-gain_pct)]
print(head(mon[, .(subgroup, feature, gain_pct = round(gain_pct, 2),
                   mono = round(mono, 2))], 15))

say("=== per-subgroup summary ===")
summ <- mkt[, .(market_features = .N,
                flagged_3 = sum(n_sig == 3),
                no_splits = sum(sig_no_splits),
                out_of_range = sum(sig_out_of_range),
                monotone = sum(sig_monotone),
                gain_at_risk = round(sum(gain_pct[sig_no_splits]), 1)),
            by = subgroup][order(-gain_at_risk)]
print(summ)
cat("\ngain_at_risk is the share of the model's explanatory weight sitting\n")
cat("on features whose forecast response is a constant. For apartments\n")
cat("before the fix this was about 11%.\n")

fwrite(res, file.path(OUT_DIR, "nonstationarity_audit_full.csv"))
fwrite(mkt[n_sig > 0][order(subgroup, -gain_pct)],
       file.path(OUT_DIR, "nonstationarity_audit_flagged.csv"))

say("=== suggested detrend pattern ===")
cat("Add these to costar_detrend_pattern if you decide to fix them:\n\n")
cand <- unique(f3[["feature"]])
if (!length(cand)) cand <- unique(fns[gain_pct >= 1, feature])
if (length(cand)) {
  stems <- unique(sub("^cs_[a-z]+_", "", cand))
  cat('  costar_detrend_pattern <- "_(',
      paste(stems, collapse = "|"), ')$"\n', sep = "")
  cat("\nCurrent default is _(demand_units|inventory_units)$\n")
} else {
  cat("  nothing beyond the current default\n")
}

say("Done. CSVs in ", OUT_DIR)
