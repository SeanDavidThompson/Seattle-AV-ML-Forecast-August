# area_report_compare_2026.R -----------------------------------------------
# Compare the new 2026 area-report import with the pre-change baseline
# written by ad_hoc/area_report_baseline_2026.R.
#
# Join key: prop_type, report_kind, area, spec_area, spec_sub, spec_region,
# basis (+ nbhds where the key alone is not unique, i.e. condo reports).
# Every column is compared except report_id, source_file and the new columns
# (report_year, tax_yr).  Each difference is classified:
#   total_mismatch  a total column (AR_CMP_TOTAL) differs in a row present in
#                   both                                  -> fails, any spec
#   filled          land/imps blank in the baseline, filled now -> allowed
#   changed         one non-blank value to another        -> fails outside
#   cleared         non-blank to blank                       AR_ALLOWED_DIFF_SPECS
#   other_filled    blank to non-blank, not land/imps
#   added, removed  whole rows                            -> fail outside
#                                                            AR_ALLOWED_DIFF_SPECS
#
#   Rscript ad_hoc/area_report_compare_2026.R
# ---------------------------------------------------------------------------

AR_CMP_KEY   <- c("prop_type", "report_kind", "area", "spec_area", "spec_sub",
                  "spec_region", "basis")
AR_CMP_SKIP  <- c("report_id", "source_file", "report_year", "tax_yr")
AR_CMP_TOTAL <- c("av_prev", "av_curr", "delta", "pct_change", "pct_change_chk")
AR_CMP_FILL  <- c("land_prev", "land_curr", "imps_prev", "imps_curr", "pct_land", "pct_imps")
AR_ALLOWED_DIFF_SPECS <- c(153L, 174L, 500L, 510L)
AR_CMP_KINDS <- c("total_mismatch", "changed", "cleared", "other_filled", "filled",
                  "added", "removed")

# Returns a data.frame of differences:
#   kind, key columns, column, old, new, allowed
ar_compare_actuals <- function(base, new, allowed_specs = AR_ALLOWED_DIFF_SPECS) {
  base <- as.data.frame(base); new <- as.data.frame(new)
  for (k in AR_CMP_KEY) {
    if (!k %in% names(base)) base[[k]] <- NA
    if (!k %in% names(new))  new[[k]]  <- NA
  }
  key <- AR_CMP_KEY
  if (anyDuplicated(base[key]) || anyDuplicated(new[key])) key <- c(key, "nbhds")
  if (anyDuplicated(base[key]) || anyDuplicated(new[key]))
    stop("join key is not unique even with nbhds")

  kstr <- function(d) do.call(paste, c(lapply(d[key], function(x) ifelse(is.na(x), "NA", as.character(x))),
                                       sep = "|"))
  kb <- kstr(base); kn <- kstr(new)
  cols <- setdiff(union(names(base), names(new)), c(key, AR_CMP_SKIP))

  out <- list()
  add <- function(kind, row, col = NA_character_, old = NA_character_, nw = NA_character_) {
    out[[length(out) + 1]] <<- data.frame(kind = kind, row[key], column = col,
                                          old = old, new = nw, stringsAsFactors = FALSE)
  }
  for (i in which(!kn %in% kb)) add("added", new[i, , drop = FALSE])
  for (i in which(!kb %in% kn)) add("removed", base[i, , drop = FALSE])
  for (i in which(kb %in% kn)) {
    j <- match(kb[i], kn)
    for (cn in cols) {
      o <- if (cn %in% names(base)) base[[cn]][i] else NA
      n <- if (cn %in% names(new))  new[[cn]][j]  else NA
      if (is.na(o) && is.na(n)) next
      if (!is.na(o) && !is.na(n) && identical(as.character(o), as.character(n))) next
      if (!is.na(o) && !is.na(n) && is.numeric(o) && is.numeric(n) && o == n) next
      kind <- if (cn %in% AR_CMP_TOTAL) "total_mismatch"
              else if (is.na(o) && cn %in% AR_CMP_FILL) "filled"
              else if (is.na(o)) "other_filled"
              else if (is.na(n)) "cleared"
              else "changed"
      add(kind, base[i, , drop = FALSE], cn, format(o, digits = 15), format(n, digits = 15))
    }
  }
  if (!length(out))
    return(data.frame(kind = character(0), spec_area = integer(0), column = character(0),
                      allowed = logical(0)))
  d <- do.call(rbind, out)
  d$allowed <- d$kind == "filled" |
               (d$kind != "total_mismatch" & d$spec_area %in% allowed_specs)
  d
}

# Counts per kind, split into allowed / failing
ar_compare_counts <- function(d) {
  k <- factor(d$kind, levels = AR_CMP_KINDS)
  data.frame(kind = AR_CMP_KINDS,
             allowed = as.integer(table(k[d$allowed])),
             failing = as.integer(table(k[!d$allowed])))
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(here))
  base_path <- here::here("data", "cache", "area_report_baseline_main_2026.rds")
  if (!file.exists(base_path)) stop("run ad_hoc/area_report_baseline_2026.R first")
  base <- readRDS(base_path)

  assign("AREA_REPORT_DEFINE_ONLY", TRUE, envir = .GlobalEnv)
  source(here::here("scripts", "ml", "area_report_import.R"))
  imp <- ar_import_year(2026L)
  ar_print_coverage(imp$coverage)

  d <- ar_compare_actuals(base, imp$actuals)
  cat("\n2026 differences vs baseline (", attr(base, "baseline_rev"), "):\n", sep = "")
  if (!nrow(d)) cat("  none\n") else print(d[order(d$allowed, d$kind), ], row.names = FALSE)
  cat("\nCounts by kind:\n")
  print(ar_compare_counts(d), row.names = FALSE)
  if (any(!d$allowed)) stop(sum(!d$allowed), " unexpected difference(s)")
  cat("\nOK: totals identical in every shared row; other differences are filled ",
      "land/imps or spec ", paste(AR_ALLOWED_DIFF_SPECS, collapse = ", "), " rows.\n", sep = "")
}
