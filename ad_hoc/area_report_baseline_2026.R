# area_report_baseline_2026.R ----------------------------------------------
# Capture the PRE-CHANGE 2026 area-report import as the regression baseline
# for feature/area-reports-2019 (see AREA_REPORT_FORMATS.md, "Testing").
#
# The import script is taken from git at BASELINE_REV (main before the
# multi-year rewrite), not from the working tree, so this can be run at any
# time and still reproduces the old behaviour.  That script does not recurse,
# so the 2026 residential/ and commercial/ PDFs are copied into one flat
# temporary folder first ("commercial/280.pdf" -> "commercial__280.pdf";
# same-numbered files in the two subfolders would otherwise collide).
#
#   Rscript ad_hoc/area_report_baseline_2026.R
#
# Output: data/cache/area_report_baseline_main_2026.rds (gitignored)
# ---------------------------------------------------------------------------
suppressPackageStartupMessages(library(here))
if (!requireNamespace("pdftools", quietly = TRUE)) stop("needs {pdftools}")

BASELINE_REV <- "376df49"
src_dir  <- here::here("data", "kca", "area_reports", "2026")
out_path <- here::here("data", "cache", "area_report_baseline_main_2026.rds")

rel <- list.files(src_dir, pattern = "\\.pdf$", recursive = TRUE, ignore.case = TRUE)
if (!length(rel)) stop("no PDFs under ", src_dir)

tmp_root <- file.path(tempdir(), "ar_baseline")
unlink(tmp_root, recursive = TRUE)
flat <- file.path(tmp_root, "2026")
dir.create(flat, recursive = TRUE)
flat_names <- gsub("/", "__", rel, fixed = TRUE)
stopifnot(!anyDuplicated(tolower(flat_names)))
stopifnot(all(file.copy(file.path(src_dir, rel), file.path(flat, flat_names))))
message("Flat copy: ", length(rel), " PDFs -> ", flat)

# Old script, with its hard-coded report root pointed at the flat copy
old <- system2("git", c("-C", shQuote(here::here()), "show",
                        paste0(BASELINE_REV, ":scripts/ml/area_report_import.R")),
               stdout = TRUE)
if (!is.null(attr(old, "status"))) stop("git show failed for ", BASELINE_REV)
old <- paste(old, collapse = "\n")
needle <- 'here::here("data", "kca", "area_reports",'
stopifnot(lengths(regmatches(old, gregexpr(needle, old, fixed = TRUE))) == 1L)
old <- sub(needle, 'file.path(Sys.getenv("AR_BASELINE_ROOT"),', old, fixed = TRUE)
old_file <- file.path(tmp_root, "area_report_import_main.R")
writeLines(old, old_file)

Sys.setenv(AR_BASELINE_ROOT = tmp_root)
assign("area_reports_year", 2026L, envir = .GlobalEnv)
assign("cache_dir",  tmp_root, envir = .GlobalEnv)
assign("output_dir", tmp_root, envir = .GlobalEnv)
if (!exists("safe_write_csv", envir = .GlobalEnv))
  assign("safe_write_csv", function(x, path, ...) utils::write.csv(x, path, row.names = FALSE),
         envir = .GlobalEnv)
if (exists("area_report_actuals", envir = .GlobalEnv))
  rm("area_report_actuals", envir = .GlobalEnv)

source(old_file, local = .GlobalEnv)
if (!exists("area_report_actuals", envir = .GlobalEnv)) stop("old import produced no rows")
base <- get("area_report_actuals", envir = .GlobalEnv)

# The baseline is only usable if every report family made it through
have <- c(
  res        = any(base$prop_type == "res"),
  com_geo    = any(base$prop_type == "com" & base$report_kind == "geo"),
  specialty  = any(base$report_kind == "specialty"),
  apartments = any(base$spec_area %in% 100L & base$report_kind %in% c("specialty", "specialty_region")),
  condo      = any(base$prop_type == "condo")
)
print(have)
if (!all(have)) stop("baseline is missing: ", paste(names(have)[!have], collapse = ", "),
                     " - not saving it")

attr(base, "baseline_rev")   <- BASELINE_REV
attr(base, "baseline_files") <- rel
saveRDS(base, out_path)
message("Baseline saved: ", out_path, " (", nrow(base), " rows)")
