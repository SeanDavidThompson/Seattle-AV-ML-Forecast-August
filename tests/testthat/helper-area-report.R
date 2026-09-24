# Loads the area-report parser functions without running the import.
assign("AREA_REPORT_DEFINE_ONLY", TRUE, envir = .GlobalEnv)
source(here::here("scripts", "ml", "area_report_import.R"), local = TRUE)

AR_ROOT <- here::here("data", "kca", "area_reports")

# Synthetic "pages": one character string per page, lines separated by "\n"
ar_pages <- function(...) vapply(list(...), function(p) paste(p, collapse = "\n"), "")

# Parse a synthetic report end to end (classification, year check, parser)
ar_parse_text <- function(pages, rel = "commercial/test.pdf", year = 2026L) {
  suppressMessages(ar_parse_file(NA_character_, rel, year, pages = pages))
}

# Import a real year once per test run
.ar_year_cache <- new.env()
ar_year <- function(y) {
  key <- as.character(y)
  if (is.null(.ar_year_cache[[key]]))
    .ar_year_cache[[key]] <- suppressWarnings(suppressMessages(ar_import_year(y, AR_ROOT)))
  .ar_year_cache[[key]]
}

skip_if_no_reports <- function(y) {
  skip_if_not_installed("pdftools")
  skip_if_not(dir.exists(file.path(AR_ROOT, y)),
              paste0("no area reports for ", y, " under data/kca/area_reports"))
}
