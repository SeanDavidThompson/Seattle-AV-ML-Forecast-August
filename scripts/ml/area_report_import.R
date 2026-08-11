# area_report_import.R -----------------------------------------------------
# Scrape actual assessed-value growth rates from KCA area revalue reports.
#
# Inputs:  PDF area reports placed in
#            here("data", "kca", "area_reports", <area_reports_year>)
#          Two report templates are recognised automatically:
#            (1) Residential revalue reports (e.g. "Area 1 - West Shoreline ...
#                Residential Revalue for 2026 Assessment Roll").  Actuals come
#                from the Executive Summary "Improved Valuation Change Summary"
#                table.  Both the "Sales" and "Pop" rows are captured; values
#                are MEAN AV per parcel.  The population ("Pop") rows are the
#                actual roll growth for the area.
#            (2) Commercial district reports (North/Central/South "Geographic
#                Areas Report ... Commercial Revalue").  Actuals come from the
#                per-Geo-Area "Change in Total Assessed Value" / "Population -
#                Parcel Summary Data" table; values are TOTAL AV.
#
# Globals expected in .GlobalEnv (set by run_main_ml()):
#   area_reports_year   assessment year of the reports (e.g. 2026)
#   cache_dir, output_dir
#
# Output:
#   area_report_actuals (tibble) assigned to .GlobalEnv, cached to
#   <cache_dir>/area_report_actuals_<year>.rds and written to
#   <output_dir>/area_report_actuals_<year>.csv
#
# Columns:
#   assessment_yr  int    e.g. 2026 (values are as of 1/1 of this year)
#   prop_type      chr    "res" | "com"  (which report template it came from)
#   area           int    KCA appraisal area / geo area code
#   basis          chr    "population" (full roll) | "sales" (ratio sample,
#                         res reports only)
#   av_prev        num    prior-year AV  (res = mean per parcel; com = total)
#   av_curr        num    current-year AV (same measure as av_prev)
#   delta          num    av_curr - av_prev
#   pct_change     num    reported % change as a decimal (e.g. -0.047)
#   pct_change_chk num    delta / av_prev, recomputed for validation
#   district       chr    first title line of the report
#   source_file    chr    PDF file name
# ---------------------------------------------------------------------------

if (!requireNamespace("pdftools", quietly = TRUE))
  stop("area_report_import.R requires the {pdftools} package.\n",
       "Install with: install.packages(\"pdftools\")")

area_reports_year <- if (exists("area_reports_year", envir = .GlobalEnv))
  get("area_reports_year", envir = .GlobalEnv) else 2026L
cache_dir  <- get("cache_dir",  envir = .GlobalEnv)
output_dir <- get("output_dir", envir = .GlobalEnv)

area_reports_dir <- here::here("data", "kca", "area_reports",
                               as.character(area_reports_year))

# ---- Helpers --------------------------------------------------------------

# "$1,143,400" / "-$57,400" / "$ -15,497,275" -> numeric
# (strip everything but digits/dot; sign detected separately since the minus
#  can precede or follow the dollar sign in these reports)
.num <- function(x) {
  # Specialty reports render negatives in parentheses: "$ (1,889,223,800)"
  neg  <- grepl("-", x) | grepl("\\(", x)
  sign <- ifelse(neg, -1, 1)
  sign * as.numeric(gsub("[^0-9.]", "", x))
}

# Extract the numbered lines of a pdf as a character vector
.pdf_lines <- function(path) {
  txt <- pdftools::pdf_text(path)
  unlist(strsplit(paste(txt, collapse = "\n"), "\n"))
}

.report_title <- function(lines) {
  non_empty <- trimws(lines[nzchar(trimws(lines))])
  if (length(non_empty) == 0) return(NA_character_)
  non_empty[1]
}

# ---- Parser: residential revalue reports ----------------------------------
# Table rows look like (layout-preserved text):
#   Area 1 Sales  $1,200,800  $1,143,400  -$57,400  -4.8%  $1,255,900  91.5% ...
#   Area 1 Pop    $1,145,200  $1,091,700  -$53,500  -4.7%
parse_res_report <- function(lines, src) {
  pat <- paste0(
    "^\\s*Area\\s+(\\d+)\\s+(Sales|Pop)\\s+",
    "(-?\\$?\\s?[\\d,]+)\\s+",      # prev value
    "(-?\\$?\\s?[\\d,]+)\\s+",      # curr value
    "([-+]?\\$?\\s?[-+]?[\\d,]+)\\s+",  # $ change (sign can precede or follow $)
    "([-+]?[\\d.]+)\\s?%"               # % change
  )
  m <- regmatches(lines, regexec(pat, lines, perl = TRUE))
  m <- m[lengths(m) == 7]
  if (length(m) == 0) return(NULL)

  purrr::map_dfr(m, function(g) {
    tibble::tibble(
      prop_type   = "res",
      report_kind = "geo",
      spec_area   = NA_integer_,
      spec_sub    = NA_integer_,
      area        = as.integer(g[2]),
      basis      = ifelse(g[3] == "Pop", "population", "sales"),
      av_prev    = .num(g[4]),
      av_curr    = .num(g[5]),
      delta      = .num(g[6]),
      pct_change = .num(g[7]) / 100,
      source_file = src
    )
  })
}

# ---- Parser: commercial district reports ----------------------------------
# The per-area table follows a header line:
#   Geo Area   2025 Total Value   2026 Total Value   $ Change   % Change
# with rows:
#   10   $3,962,976,475   $3,947,479,200   -$15,497,275   -0.39%
# and terminates at the "Total" row.
parse_com_report <- function(lines, src) {
  hdr_idx <- grep("Geo\\s*Area\\s+\\d{4}\\s+Total Value", lines)
  if (length(hdr_idx) == 0) return(NULL)

  row_pat <- paste0(
    "^\\s*(\\d{1,3})\\s+",
    "(-?\\$\\s?[\\d,]+)\\s+",
    "(-?\\$\\s?[\\d,]+)\\s+",
    "(-?\\$\\s?-?[\\d,]+|\\$\\s?-[\\d,]+)\\s+",
    "(-?[\\d.]+)\\s?%"
  )

  out <- list()
  for (h in hdr_idx) {
    i <- h + 1
    while (i <= length(lines)) {
      ln <- lines[i]
      if (grepl("^\\s*Total", ln)) break                 # summary row -> stop
      g <- regmatches(ln, regexec(row_pat, ln, perl = TRUE))[[1]]
      if (length(g) == 6) {
        out[[length(out) + 1]] <- tibble::tibble(
          prop_type   = "com",
          report_kind = "geo",
          spec_area   = NA_integer_,
          spec_sub    = NA_integer_,
          area        = as.integer(g[2]),
          basis      = "population",
          av_prev    = .num(g[3]),
          av_curr    = .num(g[4]),
          delta      = .num(g[5]),
          pct_change = .num(g[6]) / 100,
          source_file = src
        )
      } else if (!grepl("^\\s*$", ln)) {
        break                                            # non-table line -> stop
      }
      i <- i + 1
    }
  }
  if (length(out) == 0) return(NULL)
  dplyr::bind_rows(out)
}

# ---- Parser: commercial SPECIALTY reports ---------------------------------
# e.g. "Major Office Buildings / Area: 280 / Commercial Revalue for 2026
# Assessment Roll".  These cover a countywide population (Major Office 280,
# Major Retail 250, Warehouses 500, Hotels 160, ...) valued by a specialty
# appraiser, and are the correct growth source for parcels carrying that
# spec_area.  The geographic district reports EXCLUDE these parcels.
#
# Captured:
#   (a) the specialty-wide total from "CHANGE IN TOTAL ASSESSED VALUE"
#   (b) per-submarket rows from the "Specialty Area Breakdown" table, so a
#       Seattle-only rate can be built instead of the countywide headline
parse_spec_report <- function(lines, src) {
  head_txt <- paste(lines[1:min(120, length(lines))], collapse = " ")

  sa <- regmatches(head_txt,
                   regexec("(?:Specialty(?:\\s+Area)?|Area:)\\s*(\\d{2,3})",
                           head_txt, perl = TRUE))[[1]]
  if (length(sa) < 2) {
    sa2 <- regmatches(src, regexec("(\\d{2,3})", src))[[1]]
    if (length(sa2) < 2) return(NULL)
    spec_no <- as.integer(sa2[2])
  } else {
    spec_no <- as.integer(sa[2])
  }

  out <- list()

  # (a) specialty-wide total -------------------------------------------------
  tot_hdr <- grep("CHANGE IN TOTAL ASSESSED VALUE", lines, ignore.case = TRUE)
  tot_pat <- paste0(
    "^\\s*(\\$\\s*[\\d,]+)\\s+",
    "(\\$\\s*[\\d,]+)\\s+",
    "(\\$?\\s*\\(?-?[\\d,]+\\)?)\\s+",
    "(-?[\\d.]+)\\s?%"
  )
  for (h in tot_hdr) {
    for (i in seq(h + 1, min(h + 6, length(lines)))) {
      g <- regmatches(lines[i], regexec(tot_pat, lines[i], perl = TRUE))[[1]]
      if (length(g) == 5) {
        out[[length(out) + 1]] <- tibble::tibble(
          prop_type   = "com",
          report_kind = "specialty",
          spec_area   = spec_no,
          spec_sub    = NA_integer_,
          area        = NA_integer_,
          basis       = "population",
          av_prev     = .num(g[2]),
          av_curr     = .num(g[3]),
          delta       = .num(g[4]),
          pct_change  = .num(g[5]) / 100,
          source_file = src
        )
        break
      }
    }
    if (length(out) > 0) break
  }

  # (b) per-submarket rows:
  #   "280-120 Central Business District 69 $ 7,457,718,200 $ 108,082,872 -8.66%"
  sub_pat <- paste0(
    "^\\s*(\\d{2,3})[-\\s]+(\\d{3})\\s+",
    ".*?\\$\\s*([\\d,]+)\\s+",
    "\\$\\s*[\\d,]+\\s+",
    "(-?[\\d.]+)\\s?%"
  )
  m <- regmatches(lines, regexec(sub_pat, lines, perl = TRUE))
  m <- m[lengths(m) == 5]
  for (g in m) {
    if (as.integer(g[2]) != spec_no) next
    out[[length(out) + 1]] <- tibble::tibble(
      prop_type   = "com",
      report_kind = "specialty_submarket",
      spec_area   = spec_no,
      spec_sub    = as.integer(g[3]),
      area        = NA_integer_,
      basis       = "population",
      av_prev     = NA_real_,
      av_curr     = .num(g[4]),
      delta       = NA_real_,
      pct_change  = .num(g[5]) / 100,
      source_file = src
    )
  }

  if (length(out) == 0) return(NULL)
  # The breakdown table is reprinted on the per-region pages; keep one row per
  # submarket so the duplicate check downstream stays meaningful.
  dplyr::distinct(dplyr::bind_rows(out),
                  report_kind, spec_area, spec_sub, .keep_all = TRUE)
}

# ---- Main -----------------------------------------------------------------

message("\n--- area_report_import.R (assessment year ", area_reports_year, ") ---")

if (!dir.exists(area_reports_dir)) {
  warning("Area report directory not found: ", area_reports_dir,
          "\nNo actuals imported — area_report_actuals will not be created.",
          call. = FALSE)
} else {

  pdf_files <- list.files(area_reports_dir, pattern = "\\.pdf$",
                          full.names = TRUE, ignore.case = TRUE)
  if (length(pdf_files) == 0)
    warning("No PDFs found in ", area_reports_dir, call. = FALSE)

  parsed <- purrr::map_dfr(pdf_files, function(f) {
    src   <- basename(f)
    lines <- tryCatch(.pdf_lines(f), error = function(e) {
      warning("Could not read ", src, ": ", conditionMessage(e), call. = FALSE)
      character(0)
    })
    if (length(lines) == 0) return(NULL)

    head_txt <- paste(lines[1:min(200, length(lines))], collapse = " ")
    is_res  <- grepl("Residential Revalue", head_txt, ignore.case = TRUE)
    is_com  <- grepl("Commercial Revalue",  head_txt, ignore.case = TRUE)
    # Specialty reports also say "Commercial Revalue", so test for them first.
    is_spec <- grepl("Specialty", head_txt, ignore.case = TRUE) &&
               !grepl("Geographic Areas Report", head_txt, ignore.case = TRUE)

    res <- if (is_spec) parse_spec_report(lines, src)
           else if (is_res) parse_res_report(lines, src)
           else if (is_com) parse_com_report(lines, src)
           else {
             # Unknown template: try each parser
             out <- parse_res_report(lines, src)
             if (is.null(out)) out <- parse_com_report(lines, src)
             if (is.null(out)) out <- parse_spec_report(lines, src)
             out
           }

    if (is.null(res) || nrow(res) == 0) {
      warning("No area growth table recognised in: ", src, call. = FALSE)
      return(NULL)
    }

    res$district <- .report_title(lines)
    message("  \u2705 ", src, ": ",
            dplyr::n_distinct(res$area), " areas (",
            unique(res$prop_type), ")")
    res
  })

  if (nrow(parsed) > 0) {

    area_report_actuals <- parsed |>
      dplyr::mutate(
        assessment_yr  = as.integer(area_reports_year),
        pct_change_chk = delta / av_prev
      ) |>
      dplyr::select(assessment_yr, prop_type, report_kind,
                    area, spec_area, spec_sub, basis,
                    av_prev, av_curr, delta, pct_change, pct_change_chk,
                    district, source_file) |>
      dplyr::arrange(prop_type, report_kind, area, spec_area, spec_sub, basis)

    # ---- Validation -------------------------------------------------------
    # Reported vs recomputed % change (res reports round means to $100s,
    # so allow modest tolerance)
    bad_pct <- area_report_actuals |>
      dplyr::filter(!is.na(pct_change_chk),
                    abs(pct_change - pct_change_chk) > 0.005)
    if (nrow(bad_pct) > 0)
      warning("Reported %-change deviates >0.5pp from recomputed value for ",
              nrow(bad_pct), " row(s) — check parsing:\n",
              paste0("  area ", bad_pct$area, " (", bad_pct$source_file, ")",
                     collapse = "\n"),
              call. = FALSE)

    dupes <- area_report_actuals |>
      dplyr::count(prop_type, report_kind, area, spec_area, spec_sub, basis) |>
      dplyr::filter(n > 1)
    if (nrow(dupes) > 0)
      warning("Duplicate entries across reports (same kind/area/basis): ",
              paste0(dupes$report_kind, "-",
                     dplyr::coalesce(dupes$area, dupes$spec_area),
                     collapse = ", "),
              call. = FALSE)

    # Which specialty populations do we have rates for this cycle?
    spec_have <- sort(unique(stats::na.omit(
      area_report_actuals$spec_area[
        area_report_actuals$report_kind == "specialty"])))
    if (length(spec_have))
      message("  specialty rates parsed for areas: ",
              paste(spec_have, collapse = ", "))
    else
      message("  NOTE: no specialty reports found - specialty parcels will ",
              "follow `specialty_actuals_policy`, NOT the geographic rate")

    # ---- Persist ----------------------------------------------------------
    assign("area_report_actuals", area_report_actuals, envir = .GlobalEnv)

    rds_path <- file.path(cache_dir,
                          paste0("area_report_actuals_", area_reports_year, ".rds"))
    saveRDS(area_report_actuals, rds_path)
    message("  \U1f4be cached: ", basename(rds_path))

    csv_path <- file.path(output_dir,
                          paste0("area_report_actuals_", area_reports_year, ".csv"))
    safe_write_csv(area_report_actuals, csv_path)
    message("  \U1f4be csv: ", basename(csv_path))

    # ---- Summary ----------------------------------------------------------
    smry <- area_report_actuals |>
      dplyr::filter(basis == "population") |>
      dplyr::group_by(prop_type) |>
      dplyr::summarise(
        n_areas  = dplyr::n_distinct(area),
        min_pct  = min(pct_change),
        med_pct  = median(pct_change),
        max_pct  = max(pct_change),
        .groups  = "drop"
      )
    message("  Actual ", area_reports_year - 1, "\u2192", area_reports_year,
            " AV growth (population basis):")
    for (i in seq_len(nrow(smry)))
      message(sprintf("    %-4s %2d areas | min %+.1f%% | median %+.1f%% | max %+.1f%%",
                      smry$prop_type[i], smry$n_areas[i],
                      100 * smry$min_pct[i], 100 * smry$med_pct[i],
                      100 * smry$max_pct[i]))
  }
}

message("area_report_import.R complete.")
