# area_report_import.R -----------------------------------------------------
# Scrape actual assessed-value growth rates from KCA area revalue reports.
# Formats by assessment year (2019-2026) are documented in
# AREA_REPORT_FORMATS.md; the F-numbers below refer to its sections.
#
# Inputs:  PDF area reports placed in
#            here("data", "kca", "area_reports", <year>, "residential" | "commercial")
#          Both subfolders are searched (recursively).  The subfolder is only a
#          hint: the report type is decided from the cover page (F1), and a
#          disagreement is warned about.  Report templates:
#            (1) Residential revalue reports.  Two layouts, both in use
#                2021-2024 (F4):
#                  - multi-area: "Area N Sales|Sale|Pop" rows of the Executive
#                    Summary "Improved Valuation Change Summary" table;
#                  - single-area: "Sales - ..." and "Population - Improved ..."
#                    value blocks, area number from the cover ("Area: 048").
#                Values are MEAN AV per parcel.  The population rows are the
#                actual roll growth for the area.
#            (2) Commercial geographic reports (F5).  Values are TOTAL AV.
#                  - "Geo Area  YYYY Total Value ..." table (2021-2024, 2025
#                    Central);
#                  - one area per PDF with a population value block, area
#                    number from the cover (2019-2020);
#                  - one "Change in Total Assessed Value" block per area,
#                    assigned to the nearest preceding area heading (2025 North).
#            (3) Commercial specialty reports (Major Office 280, Apartments 100,
#                ...).  Headline from "CHANGE IN TOTAL ASSESSED VALUE" or a
#                population value block; per-submarket rows; for Apartments one
#                row per region with the neighborhoods it covers.  A file may
#                cover several specialties (153 & 174, F7).
#            (4) Residential condominium reports (Specialty 700).  Population
#                and Sales value blocks (MEAN AV per living unit); `nbhds` lists
#                the condo neighborhoods the report covers.
#
# Globals expected in .GlobalEnv (set by run_main_ml()):
#   area_reports_year   assessment year of the reports (e.g. 2026)
#   cache_dir, output_dir
# Set AREA_REPORT_DEFINE_ONLY <- TRUE in .GlobalEnv to only define the
# functions (tests, multi-year runs via ar_import_year()).
#
# Output:
#   area_report_actuals (tibble) assigned to .GlobalEnv, cached to
#   <cache_dir>/area_report_actuals_<year>.rds and written to
#   <output_dir>/area_report_actuals_<year>.csv.  The per-file coverage table
#   (every PDF found, parsed or not, with the reason) goes to
#   <output_dir>/area_report_coverage_<year>.csv.
#
# Columns:
#   assessment_yr  int    folder year, e.g. 2026 (values are as of 1/1 of this year)
#   report_year    int    year printed on the cover ("for 2026 Assessment Roll");
#                         a file whose cover year differs from the folder is not parsed
#   tax_yr         int    assessment_yr + 1 (the tax year the roll is levied for)
#   prop_type      chr    "res" | "com" | "condo" (which report template it came from)
#   report_kind    chr    "geo" | "specialty" | "specialty_submarket" |
#                         "specialty_region" | "condo"
#   area           int    KCA appraisal area / geo area code (NA for condo/specialty)
#   spec_area      int    specialty area (700 for condo reports)
#   spec_sub       int    specialty submarket
#   spec_region    int    specialty region (Apartments: 1 Central/North, 2 South, 3 East)
#   basis          chr    "population" (full roll) | "sales" (ratio sample,
#                         res and condo reports)
#   av_prev        num    prior-year AV  (res/condo = mean per parcel; com = total)
#   av_curr        num    current-year AV (same measure as av_prev)
#   delta          num    av_curr - av_prev
#   pct_change     num    reported % change as a decimal (e.g. -0.047)
#   pct_change_chk num    delta / av_prev, recomputed for validation
#   land_prev, land_curr, imps_prev, imps_curr   num  component AV (where reported)
#   pct_land, pct_imps    num  reported component % change as a decimal
#   n_parcels      int    improved parcels in the population (condo only)
#   nbhds          chr    comma-separated neighborhoods covered (condo reports and
#                         specialty regions)
#   area_name      chr    report area / region name, e.g. "Capitol Hill", "South"
#   report_id      chr    path under the year folder without extension,
#                         e.g. "residential/700_01", "commercial/280"
#   district       chr    first title line of the report
#   source_file    chr    path under the year folder, e.g. "commercial/280.pdf"
# ---------------------------------------------------------------------------

# ---- Constants ------------------------------------------------------------

# Commercial specialty areas.  Any 3-digit area number on a commercial cover is
# routed as a specialty; numbers missing from this list are warned about, never
# treated as geographic (geographic areas are 2-digit in every report seen).
KCA_SPECIALTY_AREAS <- c(100L, 153L, 160L, 174L, 250L, 280L, 413L, 500L, 510L,
                         520L, 608L, 625L, 700L)

# Reports known to lack a usable table.  Such a file is recorded as
# "known_gap" in the coverage table; its figures are never imputed.
AR_KNOWN_GAPS <- data.frame(
  year      = 2019L,
  spec_area = 100L,
  reason    = "Apartments report has no summary table",
  stringsAsFactors = FALSE
)

# Value-block headers (F3).  Separators between words vary ("-", "–", ":").
.SEP <- "\\s*[^\\w\\s]?\\s*"
AR_HDR <- list(
  pop = c(paste0("Total\\s+Population", .SEP, "Parcel\\s+Summary\\s+Data"),
          "TOTAL\\s+POPULATION\\s+SUMMARY\\s+DATA",
          "Parcel\\s+Summary\\s+Data",
          paste0("Population", .SEP, "Improved\\s+Valuation\\s+Change\\s+Summary"),
          paste0("Population", .SEP, "Improved\\s+Parcel\\s+Summary")),
  sales  = paste0("Sales", .SEP, "Improved\\s+Valuation\\s+Change\\s+Summary"),
  change = "Change\\s+in\\s+Total\\s+Assessed\\s+Value",
  spec   = "Population\\s*.?\\s*Value\\s+Summary"
)

# Money and percent tokens.  A sign may sit on either side of "$", with or
# without spaces; negatives may be in parentheses.  Without "$" a number needs
# thousands separators, so years and ratios are not mistaken for money.
.AR_MONEY <- "\\(?[-+]?\\s*\\$\\s*[-+]?\\s*\\(?[\\d,]*\\d\\)?|\\(?[-+]?\\(?\\d{1,3}(?:,\\d{3})+\\)?"
.AR_PCT   <- "\\(?[-+]?\\s*\\d*\\.?\\d+\\)?(?=\\s?%)"

# ---- Helpers --------------------------------------------------------------

# "$1,143,400" / "-$57,400" / "$ -15,497,275" / "- $ 504,585,300" -> numeric
# (strip everything but digits/dot; sign detected separately since the minus
#  can precede or follow the dollar sign in these reports)
.num <- function(x) {
  # Specialty reports render negatives in parentheses: "$ (1,889,223,800)"
  neg  <- grepl("-", x) | grepl("\\(", x)
  sign <- ifelse(neg, -1, 1)
  sign * as.numeric(gsub("[^0-9.]", "", x))
}

.pdf_pages <- function(path) pdftools::pdf_text(path)

# Pages -> lines.  The Unicode minus sign is folded to "-" so .num() sees it.
.split_lines <- function(pages) {
  x <- unlist(strsplit(paste(pages, collapse = "\n"), "\n"))
  gsub("−", "-", x, fixed = TRUE)
}

.pdf_lines <- function(path) .split_lines(.pdf_pages(path))

.squish <- function(x) gsub("\\s+", " ", paste(x, collapse = " "))

# The first n pages that carry text (a blank or image-only first page is skipped)
.cover_pages <- function(pages, n = 1L) {
  keep <- which(nchar(gsub("\\s", "", pages)) >= 40)
  pages[utils::head(keep, n)]
}

.report_title <- function(lines) {
  non_empty <- trimws(lines[nzchar(trimws(lines))])
  if (length(non_empty) == 0) return(NA_character_)
  non_empty[1]
}

# Area number from a cover: "Area: 048", "Area: 25", "AREA 36"
.cover_area <- function(cover) {
  g <- regmatches(cover, regexec("(?i)\\bArea\\s*:?\\s*0*(\\d{1,3})\\b", cover, perl = TRUE))[[1]]
  if (length(g) == 2) as.integer(g[2]) else NA_integer_
}

# ---- Classification (F1, F9) ------------------------------------------------
# Decides the report type from the cover page.  Returns
#   kind        "condo" | "spec" | "res" | "com_geo" | "unknown"
#   family      "residential" | "commercial" | NA (which subfolder it belongs in)
#   spec_nos    integer specialty numbers (kind == "spec")
#   cover_year  "for YYYY Assessment Roll", NA if absent
#   notes       character warnings (unlisted specialty, subfolder mismatch)
ar_classify <- function(pages, subfolder = NA_character_) {
  cover <- .squish(.split_lines(.cover_pages(pages, 1L)))
  head2 <- .squish(.split_lines(.cover_pages(pages, 2L)))
  notes <- character(0)

  yr_re <- "(?i)for\\s+(\\d{4})\\s+Assessment\\s+Roll"
  yr <- regmatches(cover, regexec(yr_re, cover, perl = TRUE))[[1]]
  if (length(yr) < 2) yr <- regmatches(head2, regexec(yr_re, head2, perl = TRUE))[[1]]
  cover_year <- if (length(yr) == 2) as.integer(yr[2]) else NA_integer_

  # Condo covers: "Residential Condominium" (all years); 2019 says
  # "Specialty 700" with no colon.
  is_condo <- grepl("(?i)Residential\\s+Condominium", head2, perl = TRUE) ||
              grepl("(?i)\\bSpecialty\\s*700\\b", cover, perl = TRUE)
  is_com   <- grepl("(?i)Commercial\\s+Revalue", head2, perl = TRUE)
  is_res   <- grepl("(?i)Residential\\s+Revalue", head2, perl = TRUE)

  # Specialty only on a 3-digit area number, never on the bare word
  # "specialty" (every commercial cover letter says "geographic or specialty").
  spec_nos <- integer(0)
  if (!is_condo && is_com &&
      !grepl("(?i)Geographic\\s+Areas?\\s+Report", cover, perl = TRUE)) {
    num <- "\\d{3}(?!\\d|[.,]\\d|\\s?%)"   # not part of a longer number or a percent
    m <- regmatches(cover, regexpr(paste0(
      "(?i)\\b(?:Specialty(?:\\s+Areas?)?|Areas?)\\s*:?\\s*", num,
      "(?:\\s*(?:&|and|,|/)\\s*", num, ")*"), cover, perl = TRUE))
    if (length(m)) spec_nos <- unique(as.integer(regmatches(m, gregexpr("\\d{3}", m))[[1]]))
    unl <- setdiff(spec_nos, KCA_SPECIALTY_AREAS)
    if (length(unl))
      notes <- c(notes, paste0("specialty ", paste(unl, collapse = ","),
                               " is not in KCA_SPECIALTY_AREAS"))
  }

  kind <- if (is_condo) "condo" else if (length(spec_nos)) "spec" else if (is_res) "res"
          else if (is_com) "com_geo" else "unknown"
  family <- switch(kind, condo = , res = "residential", spec = , com_geo = "commercial",
                   NA_character_)
  if (!is.na(subfolder) && !is.na(family) && !identical(tolower(subfolder), family))
    notes <- c(notes, sprintf("content says %s (%s) but file is in %s/", family, kind, subfolder))

  list(kind = kind, family = family, spec_nos = spec_nos, cover_year = cover_year,
       cover = cover, notes = notes)
}

# ---- Value blocks (F3) ------------------------------------------------------
# Two value rows plus a change row, land / imps / total or total only:
#   Population - Improved Parcel Summary Data:
#     2025 Value        $175,500      $365,100        $540,600
#     2026 Value        $170,000      $339,600        $509,600
#   Percent Change         -3.1%         -7.0%           -5.7%
# Row labels "YYYY Value(s)" / "YYYY Valuation"; change labels "% Change",
# "Percent Change", "Value Increase".

.vt_value_row <- function(ln) {
  m <- regmatches(ln, regexec("^\\s*(\\d{4})\\s+Val(?:ue|uation)s?\\b:?(.*)$", ln, perl = TRUE))[[1]]
  if (length(m) != 3) return(NULL)
  tok <- regmatches(m[3], gregexpr(.AR_MONEY, m[3], perl = TRUE))[[1]]
  v <- if (length(tok) >= 3) .num(tok[1:3]) else if (length(tok) == 1) .num(tok) else return(NULL)
  list(year = as.integer(m[2]), v = v)
}

.vt_change_row <- function(ln) {
  m <- regmatches(ln, regexec("(?i)^\\s*(?:%\\s*Change|Percent\\s+Change|Value\\s+Increase)\\b:?(.*)$",
                              ln, perl = TRUE))[[1]]
  if (length(m) != 2) return(NULL)
  p <- regmatches(m[2], gregexpr(.AR_PCT, m[2], perl = TRUE))[[1]]
  if (!length(p)) return(NULL)          # "Value Increase" in dollars: recompute
  .num(p) / 100
}

# All value blocks following any of the `hdr` patterns.  Each element:
#   prev, curr, pct (land, imps, total; NA where not reported),
#   pct_printed, hdr_line, years.
# pct = printed figure, or recomputed from the value rows when not printed.
# Warns when the printed and recomputed rates differ by more than 0.5pp.
.value_tables <- function(lines, hdr, src = "", window = 14L, not = NULL) {
  hdr_re <- paste0("(?i)(?:", paste(hdr, collapse = "|"), ")")
  h <- grep(hdr_re, lines, perl = TRUE)
  if (!is.null(not)) h <- h[!grepl(not, lines[h], ignore.case = TRUE, perl = TRUE)]
  out <- list()
  for (hi in h) {
    if (hi >= length(lines)) next
    win  <- lines[seq(hi + 1L, min(hi + window, length(lines)))]
    rows <- Filter(Negate(is.null), lapply(win, .vt_value_row))
    if (length(rows) < 2) next
    yrs <- vapply(rows, `[[`, 0L, "year")
    if (length(unique(yrs)) < 2) next
    prev <- rows[[which.min(yrs)]]$v
    curr <- rows[[which.max(yrs)]]$v
    if (length(prev) != length(curr)) next
    if (length(prev) == 1) { prev <- c(NA, NA, prev); curr <- c(NA, NA, curr) }

    chg <- NULL
    for (ln in win) { chg <- .vt_change_row(ln); if (!is.null(chg)) break }
    pp <- rep(NA_real_, 3)
    if (!is.null(chg)) {
      if (!is.na(prev[1])) { if (length(chg) >= 3) pp <- chg[1:3] }
      else pp[3] <- chg[1]
    }
    calc <- curr / prev - 1
    off  <- which(!is.na(pp) & !is.na(calc) & abs(pp - calc) > 0.005)
    for (k in off)
      warning(sprintf("%s: printed %s %% change %+.2f%% differs from recomputed %+.2f%% (line %d)",
                      src, c("land", "imps", "total")[k], 100 * pp[k], 100 * calc[k], hi),
              call. = FALSE)
    out[[length(out) + 1]] <- list(prev = prev, curr = curr,
                                   pct = dplyr::coalesce(pp, calc), pct_printed = pp,
                                   hdr_line = hi, years = range(yrs))
  }
  out
}

.value_table <- function(lines, hdr, src = "", window = 14L, not = NULL) {
  vt <- .value_tables(lines, hdr, src, window, not)
  if (length(vt)) vt[[1]] else NULL
}

# Value block -> the value columns of an output row
.vt_cols <- function(tb) {
  tibble::tibble(
    av_prev    = tb$prev[3],
    av_curr    = tb$curr[3],
    delta      = tb$curr[3] - tb$prev[3],
    pct_change = tb$pct[3],
    land_prev  = tb$prev[1],
    land_curr  = tb$curr[1],
    imps_prev  = tb$prev[2],
    imps_curr  = tb$curr[2],
    pct_land   = tb$pct[1],
    pct_imps   = tb$pct[2]
  )
}

# ---- Parser: residential revalue reports (F4) ------------------------------
# Multi-area layout (layout-preserved text):
#   Area 1 Sales  $1,200,800  $1,143,400  -$57,400  -4.8%  $1,255,900  91.5% ...
#   Area 1 Pop    $1,145,200  $1,091,700  -$53,500  -4.7%
# ("Sale" instead of "Sales" in 2024 Area 22).  Otherwise the single-area
# layout: Sales / Population value blocks, area number from the cover.
parse_res_report <- function(lines, src, cover = "") {
  pat <- paste0(
    "^\\s*Area\\s+(\\d+)\\s+(Sales?|Pop)\\s+",
    "(-?\\$?\\s?[\\d,]+)\\s+",      # prev value
    "(-?\\$?\\s?[\\d,]+)\\s+",      # curr value
    "([-+]?\\$?\\s?[-+]?[\\d,]+)\\s+",  # $ change (sign can precede or follow $)
    "([-+]?[\\d.]+)\\s?%"               # % change
  )
  m <- regmatches(lines, regexec(pat, lines, perl = TRUE))
  m <- m[lengths(m) == 7]
  if (length(m) == 0) return(.parse_res_single(lines, src, cover))

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

.parse_res_single <- function(lines, src, cover) {
  pop <- .value_table(lines, AR_HDR$pop, src, not = "Sales")
  if (is.null(pop)) return(NULL)
  area <- .cover_area(cover)
  if (is.na(area)) {
    warning(src, ": single-area residential layout but no area number on the cover", call. = FALSE)
    return(NULL)
  }
  sal <- .value_table(lines, AR_HDR$sales, src)
  mk <- function(tb, basis) dplyr::bind_cols(
    tibble::tibble(prop_type = "res", report_kind = "geo", spec_area = NA_integer_,
                   spec_sub = NA_integer_, area = area, basis = basis),
    .vt_cols(tb), tibble::tibble(source_file = src))
  out <- mk(pop, "population")
  if (!is.null(sal)) out <- dplyr::bind_rows(out, mk(sal, "sales"))
  out
}

# ---- Parser: commercial geographic reports (F5) ----------------------------
parse_com_report <- function(lines, src, cover = "") {
  out <- .com_geo_table(lines, src)
  if (!is.null(out)) return(out)

  # 2025 North: one "Change in Total Assessed Value" block per area
  chg <- .value_tables(lines, AR_HDR$change, src)
  if (length(chg) >= 2) return(.com_geo_sections(lines, src, chg))

  # 2019-2020: one area per PDF
  vt <- .value_table(lines, c(AR_HDR$pop, AR_HDR$change), src, not = "Sales")
  if (is.null(vt)) return(NULL)
  area <- .cover_area(cover)
  if (is.na(area)) {
    warning(src, ": single-area commercial layout but no area number on the cover", call. = FALSE)
    return(NULL)
  }
  dplyr::bind_cols(
    tibble::tibble(prop_type = "com", report_kind = "geo", spec_area = NA_integer_,
                   spec_sub = NA_integer_, area = area, basis = "population"),
    .vt_cols(vt), tibble::tibble(source_file = src))
}

# The per-area table follows a header line:
#   Geo Area   2025 Total Value   2026 Total Value   $ Change   % Change
# with rows:
#   10   $3,962,976,475   $3,947,479,200   -$15,497,275   -0.39%
# and terminates at the "Total" row.
.com_geo_table <- function(lines, src) {
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

# Assign each "Change in Total Assessed Value" block to the nearest preceding
# area heading: a bare "Area NN" line starting each area's pages (2025 North:
# 10, 14, 17, 19, 80, 85, 90, 95).  Prose such as "+1.00% in Geographic Area
# 10" is only a cross-check; it is worded differently for some areas.
.com_geo_sections <- function(lines, src, chg, expect_n = 8L) {
  head_re <- "(?i)^\\s*Area\\s+(\\d{1,2})\\s*$"
  hd <- grep(head_re, lines, perl = TRUE)
  hd_area <- as.integer(sub(head_re, "\\1", lines[hd], perl = TRUE))
  if (length(chg) != expect_n)
    warning(sprintf("%s: %d 'Change in Total Assessed Value' sections, expected %d",
                    src, length(chg), expect_n), call. = FALSE)
  out <- list()
  for (tb in chg) {
    k <- which(hd < tb$hdr_line)
    if (!length(k)) {
      warning(sprintf("%s: value section at line %d has no preceding area heading",
                      src, tb$hdr_line), call. = FALSE)
      next
    }
    a <- hd_area[max(k)]
    if (a %in% vapply(out, `[[`, 0L, "area")) {
      warning(sprintf("%s: second value section for area %d (line %d) ignored",
                      src, a, tb$hdr_line), call. = FALSE)
      next
    }
    out[[length(out) + 1]] <- dplyr::bind_cols(
      tibble::tibble(prop_type = "com", report_kind = "geo", spec_area = NA_integer_,
                     spec_sub = NA_integer_, area = a, basis = "population"),
      .vt_cols(tb), tibble::tibble(source_file = src))
  }
  if (!length(out)) return(NULL)
  out <- dplyr::bind_rows(out)

  txt <- .squish(lines)
  pr  <- regmatches(txt, gregexpr("(?i)([+-]?\\s*\\d+(?:\\.\\d+)?)\\s*%\\s+in\\s+Geographic\\s+Area\\s+(\\d{1,2})\\b",
                                  txt, perl = TRUE))[[1]]
  for (p in pr) {
    g  <- regmatches(p, regexec("(?i)([+-]?\\s*\\d+(?:\\.\\d+)?)\\s*%\\s+in\\s+Geographic\\s+Area\\s+(\\d{1,2})",
                                p, perl = TRUE))[[1]]
    a  <- as.integer(g[3]); v <- .num(g[2]) / 100
    got <- out$pct_change[out$area == a]
    if (!length(got))
      warning(sprintf("%s: prose gives %+.2f%% for Geographic Area %d but no section was found",
                      src, 100 * v, a), call. = FALSE)
    else if (abs(got[1] - v) > 1e-4)
      warning(sprintf("%s: Geographic Area %d section %+.2f%% vs prose %+.2f%%",
                      src, a, 100 * got[1], 100 * v), call. = FALSE)
  }
  out
}

# ---- Specialty helpers -------------------------------------------------------
# "Percent Change - Total Values" by region (2021: "Summary - Total Value -
# % Change"), plus the region -> neighborhood inventory.  Returns NULL when the
# report has neither.
.spec_regions <- function(lines) {
  j <- grep(paste0("(?i)Percent\\s+Change\\s*.?\\s*Total\\s+Values",
                   "|Summary", .SEP, "Total\\s+Value", .SEP, "%\\s*Change"),
            lines, perl = TRUE)
  if (!length(j)) return(NULL)
  win <- seq(j[1] + 1, min(j[1] + 10, length(lines)))
  reg <- list()
  for (i in win) {
    g <- regmatches(lines[i], regexec("^\\s*([A-Za-z][A-Za-z /&-]*?)\\s+(\\(?[-+]?[\\d.]+\\)?)\\s?%\\s*$",
                                      lines[i], perl = TRUE))[[1]]
    if (length(g) == 3 && !grepl("^Region", g[2], ignore.case = TRUE))
      reg[[length(reg) + 1]] <- data.frame(region = trimws(g[2]), pct = .num(g[3]) / 100,
                                           code = NA_integer_)
  }
  # Rows labelled by number only: "Region 1 (Central)   -5.52%"
  if (!length(reg)) for (i in win) {
    g <- regmatches(lines[i], regexec("(?i)^\\s*Region\\s+(\\d)\\b\\s*\\(?([A-Za-z /&-]*?)\\)?\\s+(\\(?[-+]?[\\d.]+\\)?)\\s?%\\s*$",
                                      lines[i], perl = TRUE))[[1]]
    if (length(g) == 4)
      reg[[length(reg) + 1]] <- data.frame(
        region = if (nzchar(trimws(g[3]))) trimws(g[3]) else paste("Region", g[2]),
        pct = .num(g[4]) / 100, code = as.integer(g[2]))
  }
  if (!length(reg)) return(NULL)
  reg <- do.call(rbind, reg)
  county <- reg$pct[grepl("^County", reg$region, ignore.case = TRUE)]
  reg <- reg[!grepl("^County", reg$region, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(reg)) return(list(regions = NULL, county = county))

  # Region codes: "divided ... into three regions: Central (1), South (2), East (3)"
  # or (2021) "Region 1 (Central)"
  txt  <- paste(lines, collapse = " ")
  defs <- regmatches(txt, gregexpr("([A-Z][a-z]+)\\s*\\((\\d)\\)", txt, perl = TRUE))[[1]]
  code_of <- setNames(as.integer(sub(".*\\((\\d)\\)", "\\1", defs)),
                      tolower(sub("\\s*\\(.*", "", defs)))
  defs2 <- regmatches(txt, gregexpr("Region\\s+(\\d)\\s*\\(([A-Za-z/ ]+)\\)", txt, perl = TRUE))[[1]]
  if (length(defs2))
    code_of <- c(code_of, setNames(as.integer(sub("Region\\s+(\\d).*", "\\1", defs2, perl = TRUE)),
                                   tolower(trimws(sub(".*\\(([^)]*)\\).*", "\\1", defs2)))))
  for (k in seq_len(nrow(reg))) {
    if (!is.na(reg$code[k])) next
    hit <- names(code_of)[vapply(names(code_of), function(nm)
      grepl(nm, tolower(reg$region[k]), fixed = TRUE), logical(1))]
    if (length(hit)) reg$code[k] <- code_of[[hit[1]]]
  }
  reg$code[is.na(reg$code)] <- seq_len(nrow(reg))[is.na(reg$code)]   # fall back to order

  reg$nbhds <- NA_character_
  nb <- .spec_inventory(lines)
  if (!is.null(nb) && nrow(nb))
    for (k in seq_len(nrow(reg)))
      reg$nbhds[k] <- paste(sort(nb$nbhd[nb$code == reg$code[k]]), collapse = ",")
  list(regions = reg, county = county)
}

# Neighborhood inventory: columns headed R1 / R2 / R3.  2022-2026 list
# "number name projects" triples with "R 1 Total: 4,652" footers; 2021 lists
# number-name pairs with no project counts (and no footer check).
.spec_inventory <- function(lines) {
  inv <- grep(paste0("(?i)Project\\s+Inventory|Inventory", .SEP, "Regions\\s+and\\s+Neighbou?rhoods"),
              lines, perl = TRUE)
  if (!length(inv)) return(NULL)
  trip <- "(\\d{1,3})\\s+([A-Za-z][A-Za-z /.&'-]*?)\\s+(\\d[\\d,]*)(?=\\s|$)"
  pair <- "(?<![\\d,])(\\d{1,3})\\s+([A-Za-z][A-Za-z/.&'-]*(?:\\s[A-Za-z/.&'-]+)*)"

  hdr_i <- inv[1] + which(grepl("\\bR1\\b.*\\bR2\\b", lines[(inv[1] + 1):min(inv[1] + 5, length(lines))]))[1]
  if (!is.na(hdr_i)) {
    hdr   <- lines[hdr_i]
    cuts  <- vapply(paste0("\\bR", 2:9, "\\b"), function(pt) regexpr(pt, hdr, perl = TRUE)[1], integer(1))
    cuts  <- cuts[cuts > 0] - 5
    end_i <- hdr_i + which(grepl("Total\\s*:", lines[(hdr_i + 1):length(lines)]))[1]
    if (is.na(end_i)) end_i <- min(hdr_i + 60, length(lines))
    scan <- function(re, counts) {
      nb <- list()
      for (i in seq(hdr_i + 1, end_i - 1)) {
        ln <- lines[i]
        mm <- gregexpr(re, ln, perl = TRUE)[[1]]
        if (mm[1] < 0) next
        for (q in seq_along(mm)) {
          seg   <- substr(ln, mm[q], mm[q] + attr(mm, "match.length")[q] - 1)
          piece <- regmatches(seg, regexec(re, seg, perl = TRUE))[[1]]
          nb[[length(nb) + 1]] <- data.frame(
            code = 1L + sum(mm[q] >= cuts), nbhd = as.integer(piece[2]),
            projects = if (counts) as.integer(gsub(",", "", piece[4])) else NA_integer_)
        }
      }
      if (length(nb)) do.call(rbind, nb) else NULL
    }
    nb <- scan(trip, TRUE)
    if (!is.null(nb)) {
      # Check against the "R 1 Total: 4,652" footers
      tot <- regmatches(lines[end_i], gregexpr("R\\s*(\\d)\\s*Total:\\s*([\\d,]+)", lines[end_i], perl = TRUE))[[1]]
      for (t in tot) {
        rc <- as.integer(sub("R\\s*(\\d).*", "\\1", t))
        want <- as.integer(gsub(",", "", sub(".*Total:\\s*", "", t)))
        got  <- sum(nb$projects[nb$code == rc])
        if (!identical(got, want))
          warning(sprintf("Region %d inventory parsed %d projects, report total is %d", rc, got, want),
                  call. = FALSE)
      }
      return(nb)
    }
    nb <- scan(pair, FALSE)
    if (!is.null(nb)) message("    inventory has no project counts - region total check skipped")
    return(nb)
  }

  # 2021: columns headed "Region 1  Region 2  Region 3" over "NHD #  NHD Name",
  # number-name pairs three per line ("5 Downtown   160 Seward Park   340
  # Mercer Island"), no project counts and no totals footer.  Column edges
  # come from the "NHD #" sub-header when present, else the Region headers.
  near  <- seq(inv[1] + 1, min(inv[1] + 5, length(lines)))
  hdr_i <- near[grepl("(?i)Region\\s+1\\b.*Region\\s+2\\b", lines[near], perl = TRUE)][1]
  if (is.na(hdr_i)) return(NULL)
  starts <- function(ln, re) as.integer(gregexpr(re, ln, perl = TRUE)[[1]])
  cuts  <- starts(lines[hdr_i], "(?i)Region\\s+[2-9]\\b") - 5L
  first <- hdr_i + 1L
  if (first <= length(lines) && grepl("(?i)NHD\\s*#", lines[first], perl = TRUE)) {
    s <- starts(lines[first], "(?i)NHD\\s*#")
    if (length(s) == length(cuts) + 1L) cuts <- s[-1] - 2L
    first <- first + 1L
  }
  nb <- list(); seen <- FALSE
  for (i in seq(first, min(first + 80L, length(lines)))) {
    ln <- lines[i]
    if (!nzchar(trimws(ln))) next
    mm <- gregexpr(pair, ln, perl = TRUE)[[1]]
    if (mm[1] < 0) { if (seen) break else next }        # end of the table
    seen <- TRUE
    for (q in seq_along(mm)) {
      seg <- substr(ln, mm[q], mm[q] + attr(mm, "match.length")[q] - 1)
      nb[[length(nb) + 1]] <- data.frame(code = 1L + sum(mm[q] >= cuts),
                                         nbhd = as.integer(sub("^(\\d+).*", "\\1", seg)),
                                         projects = NA_integer_)
    }
  }
  if (!length(nb)) return(NULL)
  message("    inventory has no project counts - region total check skipped")
  do.call(rbind, nb)
}

# Split a multi-specialty report into per-specialty line sets at headings
# "Specialty Area 153 - ..." or "Spec 153" (F7).  Lines before the first
# heading belong to no specialty.
.spec_segments <- function(lines, spec_nos) {
  re  <- "(?i)^\\s*(?:Specialty\\s+Area\\s+(\\d{3})\\s*[-–]|Spec\\.?\\s+(\\d{3})\\b)"
  hit <- regmatches(lines, regexec(re, lines, perl = TRUE))
  num <- vapply(hit, function(g) if (length(g) == 3) as.integer(paste0(g[2], g[3])) else NA_integer_,
                integer(1))
  num[!num %in% spec_nos] <- NA_integer_
  owner <- num
  for (i in seq_along(owner)[-1]) if (is.na(owner[i])) owner[i] <- owner[i - 1]
  setNames(lapply(spec_nos, function(s) lines[!is.na(owner) & owner == s]), spec_nos)
}

# ---- Parser: commercial SPECIALTY reports ---------------------------------
# e.g. "Major Office Buildings / Area: 280 / Commercial Revalue for 2026
# Assessment Roll".  These cover a countywide population (Major Office 280,
# Major Retail 250, Warehouses 500, Hotels 160, ...) valued by a specialty
# appraiser, and are the correct growth source for parcels carrying that
# spec_area.  The geographic district reports EXCLUDE these parcels.
#
# Captured, per specialty number on the cover:
#   (a) the specialty-wide total from "CHANGE IN TOTAL ASSESSED VALUE", or a
#       population value block
#   (b) per-submarket rows from the "Specialty Area Breakdown" table, so a
#       Seattle-only rate can be built instead of the countywide headline
parse_spec_report <- function(lines, src, spec_nos) {
  if (!length(spec_nos)) {
    warning(src, ": specialty number not resolvable from the cover", call. = FALSE)
    return(NULL)
  }
  segs <- if (length(spec_nos) == 1) setNames(list(lines), spec_nos)
          else .spec_segments(lines, spec_nos)
  out <- list()
  for (s in spec_nos) {
    seg <- segs[[as.character(s)]]
    if (!length(seg)) {
      warning(src, ": no 'Specialty Area ", s, " -' / 'Spec ", s, "' heading", call. = FALSE)
      next
    }
    r <- .parse_spec_one(seg, src, s, sub_lines = lines)
    if (!is.null(r)) out[[length(out) + 1]] <- r
    if (is.null(r) || !any(r$report_kind == "specialty"))
      warning(src, ": spec ", s, ": no headline", call. = FALSE)
  }
  if (!length(out)) return(NULL)
  dplyr::bind_rows(out)
}

.parse_spec_one <- function(lines, src, spec_no, sub_lines = lines) {
  out <- list()

  # (a) specialty-wide total -------------------------------------------------
  #   "$ 14,417,785,000  $ 13,913,199,700  - $ 504,585,300  -3.50%"
  money   <- "([-+]?\\s*\\$\\s*[-+]?\\s*\\(?[\\d,]+\\)?)"
  tot_hdr <- grep("CHANGE IN TOTAL ASSESSED VALUE", lines, ignore.case = TRUE)
  tot_pat <- paste0(
    "^\\s*", money, "\\s+",
    money, "\\s+",
    "([-+]?\\s*\\$?\\s*[-+]?\\s*\\(?[\\d,]+\\)?)\\s+",
    "(\\(?[-+]?\\s*[\\d.]+\\)?)\\s?%"
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

  # (a2) Value table: "Population Value Summary" (Apartments layout), else a
  #      population block or value rows under the "Change in Total Assessed
  #      Value" heading (2022 spec 160: "2021 Values" land/imps/total rows).
  #      Without a headline row it is the headline.  With one (280 in 2019,
  #      153/174 in 2024) the total stays the headline's and only land/imps
  #      come from the table.
  vs <- .value_table(lines, AR_HDR$spec, src)
  if (is.null(vs)) vs <- .value_table(lines, c(AR_HDR$pop, AR_HDR$change), src, not = "Sales")
  if (!is.null(vs)) {
    if (length(out) == 0) {
      out[[length(out) + 1]] <- dplyr::bind_cols(
        tibble::tibble(prop_type = "com", report_kind = "specialty", spec_area = spec_no,
                       spec_sub = NA_integer_, area = NA_integer_, basis = "population"),
        .vt_cols(vs), tibble::tibble(source_file = src))
    } else if (!is.na(vs$prev[1])) {
      hl <- out[[1]]
      if (abs(vs$pct[3] - hl$pct_change) > 0.005)
        warning(sprintf("%s: spec %d headline total %+.2f%% vs value table total %+.2f%%",
                        src, spec_no, 100 * hl$pct_change, 100 * vs$pct[3]), call. = FALSE)
      vc <- .vt_cols(vs)
      out[[1]] <- dplyr::bind_cols(hl[setdiff(names(hl), "source_file")],
                                   vc[c("land_prev", "land_curr", "imps_prev", "imps_curr",
                                        "pct_land", "pct_imps")],
                                   hl["source_file"])
    }
  }

  # (a3) regional rates and their neighborhoods
  rg <- .spec_regions(lines)
  if (!is.null(rg)) {
    if (length(out) == 0 && length(rg$county)) {
      out[[length(out) + 1]] <- tibble::tibble(
        prop_type = "com", report_kind = "specialty", spec_area = spec_no,
        spec_sub = NA_integer_, area = NA_integer_, basis = "population",
        av_prev = NA_real_, av_curr = NA_real_, delta = NA_real_,
        pct_change = rg$county[1], source_file = src)
    }
    if (!is.null(rg$regions)) {
      for (k in seq_len(nrow(rg$regions))) {
        out[[length(out) + 1]] <- tibble::tibble(
          prop_type   = "com",
          report_kind = "specialty_region",
          spec_area   = spec_no,
          spec_sub    = NA_integer_,
          spec_region = as.integer(rg$regions$code[k]),
          area        = NA_integer_,
          basis       = "population",
          av_prev     = NA_real_,
          av_curr     = NA_real_,
          delta       = NA_real_,
          pct_change  = rg$regions$pct[k],
          nbhds       = rg$regions$nbhds[k],
          area_name   = rg$regions$region[k],
          source_file = src
        )
      }
    }
  }

  # (b) per-submarket rows:
  #   "280-120 Central Business District 69 $ 7,457,718,200 $ 108,082,872 -8.66%"
  # Only rows of this report's own (known) specialty count: a neighbourhood
  # LAND table in a geographic report has the same shape (F2).
  sub_pat <- paste0(
    "^\\s*(\\d{2,3})[-\\s]+(\\d{3})\\s+",
    ".*?\\$\\s*([\\d,]+)\\s+",
    "\\$\\s*[\\d,]+\\s+",
    "(-?[\\d.]+)\\s?%"
  )
  m <- regmatches(sub_lines, regexec(sub_pat, sub_lines, perl = TRUE))
  m <- m[lengths(m) == 5]
  for (g in m) {
    sp <- as.integer(g[2])
    if (sp != spec_no || !sp %in% KCA_SPECIALTY_AREAS) next
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
  res <- dplyr::bind_rows(out)
  if (!"spec_region" %in% names(res)) res$spec_region <- NA_integer_
  dplyr::distinct(res, report_kind, spec_area, spec_sub, spec_region, .keep_all = TRUE)
}

# ---- Parser: residential CONDOMINIUM reports (Specialty 700) -------------
# Executive Summary layout (pdftools text):
#   Neighborhoods:                   35, 40, 65, 70, AND 85.
#   ...Sales - Improved Valuation Change Summary
#     2025 Value       $165,700      $414,900      $580,600     $599,000   98.3%   6.86%
#     2026 Value       $160,300      $391,400      $551,700     $599,000   92.7%   5.72%
#      %Change         -3.3%          -5.7%         -5.0%        -5.6%    -16.62%
#   Population - Improved Parcel Summary Data:
#     2025 Value        $175,500      $365,100        $540,600
#     2026 Value        $170,000      $339,600        $509,600
#   Percent Change         -3.1%         -7.0%           -5.7%
#   Number of improved Parcels in the Population: 8,015
parse_condo_report <- function(lines, src) {
  label <- "^\\s*(Neighbou?rhoods?|Areas?)\\s*:"

  # Neighborhood list (prefer the Executive Summary line over the cover line)
  nb_idx <- grep(label, lines, ignore.case = TRUE, perl = TRUE)
  nb_idx <- nb_idx[order(!grepl("^\\s*Neighbou?rhood", lines[nb_idx], ignore.case = TRUE))]
  nbhds <- integer(0)
  for (i in nb_idx) {
    txt <- sub(label, "", lines[i], ignore.case = TRUE, perl = TRUE)
    j <- i + 1                       # lists can wrap onto the next line
    while (j <= length(lines) && nzchar(trimws(lines[j])) &&
           grepl("^[\\s\\d,.&]*(and)?[\\s\\d,.&]*$", lines[j], ignore.case = TRUE, perl = TRUE)) {
      txt <- paste(txt, lines[j]); j <- j + 1
    }
    v <- suppressWarnings(as.integer(regmatches(txt, gregexpr("\\b\\d{1,3}\\b", txt, perl = TRUE))[[1]]))
    if (length(v)) { nbhds <- sort(unique(v)); break }
  }
  if (!length(nbhds)) {
    warning("Condo report without a neighborhood list: ", src, call. = FALSE)
    return(NULL)
  }

  spec <- regmatches(paste(lines, collapse = " "),
                     regexec("Specialty\\s+(\\d{3})\\s*:\\s*Residential Condominium",
                             paste(lines, collapse = " "), perl = TRUE))[[1]]
  spec_no <- if (length(spec) == 2) as.integer(spec[2]) else 700L

  pop <- .value_table(lines, AR_HDR$pop, src, window = 12L, not = "Sales")
  sal <- .value_table(lines, AR_HDR$sales, src, window = 12L)
  if (is.null(pop)) {
    warning("Condo report without a Population summary table: ", src, call. = FALSE)
    return(NULL)
  }

  n_txt <- regmatches(lines, regexec("Number of improved Parcels in the Population:\\s*([\\d,]+)",
                                     lines, ignore.case = TRUE, perl = TRUE))
  n_txt <- n_txt[lengths(n_txt) == 2]
  n_par <- if (length(n_txt)) as.integer(gsub(",", "", n_txt[[1]][2])) else NA_integer_

  mk <- function(tb, basis, n) {
    dplyr::bind_cols(
      tibble::tibble(prop_type = "condo", report_kind = "condo", spec_area = spec_no,
                     spec_sub = NA_integer_, area = NA_integer_, basis = basis),
      .vt_cols(tb),
      tibble::tibble(n_parcels = n, nbhds = paste(nbhds, collapse = ","),
                     area_name = .report_title(lines), source_file = src))
  }
  out <- mk(pop, "population", n_par)
  if (!is.null(sal)) out <- dplyr::bind_rows(out, mk(sal, "sales", NA_integer_))
  out
}

# ---- One file -------------------------------------------------------------
# Never throws.  Returns list(rows, coverage) where coverage is a one-row
# data.frame: year, subfolder, file, kind, cover_year, status, n_rows, reason.
# status: "parsed" | "unparsed" | "known_gap".  `pages` can be passed in
# (tests) instead of reading `path`.
ar_parse_file <- function(path, rel, folder_year, pages = NULL) {
  folder_year <- as.integer(folder_year)
  subfolder <- if (grepl("/", rel, fixed = TRUE)) sub("/.*$", "", rel) else NA_character_
  cov <- data.frame(year = folder_year, subfolder = subfolder, file = rel,
                    kind = NA_character_, cover_year = NA_integer_, status = "unparsed",
                    n_rows = 0L, reason = NA_character_, stringsAsFactors = FALSE)
  done <- function(rows = NULL, reason = NA_character_, status = NULL) {
    cov$n_rows <- if (is.null(rows)) 0L else nrow(rows)
    cov$status <- if (!is.null(status)) status else if (cov$n_rows > 0) "parsed" else "unparsed"
    cov$reason <- reason
    list(rows = rows, coverage = cov)
  }

  if (is.null(pages)) {
    pages <- tryCatch(.pdf_pages(path), error = function(e) e)
    if (inherits(pages, "error")) {
      warning("Could not read ", rel, ": ", conditionMessage(pages), call. = FALSE)
      return(done(reason = paste("read error:", conditionMessage(pages))))
    }
  }
  if (!length(pages) || all(!nzchar(trimws(pages))))
    return(done(reason = "no text layer"))

  cl <- ar_classify(pages, subfolder)
  cov$kind <- cl$kind; cov$cover_year <- cl$cover_year
  for (n in cl$notes) warning(rel, ": ", n, call. = FALSE)

  if (is.na(cl$cover_year)) {
    warning(rel, ": no 'for YYYY Assessment Roll' on the cover", call. = FALSE)
  } else if (cl$cover_year != folder_year) {
    r <- sprintf("cover year %d != folder %d", cl$cover_year, folder_year)
    warning(rel, ": ", r, " - not parsed; move it to the ", cl$cover_year, " folder",
            call. = FALSE)
    return(done(reason = r))
  }

  lines <- .split_lines(pages)
  msgs  <- character(0)
  res <- withCallingHandlers(
    tryCatch(
      switch(cl$kind,
        condo   = parse_condo_report(lines, rel),
        spec    = parse_spec_report(lines, rel, cl$spec_nos),
        res     = parse_res_report(lines, rel, cl$cover),
        com_geo = parse_com_report(lines, rel, cl$cover),
        {
          # Unknown template: try each parser
          out <- parse_res_report(lines, rel, cl$cover)
          if (is.null(out)) out <- parse_com_report(lines, rel, cl$cover)
          out
        }),
      error = function(e) { msgs <<- c(msgs, paste("error:", conditionMessage(e))); NULL }),
    warning = function(w) msgs <<- c(msgs, sub(paste0("^", rel, ":\\s*"), "", conditionMessage(w))))
  notes <- if (length(msgs)) paste(unique(msgs), collapse = "; ") else NA_character_

  gap <- AR_KNOWN_GAPS[AR_KNOWN_GAPS$year == folder_year &
                       AR_KNOWN_GAPS$spec_area %in% cl$spec_nos, , drop = FALSE]
  if (nrow(gap)) {
    has_hl <- !is.null(res) && any(res$report_kind == "specialty" & res$spec_area %in% gap$spec_area)
    if (!has_hl) return(done(reason = gap$reason[1], status = "known_gap"))
    warning(rel, ": parsed although listed in AR_KNOWN_GAPS (", gap$reason[1],
            ") - the known gap is stale", call. = FALSE)
  }

  if (is.null(res) || nrow(res) == 0) {
    r <- dplyr::coalesce(notes, "no area growth table recognised")
    warning("No area growth table recognised in: ", rel, " (routed as ", cl$kind, ")",
            call. = FALSE)
    return(done(reason = r))
  }

  res$report_year <- cl$cover_year
  res$district    <- .report_title(lines)
  res$report_id   <- tools::file_path_sans_ext(rel)
  res$source_file <- rel
  message("  ✅ ", rel, " [", cl$kind, "]: ", .ar_describe(res, cl$kind))
  done(res, reason = notes)
}

.ar_describe <- function(res, kind) {
  pop <- res[res$basis == "population", ]
  if (kind == "condo") {
    sprintf("%s, nbhds %s, %+.1f%% total (land %+.1f%%, imps %+.1f%%), %s units",
            pop$area_name[1], pop$nbhds[1], 100 * pop$pct_change[1],
            100 * pop$pct_land[1], 100 * pop$pct_imps[1],
            format(pop$n_parcels[1], big.mark = ","))
  } else if (kind == "spec") {
    hl  <- res[res$report_kind == "specialty", ]
    rg  <- res[res$report_kind == "specialty_region", ]
    paste0("spec ", paste(unique(res$spec_area), collapse = ","),
           if (nrow(hl)) paste0(",", paste(sprintf(" %+.2f%%", 100 * hl$pct_change), collapse = ""),
                                " total") else "",
           if (nrow(hl) && "pct_land" %in% names(hl) && !is.na(hl$pct_land[1]))
             sprintf(" (land %+.2f%%, imps %+.2f%%)", 100 * hl$pct_land[1], 100 * hl$pct_imps[1]) else "",
           if (nrow(rg)) paste0("; regions: ", paste(sprintf("R%d %s %+.2f%% [%d nbhds]",
             rg$spec_region, rg$area_name, 100 * rg$pct_change,
             lengths(strsplit(dplyr::coalesce(rg$nbhds, ""), ","))), collapse = ", ")) else "",
           "; ", nrow(res), " rows")
  } else {
    paste0(dplyr::n_distinct(res$area), " areas")
  }
}

# ---- One year -------------------------------------------------------------
# Returns list(actuals, coverage).  actuals is NULL when nothing parsed.
ar_import_year <- function(year, root = here::here("data", "kca", "area_reports")) {
  year <- as.integer(year)
  dir  <- file.path(root, as.character(year))
  empty_cov <- data.frame(year = integer(0), subfolder = character(0), file = character(0),
                          kind = character(0), cover_year = integer(0), status = character(0),
                          n_rows = integer(0), reason = character(0), stringsAsFactors = FALSE)
  if (!dir.exists(dir)) {
    warning("Area report directory not found: ", dir, call. = FALSE)
    return(list(actuals = NULL, coverage = empty_cov))
  }
  files <- sort(list.files(dir, pattern = "\\.pdf$", recursive = TRUE, ignore.case = TRUE))
  if (!length(files)) {
    warning("No PDFs found in ", dir, " (searched residential/ and commercial/)", call. = FALSE)
    return(list(actuals = NULL, coverage = empty_cov))
  }
  off <- files[!tolower(sub("/.*$", "", files)) %in% c("residential", "commercial") |
               !grepl("/", files, fixed = TRUE)]
  if (length(off))
    warning(year, ": PDFs outside residential/ or commercial/ (parsed anyway): ",
            paste(off, collapse = ", "), call. = FALSE)

  per <- lapply(files, function(rel) ar_parse_file(file.path(dir, rel), rel, year))
  coverage <- do.call(rbind, lapply(per, `[[`, "coverage"))
  parsed   <- dplyr::bind_rows(lapply(per, `[[`, "rows"))
  if (!nrow(parsed)) return(list(actuals = NULL, coverage = coverage))

  actuals <- parsed |>
    dplyr::mutate(
      assessment_yr  = year,
      report_year    = as.integer(report_year),
      tax_yr         = year + 1L,
      pct_change_chk = delta / av_prev
    ) |>
    dplyr::select(assessment_yr, report_year, tax_yr, prop_type, report_kind,
                  area, spec_area, spec_sub, basis,
                  av_prev, av_curr, delta, pct_change, pct_change_chk,
                  dplyr::any_of(c("spec_region",
                                  "land_prev", "land_curr", "imps_prev", "imps_curr",
                                  "pct_land", "pct_imps", "n_parcels",
                                  "nbhds", "area_name")),
                  report_id, district, source_file) |>
    dplyr::arrange(prop_type, report_kind, area, spec_area, spec_sub,
                   dplyr::across(dplyr::any_of("spec_region")), report_id, basis)
  ar_validate(actuals)
  list(actuals = actuals, coverage = coverage)
}

ar_validate <- function(area_report_actuals) {
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

  # Condo reports: components should add up, and no neighborhood should
  # appear in two reports
  if ("nbhds" %in% names(area_report_actuals)) {
    condo <- area_report_actuals |>
      dplyr::filter(prop_type == "condo")
    if (nrow(condo) > 0) {
      bad_sum <- condo |>
        dplyr::filter(abs(land_prev + imps_prev - av_prev) > 500 |
                      abs(land_curr + imps_curr - av_curr) > 500)
      if (nrow(bad_sum) > 0)
        warning("Condo land + imps != total for: ",
                paste(unique(bad_sum$report_id), collapse = ", "), call. = FALSE)
      nb_all <- unlist(strsplit(condo$nbhds[condo$basis == "population"], ","))
      nb_dup <- unique(nb_all[duplicated(nb_all)])
      if (length(nb_dup) > 0)
        warning("Condo neighborhoods listed in more than one report: ",
                paste(nb_dup, collapse = ", "), call. = FALSE)
    }
  }

  dupes <- area_report_actuals |>
    dplyr::count(prop_type, report_kind, area, spec_area, spec_sub,
                 dplyr::across(dplyr::any_of(c("spec_region", "nbhds"))), basis) |>
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
  invisible(area_report_actuals)
}

# Per-year coverage: PDFs found / parsed / unparsed / known gaps, then every
# file that was not parsed, with its reason.
ar_print_coverage <- function(coverage) {
  if (is.null(coverage) || !nrow(coverage)) {
    message("  coverage: no PDFs found")
    return(invisible(coverage))
  }
  yrs <- sort(unique(coverage$year))
  message(sprintf("  %-4s %6s %7s %9s %10s", "year", "found", "parsed", "unparsed", "known_gap"))
  for (y in yrs) {
    cy <- coverage[coverage$year == y, ]
    message(sprintf("  %-4d %6d %7d %9d %10d", y, nrow(cy), sum(cy$status == "parsed"),
                    sum(cy$status == "unparsed"), sum(cy$status == "known_gap")))
  }
  bad <- coverage[coverage$status != "parsed", ]
  if (nrow(bad)) {
    message("  not parsed:")
    for (i in seq_len(nrow(bad)))
      message(sprintf("    %d %-28s %-9s %s", bad$year[i], bad$file[i], bad$status[i],
                      dplyr::coalesce(bad$reason[i], "")))
  }
  invisible(coverage)
}

# ---- Main -----------------------------------------------------------------

if (!isTRUE(get0("AREA_REPORT_DEFINE_ONLY", envir = .GlobalEnv, inherits = FALSE))) {

  if (!requireNamespace("pdftools", quietly = TRUE))
    stop("area_report_import.R requires the {pdftools} package.\n",
         "Install with: install.packages(\"pdftools\")")

  area_reports_year <- if (exists("area_reports_year", envir = .GlobalEnv))
    get("area_reports_year", envir = .GlobalEnv) else 2026L
  cache_dir  <- get("cache_dir",  envir = .GlobalEnv)
  output_dir <- get("output_dir", envir = .GlobalEnv)

  message("\n--- area_report_import.R (assessment year ", area_reports_year, ") ---")

  .imp <- ar_import_year(area_reports_year)
  ar_print_coverage(.imp$coverage)
  if (nrow(.imp$coverage))
    safe_write_csv(.imp$coverage,
                   file.path(output_dir, paste0("area_report_coverage_", area_reports_year, ".csv")))

  if (is.null(.imp$actuals)) {
    warning("No actuals imported for ", area_reports_year,
            " — area_report_actuals will not be created.", call. = FALSE)
  } else {
    area_report_actuals <- .imp$actuals

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
      dplyr::mutate(cell = dplyr::case_when(
        prop_type == "condo" ~ report_id,
        !is.na(area)         ~ as.character(area),
        TRUE                 ~ paste(report_kind, spec_area, spec_sub,
                                     if ("spec_region" %in% names(area_report_actuals))
                                       spec_region else NA))) |>
      dplyr::group_by(prop_type) |>
      dplyr::summarise(
        n_areas  = dplyr::n_distinct(cell),
        min_pct  = min(pct_change),
        med_pct  = median(pct_change),
        max_pct  = max(pct_change),
        .groups  = "drop"
      )
    message("  Actual ", area_reports_year - 1, "→", area_reports_year,
            " AV growth (population basis):")
    for (i in seq_len(nrow(smry)))
      message(sprintf("    %-5s %2d cells | min %+.1f%% | median %+.1f%% | max %+.1f%%",
                      smry$prop_type[i], smry$n_areas[i],
                      100 * smry$min_pct[i], 100 * smry$med_pct[i],
                      100 * smry$max_pct[i]))
  }
  rm(.imp)
  message("area_report_import.R complete.")
}
