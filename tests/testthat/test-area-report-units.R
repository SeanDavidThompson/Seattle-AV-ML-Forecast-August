# String-based checks of the area-report parser (no PDFs needed).
# F-numbers refer to AREA_REPORT_FORMATS.md.

com_letter <- c(
  "This report covers parcels valued by the geographic or specialty",
  "appraisal teams of the Department of Assessments.")

# ---- F1 routing ---------------------------------------------------------------

test_that("F1: 'geographic or specialty' cover letter does not make a specialty", {
  cl <- ar_classify(ar_pages(c("Area: 25", "Commercial Revalue for 2020 Assessment Roll", com_letter)),
                    "commercial")
  expect_equal(cl$kind, "com_geo")
  expect_equal(cl$cover_year, 2020L)
  expect_length(cl$notes, 0)
  cl <- ar_classify(ar_pages(c("AREA 36", "Commercial Revalue for 2019 Assessment Roll", com_letter)))
  expect_equal(cl$kind, "com_geo")
})

test_that("F1: specialty needs a 3-digit area number on a commercial cover", {
  cl <- ar_classify(ar_pages(c("Major Office Buildings", "Area: 280",
                               "Commercial Revalue for 2026 Assessment Roll", com_letter)))
  expect_equal(cl$kind, "spec"); expect_equal(cl$spec_nos, 280L)
  cl <- ar_classify(ar_pages(c("Specialty Area 250 Major Retail",
                               "Commercial Revalue for 2020 Assessment Roll")))
  expect_equal(cl$spec_nos, 250L)
  cl <- ar_classify(ar_pages(c("Warehouses", "Area: 500,", "Commercial Revalue for 2026 Assessment Roll")))
  expect_equal(cl$spec_nos, 500L)
})

test_that("F1: an unlisted 3-digit number is still a specialty, with a note", {
  cl <- ar_classify(ar_pages(c("Area: 999", "Commercial Revalue for 2026 Assessment Roll")))
  expect_equal(cl$kind, "spec")
  expect_match(cl$notes, "999 is not in KCA_SPECIALTY_AREAS")
})

test_that("F1: condo covers route as condo, with or without a colon", {
  cl <- ar_classify(ar_pages(c("Capitol Hill", "Specialty 700",
                               "Residential Condominium Revalue for 2019 Assessment Roll")))
  expect_equal(cl$kind, "condo")
  cl <- ar_classify(ar_pages(c("Specialty 700: Residential Condominium",
                               "for 2026 Assessment Roll")))
  expect_equal(cl$kind, "condo")
  cl <- ar_classify(ar_pages(c("Downtown", "Residential Condominium Revalue for 2021 Assessment Roll")))
  expect_equal(cl$kind, "condo")
})

test_that("F1: residential cover with 'Area: 048' is residential, area 48", {
  cl <- ar_classify(ar_pages(c("Area: 048", "Residential Revalue for 2019 Assessment Roll")),
                    "residential")
  expect_equal(cl$kind, "res")
  expect_equal(.cover_area(cl$cover), 48L)
})

test_that("F1: content disagreeing with the subfolder is noted", {
  cl <- ar_classify(ar_pages(c("Area: 048", "Residential Revalue for 2019 Assessment Roll")),
                    "commercial")
  expect_match(cl$notes, "content says residential")
})

# ---- F2 submarket guard -------------------------------------------------------

test_that("F2: neighbourhood rows are not submarkets of an unknown specialty", {
  lines <- c("36-010 Downtown Land 41 $ 1,200,000,000 $ 30,000,000 5.00%",
             "36-020 Pioneer Sq  12 $ 300,000,000 $ 20,000,000 4.00%")
  expect_null(suppressWarnings(parse_spec_report(lines, "x", 36L)))
  lines <- c(lines, "280-120 Central Business District 69 $ 7,457,718,200 $ 108,082,872 -8.66%")
  r <- suppressWarnings(parse_spec_report(lines, "x", 280L))
  expect_equal(r$spec_sub, 120L)
  expect_equal(r$pct_change, -0.0866)
})

# ---- F3 value blocks ----------------------------------------------------------

vt_block <- function(hdr, row = "Value", chg = "% Change", pct = c("-6.05%", "-5.65%", "-5.75%")) {
  c("  intro text", hdr,
    "                  Land             Improvements            Total",
    sprintf("  2025 %-10s $25,448,216,024   $74,488,754,374   $99,936,970,398", row),
    sprintf("  2026 %-10s $23,909,798,440   $70,282,266,551   $94,192,064,991", row),
    "  Difference  ($1,538,417,584)  ($4,206,487,823)  ($5,744,905,407)",
    sprintf("  %-14s %s   %s   %s", chg, pct[1], pct[2], pct[3]))
}

test_that("F3: every header / row / change label is read", {
  hdrs <- c("Total Population - Parcel Summary Data", "TOTAL POPULATION SUMMARY DATA",
            "Parcel Summary Data", "Population - Improved Valuation Change Summary",
            "Population – Improved Parcel Summary", "Change in Total Assessed Value",
            "Population Value Summary")
  pats <- c(AR_HDR$pop, AR_HDR$change, AR_HDR$spec)
  for (h in hdrs) for (rw in c("Value", "Values", "Valuation"))
    for (ch in c("% Change", "Percent Change", "Value Increase")) {
      tb <- .value_table(vt_block(h, rw, ch), pats)
      expect_false(is.null(tb), info = paste(h, rw, ch))
      expect_equal(tb$prev[3], 99936970398, info = paste(h, rw, ch))
      expect_equal(tb$curr[1], 23909798440)
      expect_equal(tb$pct, c(-0.0605, -0.0565, -0.0575))
    }
})

test_that("F3: rate is recomputed when no % row, and a disagreement warns", {
  lines <- vt_block("Parcel Summary Data", chg = "Difference2", pct = c("", "", ""))
  tb <- .value_table(lines, AR_HDR$pop)
  expect_equal(tb$pct[3], 94192064991 / 99936970398 - 1)
  expect_true(all(is.na(tb$pct_printed)))
  expect_warning(.value_table(vt_block("Parcel Summary Data", pct = c("-6.05%", "-5.65%", "-2.00%")),
                              AR_HDR$pop, "f.pdf"),
                 "printed total % change -2.00% differs from recomputed -5.75%")
})

test_that("F3: total-only block", {
  lines <- c("Change in Total Assessed Value",
             "  2024 Value    $3,962,976,475",
             "  2025 Value    $4,002,606,240",
             "  % Change      +1.00%")
  tb <- .value_table(lines, AR_HDR$change)
  expect_equal(tb$prev[3], 3962976475)
  expect_true(is.na(tb$prev[1]))
  expect_equal(tb$pct[3], 0.01)
})

# ---- F4 residential -----------------------------------------------------------

test_that("F4: multi-area rows accept 'Sale'", {
  lines <- c("Area 22 Sale   $1,200,800  $1,289,700  $88,900  7.4%  $1,255,900  91.5%",
             "Area 22 Pop    $1,145,200  $1,229,900  $84,700  7.4%")
  r <- parse_res_report(lines, "x")
  expect_equal(r$basis, c("sales", "population"))
  expect_equal(r$area, c(22L, 22L))
})

test_that("F4: single-area layout, area from the cover", {
  pages <- ar_pages(
    c("Area: 048", "Residential Revalue for 2019 Assessment Roll", "Seattle, Washington"),
    c("Sales - Improved Valuation Change Summary",
      "              Land       Imps       Total",
      "  2018 Value  $300,000   $500,000   $800,000",
      "  2019 Value  $310,000   $500,000   $810,000",
      "  % Change      3.3%       0.0%       1.3%",
      "Population - Improved Parcel Summary:",
      "              Land       Imps       Total",
      "  2018 Value  $290,000   $459,000   $749,000",
      "  2019 Value  $290,000   $450,300   $740,300",
      "  Percent Change   0.0%   -1.9%     -1.2%"))
  out <- ar_parse_text(pages, "residential/048.pdf", 2019L)
  expect_equal(out$coverage$status, "parsed")
  r <- out$rows
  pop <- r[r$basis == "population", ]
  expect_equal(pop$area, 48L)
  expect_equal(pop$pct_land, 0); expect_equal(pop$pct_imps, -0.019)
  expect_equal(r$report_id[1], "residential/048")
  expect_equal(r$report_year[1], 2019L)
})

# ---- F5 commercial geographic -------------------------------------------------

test_that("F5: single-area commercial report (2019-2020)", {
  pages <- ar_pages(
    c("AREA 36", "Commercial Revalue for 2019 Assessment Roll", com_letter),
    c("Total Population - Parcel Summary Data",
      "               Land              Improvements        Total",
      "  2018 Value   $1,000,000,000    $500,000,000        $1,500,000,000",
      "  2019 Value   $1,108,800,000    $566,350,000        $1,675,150,000",
      "  % Change        10.88%            13.27%             11.68%"))
  out <- ar_parse_text(pages, "commercial/036.pdf", 2019L)
  r <- out$rows
  expect_equal(r$area, 36L)
  expect_equal(r$report_kind, "geo")
  expect_equal(r$pct_land, 0.1088)
})

test_that("F5: 2025 North sections go to the nearest preceding area heading", {
  sec <- function(a, prev, curr, pct) c(
    sprintf("Area %d", a), sprintf("Area %d covers somewhere; see Area 99 below.", a),
    "Change in Total Assessed Value",
    sprintf("  2024 Value   %s", prev), sprintf("  2025 Value   %s", curr),
    sprintf("  %% Change     %s", pct))
  pages <- ar_pages(
    c("North Geographic Areas Report", "Commercial Revalue for 2025 Assessment Roll"),
    c("Values rose +1.00% in Geographic Area 10 and 13.63% in Geographic Area 14.",
      sec(10, "$3,962,976,475", "$4,002,606,240", "+1.00%"),
      sec(14, "$1,000,000,000", "$1,136,300,000", "13.63%")))
  expect_warning(out <- ar_parse_text(pages, "commercial/north.pdf", 2025L), "2 'Change in Total")
  r <- out$rows
  expect_equal(r$area, c(10L, 14L))
  expect_equal(r$pct_change, c(0.01, 0.1363))

  bad <- sub("13.63% in", "12.00% in", pages[2], fixed = TRUE)
  expect_warning(suppressMessages(.com_geo_sections(
    .split_lines(c(pages[1], bad)), "n",
    .value_tables(.split_lines(c(pages[1], bad)), AR_HDR$change), expect_n = 2L)),
    "Area 14 section \\+13.63% vs prose \\+12.00%")
})

# ---- F6 headline signs --------------------------------------------------------

test_that("F6: signed headline in every observed form", {
  forms <- c("  $ 14,417,785,000  $ 13,913,199,700  - $ 504,585,300  -3.50%",
             "  $8,082,452,750   $7,646,000,000   -$436,452,750   -5.40%",
             "  $ 14,417,785,000  $ 13,913,199,700  $ (504,585,300)  (3.50)%",
             "  $ 14,417,785,000  $ 13,913,199,700  $ -504,585,300  -3.50%")
  for (f in forms) {
    r <- suppressWarnings(parse_spec_report(c("CHANGE IN TOTAL ASSESSED VALUE",
                                              "  2025 Total   2026 Total   $ Change   % Change", f),
                                            "x", 500L))
    expect_equal(nrow(r), 1L, info = f)
    expect_lt(r$delta, 0)
    expect_lt(r$pct_change, 0)
  }
})

# ---- F7 multi-specialty -------------------------------------------------------

test_that("F7: one headline per specialty in a combined file", {
  pages <- ar_pages(
    c("Specialty Areas 153 & 174", "Commercial Revalue for 2024 Assessment Roll"),
    c("Specialty Area 153 - Special Purpose",
      "CHANGE IN TOTAL ASSESSED VALUE",
      "  $ 1,000,000,000   $ 1,054,500,000   $ 54,500,000   5.45%",
      "Specialty Area 174 - Parking",
      "CHANGE IN TOTAL ASSESSED VALUE",
      "  $ 500,000,000   $ 492,000,000   -$ 8,000,000   -1.60%"))
  cl <- ar_classify(pages)
  expect_equal(cl$spec_nos, c(153L, 174L))
  out <- ar_parse_text(pages, "commercial/153_174.pdf", 2024L)
  hl <- out$rows[out$rows$report_kind == "specialty", ]
  expect_equal(hl$spec_area, c(153L, 174L))
  expect_equal(hl$pct_change, c(0.0545, -0.016))
  expect_equal(unique(hl$report_id), "commercial/153_174")
})

# ---- F8 apartments ------------------------------------------------------------

test_that("F8: 2021 apartments wording", {
  lines <- c(
    "The county is divided into Region 1 (Central), Region 2 (South) and Region 3 (East).",
    "Summary - Total Value - % Change",
    "   Central/North   -5.52%",
    "   South            7.89%",
    "   East             0.72%",
    "   County          -1.43%")
  r <- suppressWarnings(parse_spec_report(lines, "x", 100L))
  expect_equal(r$pct_change[r$report_kind == "specialty"], -0.0143)
  rg <- r[r$report_kind == "specialty_region", ]
  expect_equal(rg$spec_region, 1:3)
  expect_equal(rg$pct_change, c(-0.0552, 0.0789, 0.0072))
})

test_that("F8: 2019 apartments is a known gap, never imputed", {
  pages <- ar_pages(c("Apartments", "Area: 100", "Commercial Revalue for 2019 Assessment Roll"),
                    c("No summary table here."))
  out <- suppressWarnings(ar_parse_text(pages, "commercial/100.pdf", 2019L))
  expect_equal(out$coverage$status, "known_gap")
  expect_null(out$rows)
})

# ---- F9 cover year ------------------------------------------------------------

test_that("F9: a report whose cover year differs from the folder is not parsed", {
  pages <- ar_pages(c("Area: 280", "Commercial Revalue for 2021 Assessment Roll"),
                    c("CHANGE IN TOTAL ASSESSED VALUE",
                      "  $ 1,000,000,000   $ 983,800,000   -$ 16,200,000   -1.62%"))
  expect_warning(out <- ar_parse_text(pages, "commercial/280.pdf", 2022L), "cover year 2021 != folder 2022")
  expect_equal(out$coverage$status, "unparsed")
  expect_equal(out$coverage$reason, "cover year 2021 != folder 2022")
  out <- suppressWarnings(ar_parse_text(pages, "commercial/280.pdf", 2021L))
  expect_equal(out$coverage$status, "parsed")
  expect_equal(out$rows$report_year, 2021L)
})

# ---- Coverage / schema --------------------------------------------------------

test_that("unparseable files are recorded, not skipped", {
  out <- suppressWarnings(ar_parse_text(ar_pages(c("Area: 12", "Commercial Revalue for 2020 Assessment Roll"),
                                                 "nothing useful"), "commercial/012.pdf", 2020L))
  expect_equal(out$coverage$status, "unparsed")
  expect_false(is.na(out$coverage$reason))
  out <- ar_parse_text(ar_pages(""), "commercial/blank.pdf", 2020L)
  expect_equal(out$coverage$reason, "no text layer")
})

# ---- F8 2021 inventory --------------------------------------------------------

test_that("F8: 2021 inventory, Region columns over NHD # / NHD Name pairs", {
  lines <- c(
    "The county is divided into Region 1 (Central), Region 2 (South), Region 3 (East).",
    "Summary - Total Value - % Change",
    "   Central/North   -5.52%",
    "   South            7.89%",
    "   East             0.72%",
    "   County          -1.43%",
    "Inventory - Regions and Neighborhoods",
    "      Region 1                  Region 2                    Region 3",
    "  NHD #  NHD Name          NHD #  NHD Name            NHD #  NHD Name",
    "  5      Downtown          160    Seward Park         340    Mercer Island",
    "  10     Capitol Hill      165    Rainier Beach       345    Bellevue CBD",
    "  15     Queen Anne                                   350    Kirkland",
    "",
    "Market Overview")
  r  <- suppressMessages(suppressWarnings(parse_spec_report(lines, "x", 100L)))
  rg <- r[r$report_kind == "specialty_region", ]
  expect_equal(rg$nbhds, c("5,10,15", "160,165", "340,345,350"))
})

# ---- Specialty headline + value table -----------------------------------------

spec_tbl <- c(
  "Total Population - Parcel Summary Data",
  "               Land              Improvements        Total",
  "  2018 Value   $1,000,000,000    $500,000,000        $1,500,000,000",
  "  2019 Value   $1,131,500,000    $455,800,000        $1,587,300,000",
  "  Percent Change   13.15%           -8.84%            5.82%")

test_that("2020 spec 250 layout: value table only (no headline row)", {
  r <- suppressWarnings(parse_spec_report(spec_tbl, "x", 250L))
  expect_equal(r$pct_land, 0.1315); expect_equal(r$pct_imps, -0.0884)
  expect_equal(r$pct_change, 0.0582)
})

test_that("2022 spec 160 layout: value rows under the Change in Total heading", {
  lines <- c("CHANGE IN TOTAL ASSESSED VALUE",
             "               Land              Improvements        Total",
             "  2021 Values  $1,000,000,000    $2,000,000,000      $3,000,000,000",
             "  2022 Values  $1,049,800,000    $2,323,400,000      $3,373,200,000",
             "  % Change        4.98%             16.17%              12.44%")
  r <- suppressWarnings(parse_spec_report(lines, "x", 160L))
  expect_equal(r$pct_change, 0.1244)
  expect_equal(r$pct_land, 0.0498); expect_equal(r$pct_imps, 0.1617)
})

test_that("headline total wins; land/imps come from the value table; mismatch warns", {
  hl <- c("CHANGE IN TOTAL ASSESSED VALUE",
          "  $ 1,500,000,000   $ 1,587,300,000   $ 87,300,000   5.82%")
  r <- suppressWarnings(parse_spec_report(c(hl, spec_tbl), "x", 280L))
  expect_equal(r$pct_change, 0.0582); expect_equal(r$delta, 87300000)
  expect_equal(r$pct_land, 0.1315)
  hl2 <- sub("5.82%", "9.00%", hl, fixed = TRUE)
  expect_warning(r <- parse_spec_report(c(hl2, spec_tbl), "f.pdf", 280L),
                 "spec 280 headline total \\+9.00% vs value table total \\+5.82%")
  expect_equal(r$pct_change, 0.09)
})

# ---- Fitted to pdf_text() of the real reports ----------------------------------
# Lines copied from pdftools::pdf_text() output (2026-09-24).

test_that("2019 condo 700_01: cover and mid-line neighborhood label", {
  p1 <- c("            Specialty 700", "        Residential Condominium", "",
          "      Annual Mass Appraisal Report", "                    of:", "",
          "       Capitol Hill", "         Specialty Neighborhoods", "",
          "          35, 40, 65, 70, AND 85.", "", "        2019 Assessment Roll", "",
          "       For 2019 Property Taxes", "King County Department of Assessments")
  p7 <- c("                                   Executive Summary Report",
          "Appraisal Date: 1/1/2019- 2019 Assessment Roll",
          "Area Name / Number: Capitol Hill; Neighborhoods: 35, 40, 65, 70, and 85.",
          "Sales – Improved Valuation Change Summary",
          "  2018 Value      $121,100        $404,800       $525,900        $599,800       87.7%       6.82%",
          "  2019 Value      $140,100        $410,700       $550,800        $599,800       92.3%       5.48%",
          "    Change        +$19,000        +$5,900        +$24,900                                   -1.34%",
          "   %Change         +15.7%          +1.5%           +4.7%                        +4.6%      -19.64%",
          "Population - Improved Parcel Summary Data:",
          "    2018 Value       $128,500      $418,600       $547,100",
          "    2019 Value       $147,900      $420,800       $568,700",
          "Percent Change        +15.1%         +0.5%          +3.9%",
          "Number of improved Parcels in the Population: 7,388")
  cl <- ar_classify(ar_pages(p1, p7))
  expect_equal(cl$kind, "condo"); expect_equal(cl$cover_year, 2019L)
  out <- ar_parse_text(ar_pages(p1, p7), "commercial/700_01.pdf", 2019L)   # no layout warning
  pop <- out$rows[out$rows$basis == "population", ]
  expect_equal(pop$nbhds, "35,40,65,70,85")
  expect_equal(c(pop$pct_change, pop$pct_land, pop$pct_imps), c(0.039, 0.151, 0.005))
  expect_equal(pop$n_parcels, 7388L)
  sal <- out$rows[out$rows$basis == "sales", ]
  expect_equal(sal$pct_change, 0.047)
})

test_that("2025 North: headline rows under Change in Total Assessed Value", {
  sec <- function(area, rows) c(sprintf("Area %d", area), "narrative", rows, "", "North District")
  lines <- c(
    "North District Geographic Areas Report",
    "Areas: 10, 14, 17, 19, 80, 85, 90 and 95",
    "Commercial Revalue for 2025 Assessment Roll",
    sec(10, c("from the 2024 assessment of +1.00% in Geographic Area 10. The adjustments in values are",
              "                         Change in Total Assessed Value",
              "         2024 Total Value 2025 Total Value       $ Change                  % Change",
              "          $3,999,274,274    $4,039,361,700      $40,087,426                 1.00%")),
    sec(17, c("                            Change in Total Assessed Value",
              "             2024 Total        2025 Total",
              "               Value             Value           $ Change             % Change",
              "          $9,177,186,099 $9,102,344,899           -$74,841,200         -0.82%")),
    sec(80, c("                           CHANGE IN TOTAL ASSESSED VALUE",
              "   2024 Total Value         2025 Total Value       $ Change                         % Change",
              "    $4,603,139,700           $4,668,359,600    $      65,219,900                     1.42%")),
    sec(85, c("                          CHANGE IN TOTAL ASSESSED VALUE",
              "  2024 Total Value         2025 Total Value       $ Change                       % Change",
              "   $6,018,144,959           $5,967,129,909   $       (51,015,050)                 -0.85%")))
  cl <- ar_classify(ar_pages(lines))
  expect_equal(cl$kind, "com_geo")
  r <- suppressWarnings(parse_com_report(lines, "north.pdf", cl$cover))
  expect_equal(r$area, c(10L, 17L, 80L, 85L))
  expect_equal(r$pct_change, c(0.01, -0.0082, 0.0142, -0.0085))
  expect_equal(r$delta, c(40087426, -74841200, 65219900, -51015050))
  expect_equal(r$av_prev[1], 3999274274)
})

test_that("condo reports in commercial/ are not a layout mismatch", {
  pages <- ar_pages(c("Specialty 700", "Residential Condominium", "2019 Assessment Roll"))
  expect_length(ar_classify(pages, "commercial")$notes, 0)
  expect_match(ar_classify(pages, "residential")$notes, "content says commercial \\(condo\\)")
})

test_that("declared known gap: one known_gap line whether the file is absent or present", {
  root <- file.path(tempdir(), "ar_gap_test"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))

  # absent year folder
  cov <- suppressWarnings(ar_import_year(2019L, root))$coverage
  expect_equal(cov$status, "known_gap")
  expect_true(is.na(cov$file))
  expect_match(cov$reason, "^no summary published")

  # file present (not a readable PDF: it must not be opened, and not be "unparsed")
  dir.create(file.path(root, "2019", "commercial"), recursive = TRUE)
  writeLines("not a pdf", file.path(root, "2019", "commercial", "100.pdf"))
  cov <- suppressMessages(ar_import_year(2019L, root))$coverage
  expect_equal(nrow(cov), 1L)
  expect_equal(cov$file, "commercial/100.pdf")
  expect_equal(cov$status, "known_gap")
  expect_equal(cov$reason, "no summary published")

  # a year with no declared gap gets no gap line
  expect_equal(nrow(suppressWarnings(ar_import_year(2020L, root))$coverage), 0L)
})

test_that("comparison allow-lists: res areas 15/21/39/45 added, specialty 15 removed", {
  row <- function(pt, rk, area = NA, spec = NA, basis = "population")
    data.frame(prop_type = pt, report_kind = rk, area = area, spec_area = spec, spec_sub = NA,
               spec_region = NA, basis = basis, pct_change = 0.01, report_id = "r", source_file = "f")
  source(here::here("ad_hoc", "area_report_compare_2026.R"), local = TRUE)
  b <- rbind(row("com", "specialty", spec = 15L), row("res", "geo", area = 1L))
  n <- rbind(row("res", "geo", area = 1L), row("res", "geo", area = 21L),
             row("res", "geo", area = 21L, basis = "sales"), row("res", "geo", area = 22L))
  d <- ar_compare_actuals(b, n)
  expect_equal(d$allowed[d$kind == "removed"], TRUE)
  expect_equal(d$allowed[d$kind == "added"], c(TRUE, TRUE, FALSE))     # 22 is not on the list
  b2 <- rbind(b, row("com", "specialty", spec = 16L))
  expect_false(ar_compare_actuals(b2, n)$allowed[ar_compare_actuals(b2, n)$spec_area %in% 16L])
})
