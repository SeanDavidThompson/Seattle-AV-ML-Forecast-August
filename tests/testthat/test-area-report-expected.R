# Expected values from the real KCA reports (AR_EXPECTED).  Needs the PDFs in
# data/kca/area_reports/<year>/{residential,commercial}/ (never committed);
# each year is skipped when its folder is absent.

for (.y in sort(unique(c(AR_EXPECTED$year, AR_EXPECTED_280_SUBMARKETS$year)))) local({
  y <- .y
  test_that(paste("expected area-report values,", y), {
    skip_if_no_reports(y)
    act <- ar_year(y)$actuals
    expect_false(is.null(act))
    ex <- AR_EXPECTED[AR_EXPECTED$year == y, ]
    for (i in seq_len(nrow(ex))) {
      e   <- ex[i, ]
      lab <- sprintf("%d %s %s", y, e$kind, e$key)
      tol <- 0.5 * 10^-e$dp + 1e-9          # in percentage points
      r   <- ar_expected_row(act, e)
      expect_equal(nrow(r), 1L, info = lab)
      if (nrow(r) != 1L) next
      expect_lte(abs(100 * r$pct_change - e$total), tol, label = paste(lab, "total"))
      if (!is.na(e$land)) {
        expect_lte(abs(100 * r$pct_land - e$land), tol, label = paste(lab, "land"))
        expect_lte(abs(100 * r$pct_imps - e$imps), tol, label = paste(lab, "imps"))
      }
    }
    n280 <- AR_EXPECTED_280_SUBMARKETS$n[AR_EXPECTED_280_SUBMARKETS$year == y]
    if (length(n280))
      expect_equal(sum(act$report_kind == "specialty_submarket" & act$spec_area %in% 280L),
                   n280, info = paste(y, "spec 280 submarkets"))
    expect_true(all(act$assessment_yr == y & act$tax_yr == y + 1L))
    expect_true(all(is.na(act$report_year) | act$report_year == y))
  })
})

test_that("apartments 2019 is a known gap, not imputed", {
  skip_if_no_reports(2019)
  imp <- ar_year(2019)
  expect_false(any(imp$actuals$spec_area %in% 100L))
  expect_true(any(imp$coverage$status == "known_gap"))
})

test_that("coverage: every PDF found has a coverage record", {
  for (y in 2019:2026) {
    if (!dir.exists(file.path(AR_ROOT, y)) || !requireNamespace("pdftools", quietly = TRUE)) next
    n <- length(list.files(file.path(AR_ROOT, y), pattern = "\\.pdf$", recursive = TRUE,
                           ignore.case = TRUE))
    expect_equal(nrow(ar_year(y)$coverage), n, info = y)
  }
  succeed()
})
