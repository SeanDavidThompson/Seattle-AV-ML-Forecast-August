# 2026 output must match the pre-change baseline: totals identical in every
# shared row; land/imps may be filled where the baseline had them blank; any
# other difference only in spec 153/174, 500 and 510 rows.
# Baseline: ad_hoc/area_report_baseline_2026.R.

source(here::here("ad_hoc", "area_report_compare_2026.R"), local = TRUE)

cmp_row <- function(spec, pct = 0.1, land = NA_real_, name = "x")
  data.frame(prop_type = "com", report_kind = "specialty", area = NA, spec_area = spec,
             spec_sub = NA, spec_region = NA, basis = "population", pct_change = pct,
             pct_land = land, area_name = name, report_id = "r", source_file = "f")

test_that("ar_compare_actuals classifies differences", {
  b <- rbind(cmp_row(280L), cmp_row(510L))
  n <- rbind(cmp_row(280L, land = 0.05), cmp_row(510L, name = "y"), cmp_row(153L))
  n$report_id <- "commercial/r"; n$tax_yr <- 2027L                # skipped columns
  d <- ar_compare_actuals(b, n)
  expect_setequal(paste(d$kind, d$spec_area), c("filled 280", "changed 510", "added 153"))
  expect_true(all(d$allowed))

  # a total that differs fails, even in an allowed spec
  n2 <- n; n2$pct_change[n2$spec_area == 510L] <- 0.2
  d2 <- ar_compare_actuals(b, n2)
  expect_false(d2$allowed[d2$kind == "total_mismatch"])

  # non-blank to another non-blank value outside the allowed specs fails
  n3 <- n; n3$area_name[n3$spec_area == 280L] <- "z"
  d3 <- ar_compare_actuals(b, n3)
  expect_false(d3$allowed[d3$kind == "changed" & d3$spec_area == 280L])

  # a land value that was already there and changed is "changed", not "filled"
  b4 <- b; b4$pct_land[1] <- 0.01
  d4 <- ar_compare_actuals(b4, n)
  expect_false(d4$allowed[d4$kind == "changed" & d4$spec_area == 280L])

  cn <- ar_compare_counts(d)
  expect_equal(sum(cn$allowed), 3L); expect_equal(sum(cn$failing), 0L)
})

test_that("2026 matches the main baseline", {
  base_path <- here::here("data", "cache", "area_report_baseline_main_2026.rds")
  skip_if_not(file.exists(base_path), "no baseline: run ad_hoc/area_report_baseline_2026.R")
  skip_if_no_reports(2026)
  d <- ar_compare_actuals(readRDS(base_path), ar_year(2026)$actuals)
  if (nrow(d)) print(d, row.names = FALSE)
  print(ar_compare_counts(d), row.names = FALSE)
  expect_equal(sum(!d$allowed), 0L)
  expect_equal(sum(d$kind == "total_mismatch"), 0L)
  touched <- unique(d$spec_area[d$kind %in% c("added", "changed", "other_filled")])
  expect_true(all(c(153L, 174L, 500L, 510L) %in% touched))
})
