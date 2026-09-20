# =============================================================================
# test_kca_permits.R  —  fixture tests for the KCA permit read/aggregate layer
# =============================================================================
# Run:  Rscript scripts/ml/test_kca_permits.R
#
# Covers kcap_read_annual() only.  It writes its fixtures to tempdir() and
# never reads or writes anything under data/.
#
# The invariants here are the ones this pipeline has actually been bitten by:
#   - parcel_id must come out UNDASHED, or the panel join silently matches
#     zero rows (the SDCI defect, and the one the first kcap block repeated)
#   - PermitStatus / PcntComplete must never reach the output, or a 2015
#     parcel-year gets fed 2026 knowledge
#   - permits dated past the panel's last year must not enter any window
#   - a permit must never count toward a tax year before it was issued
#
# The panel-side window arithmetic in xx_kca_permits_to_panel.R is not covered
# here: it needs panel_tbl and a here::here() project root, and mocking those
# would make the test more fragile than the code it guards.  That script
# reports its own join rate and raises a warning on a zero or near-zero match.
# =============================================================================

suppressPackageStartupMessages({library(data.table); library(here)})
O <- Sys.getenv("ML_DIR", here::here("scripts", "ml"))
source(file.path(O, "xx_kca_permits_read.R"))

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) {
    cat("  ok   ", what, "\n")
  } else {
    cat("  FAIL ", what, "\n"); FAIL <<- FAIL + 1L
  }
}
eq <- function(actual, expected, what)
  ok(isTRUE(all.equal(actual, expected)),
     paste0(what, " (got ", paste(actual, collapse = ","),
            ", want ", paste(expected, collapse = ","), ")"))

fx <- file.path(tempdir(), "kcap_fixture")
dir.create(fx, showWarnings = FALSE, recursive = TRUE)

# Header fixture.  PermitStatus / PcntComplete / UpdatedBy / UpdateDate are
# present in the file precisely so the test can prove they are not read.
fwrite(data.table(
  Major     = c("006200", "006200", "123456", "123456", "123456", "000100", "000100"),
  Minor     = c("0010",   "0010",   "0020",   "0020",   "0020",   "0001",   "0001"),
  PermitNbr = c("P1", "P2", "P3", "P4", "P5", "P6", "P7"),
  PermitType = c("Building, New", "Remodel", "Demolition", "Building, New",
                 "Electrical", "Accessory, New", "Widget, Strange"),
  IssueDate = c("2015-06-01 00:00:00", "2016-03-02 00:00:00",
                "2015-01-01 00:00:00", "2016-11-30 00:00:00",
                "05/05/2016",          "1900-01-01 00:00:00",
                "2027-02-02 00:00:00"),
  PermitVal    = c(500000, 25000, 0, 2e9, 1200, 40000, 9000),
  PermitStatus = "Complete", PcntComplete = 100,
  UpdatedBy    = "LEAK", UpdateDate = "2026-09-04"),
  file.path(fx, "EXTR_PermitHistory_V.csv"))

fwrite(data.table(
  PermitNbr  = c("P1", "P1", "P2", "P3", "P5"),
  PermitItem = c(8L, 12L, 12L, 12L, 12L),
  ItemValue  = c("123 Main St", "New Construction", "SF REROOF",
                 "Demolish SFR", "FIRE ALARM")),
  file.path(fx, "EXTR_PermitDetailHistory_V.csv"))

cat("\n-- kcap_read_annual, lag 0, panel ending 2026 --\n")
r <- kcap_read_annual(fx, yr_max = 2026L, tax_yr_lag = 0L, verbose = FALSE)
a <- r$annual

ok(!is.null(r), "returns a result")
eq(r$diag$drop_sentinel, 1L, "pre-1901 sentinel dropped")
eq(r$diag$drop_future,   1L, "permit dated 2027 dropped before aggregation")
eq(r$diag$rows_kept,     5L, "5 of 7 header rows survive filtering")

# --- key format ---
ok(all(nchar(a$parcel_id) == 10L), "parcel_id is 10 characters")
ok(!any(grepl("-", a$parcel_id)),  "parcel_id is UNDASHED (panel join depends on it)")
ok("0062000010" %chin% a$parcel_id, "Major+Minor concatenate without reformatting")

# --- leakage ---
leaky <- c("permit_status", "pcnt_complete", "updated_by", "update_date",
           "PermitStatus", "PcntComplete", "UpdatedBy", "UpdateDate")
ok(!any(leaky %chin% names(a)),
   "no as-of-extract-date status column reaches the aggregate")

# --- type bucketing ---
setkey(a, parcel_id, tax_yr)
eq(a[.("0062000010", 2015L), kcap_n_newconst], 1L, "Building, New buckets as new construction")
eq(a[.("0062000010", 2016L), kcap_n_remodel],  1L, "Remodel buckets as remodel")
eq(a[.("1234560020", 2015L), kcap_n_demo],     1L, "Demolition buckets as demolition")
eq(a[.("1234560020", 2016L), kcap_n_all],      2L, "mm/dd/yyyy dates parse (P4 + P5 in 2016)")
ok(r$diag$types_unknown_rows == 0L,
   "the unknown PermitType was the 2027 row, already dropped")

# --- value handling ---
ok(a[.("1234560020", 2016L), kcap_val_max] < 2e9,
   "the $2B permit is winsorized below its raw value")
eq(a[.("1234560020", 2015L), kcap_val_sum], 0, "a zero-value permit stays zero, not NA")

# --- description flag from item 12 ---
eq(a[.("0062000010", 2015L), kcap_n_desc_major], 1L, "'New Construction' is structural")
eq(a[.("0062000010", 2016L), kcap_n_desc_major], 0L, "'SF REROOF' is not structural")
eq(a[.("1234560020", 2015L), kcap_n_desc_major], 1L, "'Demolish SFR' is structural")

# --- no permit lands before its issue year ---
ok(all(a$tax_yr >= 2015L), "no parcel-year predates the earliest surviving permit")
ok(all(r$newconst$tax_yr %in% c(2015L, 2016L)), "new-construction years are the issue years")

cat("\n-- tax_yr_lag = 1 shifts every row forward one year --\n")
r1 <- kcap_read_annual(fx, yr_max = 2026L, tax_yr_lag = 1L, verbose = FALSE)
eq(sort(r1$annual$tax_yr), sort(a$tax_yr + 1L), "lag 1 shifts tax_yr by exactly one")

cat("\n-- a permit past the panel boundary cannot enter a window --\n")
r2 <- kcap_read_annual(fx, yr_max = 2015L, tax_yr_lag = 0L, verbose = FALSE)
ok(all(r2$annual$tax_yr <= 2015L), "nothing after yr_max survives")
eq(r2$diag$drop_future, 4L,
   "the three 2016 rows and the 2027 row are counted as dropped, not silently lost")

cat("\n-- missing detail file degrades, does not crash --\n")
fx2 <- file.path(tempdir(), "kcap_fixture_nodetail")
dir.create(fx2, showWarnings = FALSE, recursive = TRUE)
invisible(file.copy(file.path(fx, "EXTR_PermitHistory_V.csv"), fx2, overwrite = TRUE))
r3 <- kcap_read_annual(fx2, yr_max = 2026L, verbose = FALSE)
ok(!is.null(r3), "reads with no detail file")
eq(sum(r3$annual$kcap_n_desc_major), 0L, "description flag is zero, not NA, with no detail")

cat("\n-- missing extract returns NULL rather than erroring --\n")
ok(is.null(kcap_read_annual(file.path(tempdir(), "nope"), yr_max = 2026L,
                            verbose = FALSE)),
   "absent extract returns NULL")

cat("\n-- a missing required column stops loudly --\n")
fx3 <- file.path(tempdir(), "kcap_fixture_badschema")
dir.create(fx3, showWarnings = FALSE, recursive = TRUE)
h <- fread(file.path(fx, "EXTR_PermitHistory_V.csv"))
h[, PermitVal := NULL]
fwrite(h, file.path(fx3, "EXTR_PermitHistory_V.csv"))
e <- try(kcap_read_annual(fx3, yr_max = 2026L, verbose = FALSE), silent = TRUE)
ok(inherits(e, "try-error") && grepl("PermitVal", as.character(e)),
   "a dropped column errors by name instead of producing empty features")

cat("\n")
if (FAIL > 0L) {
  cat(FAIL, "FAILURE(S)\n"); quit(status = 1L)
}
cat("all kca permit tests passed\n")
