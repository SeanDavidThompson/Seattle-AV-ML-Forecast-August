# =============================================================================
# com_ty2026_av_diff.R   — READ ONLY, writes nothing
# -----------------------------------------------------------------------------
# Diffs TY2026 commercial AV between the panel cached BEFORE the av-dedup
# changes and the one built AFTER, and decomposes the delta into:
#
#   dropped  parcels in the old panel only  (out-of-city, step 3)
#   added    parcels in the new panel only  (Seattle PropType C newcomers, step 2)
#   changed  parcels in both whose TY2026 AV moved
#   same     parcels in both whose TY2026 AV is identical
#
# >>> SNAPSHOT FIRST <<<
# Rebuilding overwrites data/cache/*.rds in place. Before re-running the
# pipeline, copy the current cache aside, e.g.
#
#     cp -a data/cache data/cache_pre_avdedup
#
# then point BEFORE_PATH at the snapshot and AFTER_PATH at the rebuilt file.
# Without that snapshot this comparison cannot be made after the fact.
# =============================================================================

suppressPackageStartupMessages({library(data.table); library(here)})

BEFORE_PATH <- here::here("data", "cache_pre_avdedup",
                          "panel_tbl_2006_2031_forecasted_baseline_com.rds")
AFTER_PATH  <- here::here("data", "cache",
                          "panel_tbl_2006_2031_forecasted_baseline_com.rds")
TAX_YR      <- 2026L

for (p in c(BEFORE_PATH, AFTER_PATH))
  if (!file.exists(p)) stop("Not found: ", p,
    "\n  Did you snapshot data/cache before rebuilding? See the header.")

nodash <- function(x) gsub("-", "", as.character(x), fixed = TRUE)
pick <- function(dt, nm) {
  if (nm %in% names(dt)) as.numeric(dt[[nm]]) else rep(NA_real_, nrow(dt))
}
addna <- function(a, b) {
  fifelse(is.na(a) & is.na(b), NA_real_,
          fifelse(is.na(a), 0, a) + fifelse(is.na(b), 0, b))
}

load_ty <- function(path, label) {
  d <- as.data.table(readRDS(path))
  if (!"tax_yr" %in% names(d)) stop("no tax_yr column in ", path)
  d <- d[tax_yr == TAX_YR]
  d[, pid := nodash(parcel_id)]
  d[, av := fcoalesce(
    pick(d, "total_assessed"),
    addna(pick(d, "appr_land_val"), pick(d, "appr_imps_val")),
    pick(d, "pred_total_assessed"),
    addna(pick(d, "pred_appr_land_val"), pick(d, "pred_appr_imps_val")))]
  keep <- intersect(c("pid","av","has_comm_bldg","levy_code","spec_area",
                      "prop_type_extr","com_subgroup"), names(d))
  d <- unique(d[, ..keep], by = "pid")
  message(sprintf("%-6s %s: %s parcels | TY%d AV $%.3fB | AV missing %s",
                  label, basename(path), format(nrow(d), big.mark = ","),
                  TAX_YR, sum(d$av, na.rm = TRUE) / 1e9,
                  format(sum(is.na(d$av)), big.mark = ",")))
  d
}

B <- function(x) sum(x, na.rm = TRUE) / 1e9
cat("=== inputs ===\n")
old <- load_ty(BEFORE_PATH, "BEFORE")
new <- load_ty(AFTER_PATH,  "AFTER")

m <- merge(old[, .(pid, av_old = av)], new[, .(pid, av_new = av)],
           by = "pid", all = TRUE)
m[, bucket := fcase(
  is.na(av_new) & !pid %chin% new$pid, "dropped",
  !pid %chin% old$pid,                 "added",
  default = NA_character_)]
m[pid %chin% old$pid & pid %chin% new$pid,
  bucket := fifelse(
    fifelse(is.na(av_old), -1, av_old) == fifelse(is.na(av_new), -1, av_new),
    "same", "changed")]

cat("\n=== decomposition ===\n")
summ <- m[, .(parcels = .N,
              av_before_B = round(B(av_old), 3),
              av_after_B  = round(B(av_new),  3),
              delta_B     = round(B(av_new) - B(av_old), 3)),
          by = bucket][order(factor(bucket,
                                    levels = c("dropped","added","changed","same")))]
print(summ)

tot_old <- B(old$av); tot_new <- B(new$av)
cat(sprintf("\nTY%d AV: before $%.3fB -> after $%.3fB | net %+.3fB (%+.2f%%)\n",
            TAX_YR, tot_old, tot_new, tot_new - tot_old,
            100 * (tot_new / tot_old - 1)))
cat(sprintf("parcels: before %s -> after %s | net %+d\n",
            format(nrow(old), big.mark = ","), format(nrow(new), big.mark = ","),
            nrow(new) - nrow(old)))

# The buckets must add back to the net change, or the decomposition is wrong.
chk <- sum(summ$delta_B)
cat(sprintf("\nreconciliation: buckets sum to %+.3fB vs net %+.3fB -> %s\n",
            chk, tot_new - tot_old,
            if (abs(chk - (tot_new - tot_old)) < 1e-6) "OK" else "MISMATCH"))

cat("\n=== dropped: expected to be the out-of-city parcels (step 3) ===\n")
drop <- m[bucket == "dropped"]
cat(sprintf("%s parcels | $%.3fB | of which zero/NA AV: %s\n",
            format(nrow(drop), big.mark = ","), B(drop$av_old),
            format(sum(is.na(drop$av_old) | drop$av_old == 0), big.mark = ",")))

cat("\n=== added: expected to be Seattle PropType C newcomers (step 2) ===\n")
add <- m[bucket == "added"]
cat(sprintf("%s parcels | $%.3fB\n", format(nrow(add), big.mark = ","), B(add$av_new)))
for (col in c("has_comm_bldg", "spec_area", "com_subgroup")) {
  if (col %in% names(new)) {
    a <- merge(add[, .(pid)], new, by = "pid")
    t <- a[, .(parcels = .N, av_B = round(B(av), 3)), by = col][order(-parcels)]
    cat("\n by ", col, ":\n", sep = ""); print(head(t, 15))
  }
}

cat("\n=== changed: parcels in both whose TY2026 AV moved ===\n")
chg <- m[bucket == "changed"][order(-abs(fifelse(is.na(av_new), 0, av_new) -
                                         fifelse(is.na(av_old), 0, av_old)))]
cat(sprintf("%s parcels | before $%.3fB -> after $%.3fB (%+.3fB)\n",
            format(nrow(chg), big.mark = ","), B(chg$av_old), B(chg$av_new),
            B(chg$av_new) - B(chg$av_old)))
cat(sprintf("  gained AV (was NA/0): %s | lost AV (now NA/0): %s\n",
            format(chg[(is.na(av_old) | av_old == 0) & av_new > 0, .N], big.mark = ","),
            format(chg[av_old > 0 & (is.na(av_new) | av_new == 0), .N], big.mark = ",")))
if (nrow(chg)) {
  cat("\n largest 10 moves:\n")
  print(head(chg[, .(pid, av_old_B = round(av_old/1e9, 4),
                     av_new_B = round(av_new/1e9, 4))], 10))
}

cat("\nRead-only check complete — nothing written.\n")
