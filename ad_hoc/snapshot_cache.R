# =============================================================================
# snapshot_cache.R — copy the cache aside before rebuilding. Pure R, no shell.
# -----------------------------------------------------------------------------
# Run this BEFORE re-running the pipeline. Rebuilding overwrites
# data/cache/*.rds in place and the "before" side cannot be reconstructed.
#
# Do NOT use file.copy(src_dir, dst_dir, recursive = TRUE): if dst does not
# exist it returns TRUE, ignores `recursive`, and creates an empty FILE named
# dst — nothing is copied and it looks like it worked. If dst does exist it
# nests the copy at dst/cache/..., which is not where the diff script looks.
# =============================================================================

suppressPackageStartupMessages(library(here))

SRC  <- here::here("data", "cache")
DST  <- here::here("data", "cache_pre_avdedup")

# The diff needs one file. Set to NULL to snapshot the whole cache instead
# (panels are large, so only do that if you want the full rollback).
ONLY <- "panel_tbl_2006_2031_forecasted_baseline_com.rds"

stopifnot(dir.exists(SRC))
if (dir.exists(DST) && length(list.files(DST)))
  stop("Snapshot already exists and is not empty: ", DST,
       "\n  Refusing to overwrite it — a stale snapshot is still a good one. ",
       "Move or rename it if you really want a fresh copy.")
if (file.exists(DST) && !dir.exists(DST))
  stop(DST, " exists as a FILE, not a directory — almost certainly the empty ",
       "file left behind by file.copy(..., recursive = TRUE). Delete it first.")

dir.create(DST, showWarnings = FALSE, recursive = TRUE)

src_files <- if (is.null(ONLY)) {
  list.files(SRC, recursive = TRUE, full.names = TRUE, all.files = TRUE,
             no.. = TRUE)
} else {
  f <- file.path(SRC, ONLY)
  if (!file.exists(f)) stop("Not found: ", f)
  f
}
src_files <- src_files[!dir.exists(src_files)]        # files only
if (!length(src_files)) stop("Nothing to copy from ", SRC)

rel <- substring(src_files, nchar(SRC) + 2L)
for (d in unique(dirname(file.path(DST, rel))))       # mirror any subdirs
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

ok <- file.copy(src_files, file.path(DST, rel),
                overwrite = FALSE, copy.date = TRUE)

# Verify by size: file.copy's return value alone is not proof.
src_sz <- file.size(src_files)
dst_sz <- file.size(file.path(DST, rel))
good   <- ok & !is.na(dst_sz) & dst_sz == src_sz

for (i in seq_along(rel))
  cat(sprintf("  %-55s %8.1f MB  %s\n", rel[i], src_sz[i] / 1024^2,
              if (good[i]) "OK" else "FAILED"))
cat(sprintf("\n%d of %d file(s) copied to %s (%.1f MB)\n",
            sum(good), length(good), DST, sum(dst_sz[good]) / 1024^2))

if (!all(good))
  stop("Snapshot incomplete — do NOT rebuild yet. Failed: ",
       paste(rel[!good], collapse = ", "))
cat("Snapshot verified. Safe to rebuild.\n")
