# xx_health_inspections_import.R ---------------------------------------------
# Download + cache Public Health Seattle & King County food establishment
# inspection data (Socrata dataset f29f-zza5 on data.kingcounty.gov).
#
# Notes on the source:
#   * Each row is one VIOLATION; an inspection with multiple violations spans
#     multiple rows sharing inspection_serial_num.  Collapsing to inspection /
#     establishment level happens downstream in xx_health_to_panel.R.
#   * Environmental Health migrated permitting systems in late 2025; the
#     refreshed dataset may only span 2021+ (the legacy series covered
#     2006-2025).  The year range is reported below so truncation is visible.
#   * Hotels are represented via their food-service operations (hotel
#     restaurants/bars/kitchens) — lodging itself is not inspected here.
#
# Freshness logic:
#   1. cache_dir/health_inspections_raw.rds exists and is younger than
#      `health_max_age_days` (default 30) -> reuse.
#   2. Otherwise try downloading the full CSV export to data/health/.
#   3. On download failure, fall back to the newest *.csv already present in
#      data/health/ (manual-drop escape hatch).
#   4. If nothing is available: warn and create no object —
#      xx_health_to_panel.R will stub NA columns (CoStar-style graceful skip).
#
# Output: `health_insp_raw` (data.table, violation-level) in .GlobalEnv,
#         cached to <cache_dir>/health_inspections_raw.rds
# -----------------------------------------------------------------------------

cache_dir <- get("cache_dir", envir = .GlobalEnv)

health_dir <- here::here("data", "health")
dir.create(health_dir, showWarnings = FALSE, recursive = TRUE)

health_max_age_days <- get0("health_max_age_days", envir = .GlobalEnv,
                            ifnotfound = 30L)
health_url <- paste0("https://data.kingcounty.gov/api/views/",
                     "f29f-zza5/rows.csv?accessType=DOWNLOAD")

health_cache <- file.path(cache_dir, "health_inspections_raw.rds")

message("\n--- xx_health_inspections_import.R ---")

# ---- Read helper (KCA-style encoding hardening) -----------------------------
.read_health_csv <- function(path) {
  dt <- data.table::fread(file = path, na.strings = c("", "NA"),
                          encoding = "Latin-1", showProgress = FALSE)
  data.table::setDT(dt)
  chr_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (cc in chr_cols)
    data.table::set(dt, j = cc,
                    value = iconv(dt[[cc]], from = "UTF-8", to = "UTF-8",
                                  sub = ""))
  janitor::clean_names(dt)
}

.cache_age_days <- function(p) {
  as.numeric(difftime(Sys.time(), file.info(p)$mtime, units = "days"))
}

# ---- 1. Reuse fresh cache ---------------------------------------------------
health_insp_raw <- NULL

if (file.exists(health_cache) &&
    .cache_age_days(health_cache) < health_max_age_days) {
  health_insp_raw <- readRDS(health_cache)
  message("  \u2705 cache is ", round(.cache_age_days(health_cache), 1),
          " days old (< ", health_max_age_days, ") — reusing ",
          basename(health_cache))
} else {

  # ---- 2. Attempt download --------------------------------------------------
  dl_path <- file.path(health_dir,
                       paste0("food_inspections_", Sys.Date(), ".csv"))
  dl_ok <- FALSE
  old_timeout <- getOption("timeout")
  options(timeout = max(1200, old_timeout))
  tryCatch({
    message("  Downloading food inspection data from data.kingcounty.gov ...")
    utils::download.file(health_url, destfile = dl_path,
                         mode = "wb", quiet = TRUE)
    if (file.exists(dl_path) && file.info(dl_path)$size > 1e6) {
      dl_ok <- TRUE
      message("  \u2705 downloaded: ", basename(dl_path), " (",
              round(file.info(dl_path)$size / 1024^2, 1), " MB)")
    } else {
      warning("Downloaded file suspiciously small — treating as failed.",
              call. = FALSE)
    }
  }, error = function(e) {
    warning("Download failed: ", conditionMessage(e), call. = FALSE)
  })
  options(timeout = old_timeout)

  # ---- 3. Pick a source file ------------------------------------------------
  src_file <- NULL
  if (dl_ok) {
    src_file <- dl_path
  } else {
    local_csvs <- list.files(health_dir, pattern = "\\.csv$",
                             full.names = TRUE, ignore.case = TRUE)
    # Ignore tiny files — aborted downloads (e.g. VPN drops) leave partial
    # CSVs behind that would otherwise be silently read as "the data"
    local_csvs <- local_csvs[file.info(local_csvs)$size > 1e6]
    if (length(local_csvs) > 0) {
      src_file <- local_csvs[which.max(file.info(local_csvs)$mtime)]
      message("  \u26a0\ufe0f  using newest local file instead: ",
              basename(src_file))
    }
  }

  if (is.null(src_file)) {
    warning("No health inspection data available (download failed and no ",
            "local CSV in ", health_dir, ").\n",
            "hlth_* features will be stubbed as NA.", call. = FALSE)
  } else {
    health_insp_raw <- .read_health_csv(src_file)
  }
}

# ---- 4. Normalize + validate ------------------------------------------------
if (!is.null(health_insp_raw)) {

  # Core columns without which parcel matching is impossible
  core_cols <- c("address", "zip_code", "inspection_date")
  missing_core <- setdiff(core_cols, names(health_insp_raw))
  if (length(missing_core) > 0) {
    warning("Health data missing core column(s): ",
            paste(missing_core, collapse = ", "),
            " — schema may have changed after the 2025 permitting-system ",
            "transition. hlth_* features will be stubbed as NA.",
            call. = FALSE)
    health_insp_raw <- NULL
  }
}

if (!is.null(health_insp_raw)) {

  # Parse inspection date (export format is typically m/d/Y; accept Y-m-d too)
  health_insp_raw[, inspection_date :=
    as.Date(lubridate::parse_date_time(inspection_date,
                                       orders = c("mdY", "Ymd"),
                                       quiet = TRUE))]
  health_insp_raw <- health_insp_raw[!is.na(inspection_date)]
  health_insp_raw[, insp_yr := as.integer(format(inspection_date, "%Y"))]

  # Numeric coercions for optional fields (schema-defensive)
  for (nc in intersect(c("inspection_score", "violation_points", "grade"),
                       names(health_insp_raw)))
    health_insp_raw[, (nc) := suppressWarnings(as.numeric(get(nc)))]

  # Establishment key: business_id preferred, then program_identity,
  # then name+address composite
  if ("business_id" %in% names(health_insp_raw)) {
    health_insp_raw[, estab_id := as.character(business_id)]
  } else if ("program_identity" %in% names(health_insp_raw)) {
    health_insp_raw[, estab_id := as.character(program_identity)]
  } else {
    health_insp_raw[, estab_id := paste(name, address, sep = "|")]
  }

  # Inspection key fallback (needed to collapse violation rows)
  if (!"inspection_serial_num" %in% names(health_insp_raw))
    health_insp_raw[, inspection_serial_num :=
      paste(estab_id, inspection_date, sep = "|")]

  assign("health_insp_raw", health_insp_raw, envir = .GlobalEnv)
  saveRDS(health_insp_raw, health_cache)

  message("  health_insp_raw: ",
          scales::comma(nrow(health_insp_raw)), " violation rows | ",
          scales::comma(data.table::uniqueN(health_insp_raw$estab_id)),
          " establishments | years ",
          min(health_insp_raw$insp_yr), "-", max(health_insp_raw$insp_yr))
  if (min(health_insp_raw$insp_yr) > 2010)
    message("  \u2139\ufe0f  note: series starts ", min(health_insp_raw$insp_yr),
            " — post-transition dataset is truncated vs the legacy 2006+ series")
  message("  \U1f4be cached: ", basename(health_cache))
}

# If nothing was imported, remove the NULL placeholder binding so downstream
# exists() checks (xx_health_to_panel.R) correctly take the stub path instead
# of trying to match against an empty object.
if (is.null(health_insp_raw) &&
    exists("health_insp_raw", envir = .GlobalEnv))
  rm("health_insp_raw", envir = .GlobalEnv)

message("xx_health_inspections_import.R complete.")
