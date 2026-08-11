# scripts/ml/xx_nwmls_to_panel.R -------------------------------------------
# Join NWMLS SFH market indicators to panel_tbl (residential backbone).
# Reads from the EViews Excel export (preferred) or legacy CSV (fallback).
# Reads `scenario` from .GlobalEnv (set by main_ml.R).
#
# Excel layout (from EViews tbl_export.save):
#   Row 1: col A blank, col B onward = series names
#   Row 2: blank
#   Row 3+: col A = date string "2005M01", col B onward = values
#
# Gracefully handles missing KC SFH columns.
# -------------------------------------------------------------------------

scenario <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
message("xx_nwmls_to_panel.R: scenario = ", scenario)

# ---- Locate the NWMLS data file -----------------------------------------
nwmls_dir <- here("data", "nwmls")

# Try Excel first (full EViews export with all series)
nwmls_xlsx <- list.files(nwmls_dir,
                         pattern = paste0("nwmls_housing_forecast_", scenario, "\\.xlsx$"),
                         full.names = TRUE)

# Fallback: legacy CSV
nwmls_csv <- list.files(nwmls_dir,
                        pattern = paste0("combined_nwmls_m_", scenario, ".*\\.csv$"),
                        full.names = TRUE)

if (length(nwmls_xlsx) > 0) {
  nwmls_file <- nwmls_xlsx[which.max(file.info(nwmls_xlsx)$mtime)]
  message("  Reading NWMLS from Excel: ", basename(nwmls_file))

  # Row 1 = headers (col A blank, B+ = series names), Row 2 = blank, Row 3+ = data
  raw <- read_xlsx(nwmls_file, skip = 0, col_names = TRUE)
  names(raw)[1] <- "date_str"
  names(raw) <- tolower(names(raw))

  # Coerce all non-date columns to numeric (some may read as character)
  num_cols <- setdiff(names(raw), "date_str")
  raw[num_cols] <- lapply(raw[num_cols], function(x) as.numeric(as.character(x)))

  nwmls_raw <- raw %>%
    filter(!is.na(date_str), date_str != "") %>%
    mutate(date = as.Date(paste0(
      substr(date_str, 1, 4), "-",
      substr(date_str, 6, 7), "-01"
    ))) %>%
    select(-date_str)

} else if (length(nwmls_csv) > 0) {
  nwmls_file <- nwmls_csv[which.max(file.info(nwmls_csv)$mtime)]
  message("  Reading NWMLS from CSV (legacy): ", basename(nwmls_file))
  nwmls_raw <- read_csv(nwmls_file, show_col_types = FALSE) %>%
    janitor::clean_names()
} else {
  stop("No NWMLS file found for scenario '", scenario, "' in ", nwmls_dir)
}


# ---- Detect Seattle SFH columns -----------------------------------------
# Try _fav (forecast average) first, then individual spec names, then raw
sea_sfh_candidates <- list(
  c(sea_pmedesfh = "sea_pmedesfh_fav", sea_sesfh = "sea_sesfh_fav", sea_alesfh = "sea_alesfh_fav"),
  c(sea_pmedesfh = "sea_pmedesfh_f",   sea_sesfh = "sea_sesfhf",    sea_alesfh = "sea_alesfhf"),
  c(sea_pmedesfh = "sea_pmedesfh",     sea_sesfh = "sea_sesfh",     sea_alesfh = "sea_alesfh")
)

sea_sfh_found <- character(0)
for (cand in sea_sfh_candidates) {
  if (all(cand %in% names(nwmls_raw))) {
    sea_sfh_found <- cand
    break
  }
}

if (length(sea_sfh_found) != 3) {
  stop("Seattle SFH columns not found in NWMLS file.\n",
       "  Available columns: ", paste(names(nwmls_raw), collapse = ", "))
}
message("  Seattle SFH columns: ", paste(sea_sfh_found, collapse = ", "))

# ---- Detect King County SFH columns (optional) --------------------------
kc_sfh_candidates <- list(
  c(k_pmedesfh = "k_pmedesfh_fav", k_sesfh = "k_sesfh_fav", k_alesfh = "k_alesfh_fav"),
  c(k_pmedesfh = "k_pmedesfh_f90", k_sesfh = "k_sesfh_f50", k_alesfh = "k_alesfh_f50"),
  c(k_pmedesfh = "k_pmedesfh",     k_sesfh = "k_sesfh",     k_alesfh = "k_alesfh")
)

kc_sfh_found <- character(0)
for (cand in kc_sfh_candidates) {
  if (all(cand %in% names(nwmls_raw))) {
    kc_sfh_found <- cand
    break
  }
}
has_kc_sfh <- length(kc_sfh_found) == 3

if (has_kc_sfh) {
  message("  King County SFH columns: ", paste(kc_sfh_found, collapse = ", "))
} else {
  message("  King County SFH columns not found in CSV — skipping KC variables")
}

# ---- Select and rename --------------------------------------------------
rename_vec <- sea_sfh_found
if (has_kc_sfh) rename_vec <- c(rename_vec, kc_sfh_found)

cols_to_keep <- c("date", unname(rename_vec))
nwmls <- nwmls_raw %>%
  select(all_of(cols_to_keep)) %>%
  rename(all_of(rename_vec)) %>%
  mutate(
    ym    = zoo::as.yearmon(date),
    year  = lubridate::year(date),
    month = lubridate::month(date)
  )

# ---- Rolling lag windows: Seattle SFH -----------------------------------
nwmls_lagged <- nwmls %>%
  select(ym, year, month,
         sea_pmedesfh, sea_sesfh, sea_alesfh,
         any_of(c("k_pmedesfh", "k_sesfh", "k_alesfh"))) %>%
  arrange(ym) %>%
  mutate(
    sea_pmedesfh_lag6  = slide_dbl(sea_pmedesfh, mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
    sea_pmedesfh_lag12 = slide_dbl(sea_pmedesfh, mean, .before = 11, .complete = TRUE, na.rm = TRUE),
    sea_sesfh_lag6     = slide_dbl(sea_sesfh,    mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
    sea_sesfh_lag12    = slide_dbl(sea_sesfh,    mean, .before = 11, .complete = TRUE, na.rm = TRUE),
    sea_alesfh_lag6    = slide_dbl(sea_alesfh,   mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
    sea_alesfh_lag12   = slide_dbl(sea_alesfh,   mean, .before = 11, .complete = TRUE, na.rm = TRUE)
  )

# ---- Rolling lag windows: King County SFH (conditional) -----------------
if (has_kc_sfh) {
  nwmls_lagged <- nwmls_lagged %>%
    mutate(
      k_pmedesfh_lag6  = slide_dbl(k_pmedesfh, mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      k_pmedesfh_lag12 = slide_dbl(k_pmedesfh, mean, .before = 11, .complete = TRUE, na.rm = TRUE),
      k_sesfh_lag6     = slide_dbl(k_sesfh,    mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      k_sesfh_lag12    = slide_dbl(k_sesfh,    mean, .before = 11, .complete = TRUE, na.rm = TRUE),
      k_alesfh_lag6    = slide_dbl(k_alesfh,   mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      k_alesfh_lag12   = slide_dbl(k_alesfh,   mean, .before = 11, .complete = TRUE, na.rm = TRUE)
    )
}

# ---- Annual: December snapshot → next tax_yr ----------------------------
nwmls_av_year <- nwmls_lagged %>%
  filter(month == 12) %>%
  arrange(year) %>%
  transmute(
    tax_yr = year + 1,
    # Seattle SFH
    sea_pmedesfh_lag12,
    sea_pmedesfh_lag6,
    sea_sesfh_lag6,
    sea_sesfh_lag12,
    sea_alesfh_lag6,
    sea_alesfh_lag12
  )

# Add King County SFH lags if available
if (has_kc_sfh) {
  kc_annual <- nwmls_lagged %>%
    filter(month == 12) %>%
    arrange(year) %>%
    transmute(
      tax_yr = year + 1,
      k_pmedesfh_lag12,
      k_pmedesfh_lag6,
      k_sesfh_lag6,
      k_sesfh_lag12,
      k_alesfh_lag6,
      k_alesfh_lag12
    )
  nwmls_av_year <- nwmls_av_year %>%
    left_join(kc_annual, by = "tax_yr")
}

# ---- Cache forecast years -----------------------------------------------
cache_dir <- get0("cache_dir", envir = .GlobalEnv,
                  ifnotfound = here("data", "cache"))
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

nwmls_fcst_out <- nwmls_av_year %>%
  filter(tax_yr >= get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2027L) - 1L,
         tax_yr <= get0("forecast_end",   envir = .GlobalEnv, ifnotfound = 2032L)) %>%
  arrange(tax_yr) %>%
  distinct(tax_yr, .keep_all = TRUE)

nwmls_fcst_path <- file.path(cache_dir,
                              paste0("nwmls_fcst_2026_2031_", scenario, ".rds"))
saveRDS(nwmls_fcst_out, nwmls_fcst_path)
message("\U0001f4be NWMLS forecast cached to: ", basename(nwmls_fcst_path))

# ---- Join to panel_tbl --------------------------------------------------
panel_tbl <- panel_tbl %>%
  left_join(nwmls_av_year, by = "tax_yr")

message("xx_nwmls_to_panel.R loaded (scenario = ", scenario,
        " | KC SFH = ", has_kc_sfh, ")")
