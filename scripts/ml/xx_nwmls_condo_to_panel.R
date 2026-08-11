# scripts/ml/xx_nwmls_condo_to_panel.R -----------------------------------
# Join NWMLS condo market indicators to panel_tbl_condo.
# Reads from the EViews Excel export.
# Reads `scenario` from .GlobalEnv (set by main_ml.R).
#
# Excel layout (from EViews tbl_export.save):
#   Row 1: col A blank, col B onward = series names (K_ALESFH, SEA_PMEDEC, etc.)
#   Row 2: blank
#   Row 3+: col A = date string "2005M01", col B onward = values
# -------------------------------------------------------------------------

scenario <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
message("xx_nwmls_condo_to_panel.R: scenario = ", scenario)

# ---- Locate the EViews NWMLS Excel export --------------------------------
nwmls_dir <- here("data", "nwmls")

nwmls_xlsx <- list.files(nwmls_dir,
                         pattern = paste0("nwmls_housing_forecast_", scenario, "\\.xlsx$"),
                         full.names = TRUE)

nwmls_csv <- list.files(nwmls_dir,
                        pattern = paste0("combined_nwmls_m_", scenario, ".*\\.csv$"),
                        full.names = TRUE)

if (length(nwmls_xlsx) > 0) {
  nwmls_file <- nwmls_xlsx[which.max(file.info(nwmls_xlsx)$mtime)]
  message("  Reading NWMLS from Excel: ", basename(nwmls_file))

  # Row 1 = headers (col A blank, B+ = series names), Row 2 = blank, Row 3+ = data
  # skip = 0: read row 1 as header. Row 2 (blank) becomes first data row → filtered out.
  raw <- read_xlsx(nwmls_file, skip = 0, col_names = TRUE)

  # First column (originally blank in header) gets auto-named; rename to date_str
  names(raw)[1] <- "date_str"

  # Lowercase all column names to match expected patterns
  names(raw) <- tolower(names(raw))

  # Coerce all non-date columns to numeric (some may read as character from Excel)
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

message("  Available columns: ", paste(names(nwmls_raw), collapse = ", "))

# ---- Detect which condo columns are present ------------------------------

# Seattle condo — try _fav (forecast average) first, then individual specs
sea_condo_candidates <- list(
  c(sea_pmedec = "sea_pmedec_fav", sea_sec = "sea_sec_fav", sea_alec = "sea_alec_fav"),
  c(sea_pmedec = "sea_pmedec_f90", sea_sec = "sea_sec_f50", sea_alec = "sea_alec_f50"),
  c(sea_pmedec = "sea_pmedec",     sea_sec = "sea_sec",     sea_alec = "sea_alec")
)

sea_condo_found <- character(0)
for (cand in sea_condo_candidates) {
  if (all(cand %in% names(nwmls_raw))) {
    sea_condo_found <- cand
    break
  }
}

has_sea_condo <- length(sea_condo_found) == 3

if (!has_sea_condo) {
  message("  Seattle condo columns not found in NWMLS file — no condo NWMLS variables will be added")
  message("  Searched for: sea_pmedec_fav/sea_sec_fav/sea_alec_fav (and variants)")
  message("xx_nwmls_condo_to_panel.R loaded (scenario = ", scenario,
          " | SEA condo = FALSE | KC condo = FALSE)")

} else {
  message("  Seattle condo columns found: ", paste(sea_condo_found, collapse = ", "))

  # King County condo (optional)
  kc_condo_candidates <- list(
    c(k_pmedec = "k_pmedec_fav", k_sec = "k_sec_fav", k_alec = "k_alec_fav"),
    c(k_pmedec = "k_pmedec_f90", k_sec = "k_sec_f50", k_alec = "k_alec_f50"),
    c(k_pmedec = "k_pmedec",     k_sec = "k_sec",     k_alec = "k_alec")
  )

  kc_condo_found <- character(0)
  for (cand in kc_condo_candidates) {
    if (all(cand %in% names(nwmls_raw))) {
      kc_condo_found <- cand
      break
    }
  }
  has_kc_condo <- length(kc_condo_found) == 3

  rename_vec <- sea_condo_found
  if (has_kc_condo) {
    rename_vec <- c(rename_vec, kc_condo_found)
    message("  King County condo columns detected: ", paste(kc_condo_found, collapse = ", "))
  } else {
    message("  King County condo columns not found — skipping KC condo variables")
  }

  # ---- Drop pre-existing NWMLS condo columns from panel -----------------
  condo_drop_cols <- grep("^nwmls_condo_|^nwmls_kc_condo_",
                          names(panel_tbl_condo), value = TRUE)
  if (length(condo_drop_cols) > 0) {
    panel_tbl_condo <- panel_tbl_condo %>%
      select(-all_of(condo_drop_cols))
  }

  # ---- Build monthly NWMLS condo series ---------------------------------
  # Select only the columns we need BEFORE renaming to avoid collisions
  # (Excel has both k_alec and k_alec_fav — renaming _fav → base name would clash)
  cols_to_keep <- c("date", unname(rename_vec))
  nwmls_condo <- nwmls_raw %>%
    select(all_of(cols_to_keep)) %>%
    mutate(
      ym    = zoo::as.yearmon(date),
      year  = lubridate::year(date),
      month = lubridate::month(date)
    ) %>%
    rename(all_of(rename_vec)) %>%
    relocate(ym, year, month, .before = 1)

  # ---- Rolling lag windows: Seattle condo -------------------------------
  nwmls_condo_lagged <- nwmls_condo %>%
    select(ym, year, month,
           sea_pmedec, sea_sec, sea_alec,
           any_of(c("k_pmedec", "k_sec", "k_alec"))) %>%
    arrange(ym) %>%
    mutate(
      sea_pmedec_lag6  = slide_dbl(sea_pmedec, mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      sea_pmedec_lag12 = slide_dbl(sea_pmedec, mean, .before = 11, .complete = TRUE, na.rm = TRUE),
      sea_sec_lag6     = slide_dbl(sea_sec,    mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      sea_sec_lag12    = slide_dbl(sea_sec,    mean, .before = 11, .complete = TRUE, na.rm = TRUE),
      sea_alec_lag6    = slide_dbl(sea_alec,   mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      sea_alec_lag12   = slide_dbl(sea_alec,   mean, .before = 11, .complete = TRUE, na.rm = TRUE)
    )

  # ---- Rolling lag windows: King County condo (conditional) -------------
  if (has_kc_condo) {
    nwmls_condo_lagged <- nwmls_condo_lagged %>%
      mutate(
        k_pmedec_lag6  = slide_dbl(k_pmedec, mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
        k_pmedec_lag12 = slide_dbl(k_pmedec, mean, .before = 11, .complete = TRUE, na.rm = TRUE),
        k_sec_lag6     = slide_dbl(k_sec,    mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
        k_sec_lag12    = slide_dbl(k_sec,    mean, .before = 11, .complete = TRUE, na.rm = TRUE),
        k_alec_lag6    = slide_dbl(k_alec,   mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
        k_alec_lag12   = slide_dbl(k_alec,   mean, .before = 11, .complete = TRUE, na.rm = TRUE)
      )
  }

  # ---- Annual: December snapshot -> next tax_yr --------------------------
  nwmls_av_year_condo <- nwmls_condo_lagged %>%
    filter(month == 12) %>%
    arrange(year) %>%
    transmute(
      tax_yr = year + 1,
      nwmls_condo_med_price         = sea_pmedec_lag12,
      nwmls_condo_active_list       = sea_alec_lag12,
      nwmls_condo_closed_sales      = sea_sec_lag12,
      nwmls_condo_med_price_lag1    = lag(sea_pmedec_lag12, 1),
      nwmls_condo_med_price_lag2    = lag(sea_pmedec_lag12, 2),
      nwmls_condo_active_list_lag1  = lag(sea_alec_lag12, 1),
      nwmls_condo_active_list_lag2  = lag(sea_alec_lag12, 2),
      nwmls_condo_med_price_delta   = sea_pmedec_lag12 - lag(sea_pmedec_lag12, 1),
      nwmls_condo_active_list_delta = sea_alec_lag12 - lag(sea_alec_lag12, 1)
    )

  if (has_kc_condo) {
    kc_annual <- nwmls_condo_lagged %>%
      filter(month == 12) %>%
      arrange(year) %>%
      transmute(
        tax_yr = year + 1,
        nwmls_kc_condo_med_price          = k_pmedec_lag12,
        nwmls_kc_condo_active_list        = k_alec_lag12,
        nwmls_kc_condo_closed_sales       = k_sec_lag12,
        nwmls_kc_condo_med_price_lag1     = lag(k_pmedec_lag12, 1),
        nwmls_kc_condo_med_price_lag2     = lag(k_pmedec_lag12, 2),
        nwmls_kc_condo_active_list_lag1   = lag(k_alec_lag12, 1),
        nwmls_kc_condo_active_list_lag2   = lag(k_alec_lag12, 2),
        nwmls_kc_condo_med_price_delta    = k_pmedec_lag12 - lag(k_pmedec_lag12, 1),
        nwmls_kc_condo_active_list_delta  = k_alec_lag12 - lag(k_alec_lag12, 1)
      )

    nwmls_av_year_condo <- nwmls_av_year_condo %>%
      left_join(kc_annual, by = "tax_yr")
  }

  # ---- Cache forecast years ---------------------------------------------
  cache_dir <- get0("cache_dir", envir = .GlobalEnv,
                    ifnotfound = here("data", "cache"))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  nwmls_condo_fcst_out <- nwmls_av_year_condo %>%
    filter(tax_yr >= get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2027L) - 1L,
         tax_yr <= get0("forecast_end",   envir = .GlobalEnv, ifnotfound = 2032L)) %>%
    arrange(tax_yr) %>%
    distinct(tax_yr, .keep_all = TRUE)

  stopifnot("tax_yr" %in% names(nwmls_condo_fcst_out))
  stopifnot(nrow(nwmls_condo_fcst_out) > 0)

  nwmls_condo_fcst_path <- file.path(
    cache_dir,
    paste0("nwmls_condo_fcst_2026_2031_", scenario, ".rds")
  )
  saveRDS(nwmls_condo_fcst_out, nwmls_condo_fcst_path)
  message("\U0001f4be NWMLS condo forecast cached to: ", basename(nwmls_condo_fcst_path))

  # ---- Join to condo panel ----------------------------------------------
  panel_tbl_condo <- panel_tbl_condo %>%
    left_join(nwmls_av_year_condo, by = "tax_yr")

  message("xx_nwmls_condo_to_panel.R loaded (scenario = ", scenario,
          " | SEA condo = TRUE | KC condo = ", has_kc_condo, ")")
}
