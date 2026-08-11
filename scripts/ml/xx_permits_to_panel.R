# scripts/ml/xx_permits_to_panel.R ------------------------------------
message("Running xx_permits_to_panel.R (robust) ...")

panel_tbl <- as_tibble(panel)
# ---- 0) Config ----
permit_file <- here::here("data", "permits", "New Construction for OERF AV 2026-07-28.xlsx")

permit_predictors <- c(
  "permits_last_1yr","permits_last_3yr","permits_last_5yr",
  "val_last_3yr","val_last_5yr",
  "sqft_last_3yr","units_last_3yr",
  "any_newconst","years_since_newconst",
  "log_val_last_3yr","log_sqft_last_3yr"
)

# ---- 1) Import raw sheets ----
permit_rec_raw  <- read_excel(permit_file, sheet = "record") %>% clean_names()
permit_gis_raw  <- read_excel(permit_file, sheet = "gis")    %>% clean_names()
permit_sqft_raw <- read_excel(permit_file, sheet = "sqft")   %>% clean_names()

# ---- 2) Clean GIS -> parcel_id ----
permit_gis <- permit_gis_raw %>%
  mutate(
    kc_parcel = str_pad(as.character(kc_parcel), 10, pad = "0"),
    parcel_id = str_c(str_sub(kc_parcel, 1, 6), "-", str_sub(kc_parcel, 7, 10))
  )

# ---- 3) Summarise sqft lines per record_number ----
permit_sqft <- permit_sqft_raw %>%
  group_by(record_number) %>%
  summarise(
    sqft_total   = sum(sqft, na.rm = TRUE),
    n_sqft_lines = n(),
    .groups = "drop"
  ) %>%
  mutate(sqft_total = na_if(sqft_total, 0))

# ---- 4) Combine into one permits table ----
permits <- permit_gis %>%
  left_join(permit_rec_raw,  by = "record_number", suffix = c("_gis","_rec")) %>%
  left_join(permit_sqft,     by = "record_number") %>%
  clean_names()  # re-clean to guarantee unique, repaired colnames

# ---- 5) Choose best event date & derive year (only from cols that exist) ----
date_cols <- c(
  "certificate_of_occupancy_date",
  "inspections_completed_date_gis",
  "permit_issued_date_gis",
  "application_completed_date_gis"
)
date_cols <- intersect(date_cols, names(permits))
if (length(date_cols) == 0) stop("No usable date columns found in permits.")

permits <- permits %>%
  mutate(
    event_date = coalesce(!!!syms(date_cols)),
    event_year = lubridate::year(event_date)
  )

# ---- 6) Annual parcel-year permit features (force uniqueness) ----
permit_features_annual <- permits %>%
  filter(!is.na(parcel_id), !is.na(event_year)) %>%
  group_by(parcel_id, event_year) %>%
  summarise(
    permits_cnt      = n(),
    permits_val_sum  = sum(issuance_valuation_total_gis, na.rm = TRUE),
    permits_sqft_sum = sum(sqft_total, na.rm = TRUE),
    units_added_sum  = sum(housing_units_added_gis, na.rm = TRUE),
    any_newconst_permit = 1L,
    .groups = "drop"
  ) %>%
  distinct(parcel_id, event_year, .keep_all = TRUE)

# ---- 7) Join annual permits into panel years (no collisions) ----
panel_tbl <- panel_tbl %>%
  mutate(
    parcel_id = as.character(parcel_id),
    tax_yr    = as.integer(tax_yr)
  ) %>%
  left_join(
    permit_features_annual,
    by = c("parcel_id" = "parcel_id", "tax_yr" = "event_year"),
    relationship = "many-to-one"
  )

# ---- 8) Zero-fill annual cols before rolling ----
annual_cols <- c("permits_cnt","permits_val_sum","permits_sqft_sum",
                 "units_added_sum","any_newconst_permit")
annual_cols_present <- annual_cols[annual_cols %in% names(panel_tbl)]

panel_tbl <- panel_tbl %>%
  mutate(across(all_of(annual_cols_present), ~ replace_na(.x, 0)))

# ---- 9) Rolling windows by parcel over tax_yr ----
panel_tbl <- panel_tbl %>%
  group_by(parcel_id) %>%
  arrange(tax_yr) %>%
  mutate(
    permits_last_1yr = permits_cnt,
    permits_last_3yr = rollapplyr(permits_cnt, 3, sum, fill = 0, partial = TRUE),
    permits_last_5yr = rollapplyr(permits_cnt, 5, sum, fill = 0, partial = TRUE),
    
    val_last_3yr   = rollapplyr(permits_val_sum, 3, sum, fill = 0, partial = TRUE),
    val_last_5yr   = rollapplyr(permits_val_sum, 5, sum, fill = 0, partial = TRUE),
    sqft_last_3yr  = rollapplyr(permits_sqft_sum, 3, sum, fill = 0, partial = TRUE),
    units_last_3yr = rollapplyr(units_added_sum, 3, sum, fill = 0, partial = TRUE),
    
    years_since_newconst = {
      last_nc_year <- if_else(any_newconst_permit == 1, tax_yr, NA_integer_)
      last_nc_year <- na.locf(last_nc_year, na.rm = FALSE)
      if_else(is.na(last_nc_year), NA_real_, tax_yr - last_nc_year)
    }
  ) %>%
  ungroup()

# ---- 10) Build/merge any_newconst safely ----
# If panel already had any_newconst, keep max(panel, permits).
if ("any_newconst" %in% names(panel_tbl)) {
  panel_tbl <- panel_tbl %>%
    mutate(any_newconst = pmax(any_newconst, any_newconst_permit, na.rm = TRUE))
} else {
  panel_tbl <- panel_tbl %>%
    mutate(any_newconst = any_newconst_permit)
}

# ---- 11) Zero-fill rolling predictors only ----
roll_cols <- c(
  "permits_last_1yr","permits_last_3yr","permits_last_5yr",
  "val_last_3yr","val_last_5yr",
  "sqft_last_3yr","units_last_3yr",
  "years_since_newconst","any_newconst"
)
roll_cols_present <- roll_cols[roll_cols %in% names(panel_tbl)]

panel_tbl <- panel_tbl %>%
  mutate(across(all_of(roll_cols_present), ~ replace_na(.x, 0)))

# ---- 12) Log transforms ----
if ("val_last_3yr" %in% names(panel_tbl)) {
  panel_tbl <- panel_tbl %>% mutate(log_val_last_3yr = log1p(val_last_3yr))
}
if ("sqft_last_3yr" %in% names(panel_tbl)) {
  panel_tbl <- panel_tbl %>% mutate(log_sqft_last_3yr = log1p(sqft_last_3yr))
}

panel_tbl <- panel_tbl %>%
  mutate(
    appr_land_val     = if_else(appr_land_val <= 0, NA_real_, appr_land_val),
    log_appr_land_val = log(appr_land_val)
  ) %>%
  group_by(parcel_id) %>%
  arrange(tax_yr) %>%
  mutate(
    log_appr_land_val_lag1 = lag(log_appr_land_val, 1),
    log_appr_land_val_lag2 = lag(log_appr_land_val, 2)
  ) %>%
  ungroup()

# ---- 13) Optional: drop raw annual cols to reduce clutter ----
# panel_tbl <- panel_tbl %>% select(-any_of(annual_cols), -any_newconst_permit)

message("✅ permits added to panel_tbl. New permit cols: ",
        paste(intersect(permit_predictors, names(panel_tbl)), collapse = ", "))
message("xx_permits_to_panel.R loaded")
