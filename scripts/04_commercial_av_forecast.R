####  Commercial AV Forecast (Aggregate Method)
####  Author: Sean Thompson (OERF)
####  
####  This script handles commercial properties using the aggregate approach:
####    - Imports KCA parcel/tax data
####    - Assigns parcels to forecast groups using:
####        1. spec_area crosswalk (special commercial districts)
####        2. present_use codes (general commercial, by building type)
####    - Imports CoStar forecasts (hospitality, industrial, multifamily, office, retail)
####    - Applies YoY growth rates from CoStar to grouped commercial AV
####    - Produces baseline/optimistic/pessimistic scenarios through 2031
####  
####  Residential and Condo properties are handled by the ML pipeline.

library(tidyverse)
library(readxl)
library(janitor)
library(here)

# ── Configuration ──────────────────────────────────────────────────────────────

kca_date_data_extracted <- "2026-03-27"

# Levy codes for City of Seattle (per KC Rate Book 2026)
levy_code_list <- c(
  "0010", "0011", "0013", "0014", "0016", "0025", "0030", "0032"
)

# Scenario adjustment: optimistic +1pp, pessimistic -1pp
scenario_adj <- 0.01

# Forecast horizon
forecast_years <- 2026:2031

# ── Present Use → Forecast Group Mapping ──────────────────────────────────────
# Used to assign general commercial parcels (no spec_area) to industry groups
# based on their KCA present_use code.

present_use_mapping <- tribble(
  ~present_use, ~forecast_group,
  # Office
  300L,         "Major Office",       # Office Building
  309L,         "Major Office",       # Office Park
  # Banks would go here if code is identified

  # Medical / Life Science
  304L,         "Medical/Life Science", # Medical/Dental Office
  426L,         "Medical/Life Science", # Hospital

  # Industrial
  365L,         "Industrial",         # Mini-Storage/Warehouse
  406L,         "Industrial",         # Warehouse
  407L,         "Industrial",         # Manufacturing/Industrial
  528L,         "Industrial",         # Garage/Service Repair

  # Retail
  326L,         "Retail",             # Gas Station
  348L,         "Retail",             # Car Wash
  349L,         "Retail",             # Auto Dealer
  350L,         "Retail",             # Shopping Center/Mall
  352L,         "Retail",             # Retail Store
  353L,         "Retail",             # Supermarket/Grocery
  531L,         "Retail",             # Car Lot/Dealer

  # Hospitality (restaurants + hotels)
  341L,         "Hotels",             # Fast Food
  344L,         "Hotels",             # Restaurant/Lounge
  386L,         "Hotels",             # Hotel/Motel

  # Multifamily
  419L,         "Multifamily",        # Nursing Home
  470L,         "Multifamily",        # Apartments (5+ units)
  472L,         "Multifamily",        # Senior Housing
  706L,         "Multifamily",        # Mixed Use Commercial (often apt over retail)

  # Other / Institutional (no CoStar forecast — gets blended rate)
  311L,         "Other",              # Recreation/Club
  453L,         "Other",              # Church/Religious
  459L,         "Other",              # School/University
  494L,         "Other",              # Community Center
  551L,         "Other",              # Parking Garage
  820L,         "Other",              # Park/Open Space
  830L,         "Other",              # Golf Course
  840L,         "Other",              # Cemetery
  860L,         "Other"               # Utility/Infrastructure
)

# Blend weights for "Other" parcels that can't be assigned to an industry
other_blend_wt <- c(
  "Major Office" = 0.22,
  "Industrial"   = 0.25,
  "Retail"       = 0.40,
  "Multifamily"  = 0.13
)

# ── 1. Data Import ────────────────────────────────────────────────────────────

message("Importing KCA data...")

## Crosswalks
commercial_crosswalk <-
  read_excel(here("data", "crosswalk.xlsx"), sheet = "commercial")
commercial2_crosswalk <-
  read_excel(here("data", "crosswalk.xlsx"), sheet = "commercial2")

## KCA parcel and tax account data
real_prop_acct_receivable <- read_csv(
  here("data", "kca", kca_date_data_extracted, "EXTR_RPAcct_NoName.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  filter(levy_code %in% levy_code_list)

parcel <- read_csv(
  here("data", "kca", kca_date_data_extracted, "EXTR_Parcel.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  filter(levy_code %in% levy_code_list)

# ── 2. Commercial Grouping ───────────────────────────────────────────────────

message("Grouping commercial parcels...")

max_bill_yr <- max(real_prop_acct_receivable$bill_yr, na.rm = TRUE)
base_tax_year <- max_bill_yr
message("  Using bill_yr = ", max_bill_yr, " (base tax year = ", base_tax_year, ")")

tax_acct <- real_prop_acct_receivable %>%
  filter(
    levy_code %in% levy_code_list,
    bill_yr == max_bill_yr,
    tax_stat == "T"
  ) %>%
  mutate(parcel_key = str_c(major, minor)) %>%
  select(acct_nbr, parcel_key, appr_land_val, appr_imps_val, major, minor)

parcel_location <- parcel %>%
  filter(levy_code %in% levy_code_list) %>%
  mutate(parcel_key = str_c(major, minor)) %>%
  select(parcel_key, prop_type, area, spec_area, sub_area, spec_sub_area,
         present_use)

tax_acct_parcel <- tax_acct %>%
  left_join(parcel_location, by = "parcel_key")

## Commercial stub: prop_type C only
commercial_stub <- tax_acct_parcel %>%
  filter(prop_type == "C") %>%
  mutate(av = appr_land_val + appr_imps_val)

message("  Total commercial parcels: ", comma(nrow(commercial_stub)))
message("  Total commercial AV: ", scales::dollar(sum(commercial_stub$av, na.rm = TRUE),
                                                    scale = 1e-9, suffix = "B"))

# ── 2a. Special commercial (has spec_area) ────────────────────────────────────

special_stub <- commercial_stub %>%
  filter(!is.na(spec_area), spec_area != 0)

com_spec_by_industry <- special_stub %>%
  left_join(commercial2_crosswalk, by = "spec_area") %>%
  mutate(
    forecast_group = case_when(
      name %in% c("Apartment", "Nursinghome") ~ "Multifamily",
      name %in% c("Industrial", "Warehouse", "Biotech") ~ "Industrial",
      name %in% c("MajorRetail", "Restaurants") ~ "Retail",
      name == "Hotel"       ~ "Hotels",
      name == "MajorOffice" ~ "Major Office",
      TRUE                  ~ "Other"
    )
  ) %>%
  group_by(forecast_group) %>%
  summarise(sum_val_base = sum(av, na.rm = TRUE),
            n_parcels = n(), .groups = "drop") %>%
  mutate(source = "spec_area")

message("\n  Special commercial (spec_area):")
print(com_spec_by_industry)

# ── 2b. General commercial (no spec_area) — assign by present_use ────────────

general_stub <- commercial_stub %>%
  filter(is.na(spec_area) | spec_area == 0)

# Join present_use mapping
general_mapped <- general_stub %>%
  left_join(present_use_mapping, by = "present_use")

# Parcels that matched a present_use code
general_assigned <- general_mapped %>%
  filter(!is.na(forecast_group), forecast_group != "Other") %>%
  group_by(forecast_group) %>%
  summarise(sum_val_base = sum(av, na.rm = TRUE),
            n_parcels = n(), .groups = "drop") %>%
  mutate(source = "present_use")

# Parcels that didn't match or mapped to "Other" — get blended rate
general_unassigned <- general_mapped %>%
  filter(is.na(forecast_group) | forecast_group == "Other")

unassigned_total <- sum(general_unassigned$av, na.rm = TRUE)
unassigned_n <- nrow(general_unassigned)

general_blended <- tibble(
  forecast_group = names(other_blend_wt),
  sum_val_base = unassigned_total * other_blend_wt,
  n_parcels = NA_integer_,
  source = "blended"
)

message("\n  General commercial by present_use:")
print(general_assigned)
message("\n  Unassigned (blended): ", comma(unassigned_n), " parcels, ",
        scales::dollar(unassigned_total, scale = 1e-9, suffix = "B"))

# ── 2c. Combine all into forecast groups ──────────────────────────────────────

com_av_by_group <- bind_rows(com_spec_by_industry, general_assigned, general_blended) %>%
  group_by(forecast_group) %>%
  summarise(sum_val_base = sum(sum_val_base, na.rm = TRUE),
            n_parcels = sum(n_parcels, na.rm = TRUE), .groups = "drop")

# Medical/Life Science parcels get the multifamily CoStar rate as a proxy
# (closest available; no dedicated CoStar medical index)
# Map them to their own display group but use Multifamily growth rates
med_group <- com_av_by_group %>% filter(forecast_group == "Medical/Life Science")
if (nrow(med_group) > 0) {
  message("\n  Medical/Life Science: ",
          scales::dollar(med_group$sum_val_base, scale = 1e-9, suffix = "B"),
          " (will use Multifamily growth rate as proxy)")
}

message("\n=== Commercial AV by Forecast Group ===")
print(com_av_by_group %>%
        mutate(av_b = scales::dollar(sum_val_base, scale = 1e-9, suffix = "B")))

# ── 3. Import CoStar Forecasts ───────────────────────────────────────────────

message("\nImporting CoStar forecasts...")

## Hospitality (monthly data, use January, Market Sale Price/Room)
hospitality_forecast_cln <- read_excel(
  here("data", "CoStar and SPG", "CoStar for AV - hospitality - 2026-03-31.xlsx")
) %>%
  clean_names() %>%
  select(period, market_sale_price_room) %>%
  mutate(date = as.Date(period), year = as.character(year(period))) %>%
  filter(str_detect(as.character(date), "-01-"), year >= 2018) %>%
  group_by(year) %>%
  summarise(lvl = sum(market_sale_price_room, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(
    gyy = (lvl / lag(lvl)) - 1,
    forecast_group = "Hotels"
  )

## Industrial (quarterly, use Q1, Market Sale Price Index)
industrial_forecast_cln <- read_excel(
  here("data", "CoStar and SPG", "CoStar for AV - industrial - 2026-03-31.xlsx")
) %>%
  clean_names() %>%
  select(period, market_sale_price_index) %>%
  filter(str_detect(period, "Q1")) %>%
  filter(!(period == "2026 Q1 QTD")) %>%
  mutate(
    period = str_replace(period, " Q1 EST", " Q1"),
    year = str_sub(period, 1, 4)
  ) %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  summarise(lvl = mean(market_sale_price_index), .groups = "drop") %>%
  mutate(
    gyy = (lvl / lag(lvl)) - 1,
    forecast_group = "Industrial"
  )

## Multifamily
multifamily_forecast_cln <- read_excel(
  here("data", "CoStar and SPG", "CoStar for AV - multifamily - 2026-03-31.xlsx")
) %>%
  clean_names() %>%
  select(period, market_sale_price_index) %>%
  filter(str_detect(period, "Q1")) %>%
  filter(!(period == "2026 Q1 QTD")) %>%
  mutate(
    period = str_replace(period, " Q1 EST", " Q1"),
    year = str_sub(period, 1, 4)
  ) %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  summarise(lvl = mean(market_sale_price_index), .groups = "drop") %>%
  mutate(
    gyy = (lvl / lag(lvl)) - 1,
    forecast_group = "Multifamily"
  )

## Office
office_forecast_cln <- read_excel(
  here("data", "CoStar and SPG", "CoStar for AV - office - 2026-03-31.xlsx")
) %>%
  clean_names() %>%
  select(period, market_sale_price_index) %>%
  filter(str_detect(period, "Q1")) %>%
  filter(!(period == "2026 Q1 QTD")) %>%
  mutate(
    period = str_replace(period, " Q1 EST", " Q1"),
    year = str_sub(period, 1, 4)
  ) %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  summarise(lvl = mean(market_sale_price_index), .groups = "drop") %>%
  mutate(
    gyy = (lvl / lag(lvl)) - 1,
    forecast_group = "Major Office"
  )

## Retail
retail_forecast_cln <- read_excel(
  here("data", "CoStar and SPG", "CoStar for AV - retail - 2026-03-31.xlsx")
) %>%
  clean_names() %>%
  select(period, market_sale_price_index) %>%
  filter(str_detect(period, "Q1")) %>%
  filter(!(period == "2026 Q1 QTD")) %>%
  mutate(
    period = str_replace(period, " Q1 EST", " Q1"),
    year = str_sub(period, 1, 4)
  ) %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  summarise(lvl = mean(market_sale_price_index), .groups = "drop") %>%
  mutate(
    gyy = (lvl / lag(lvl)) - 1,
    forecast_group = "Retail"
  )

## Combine all CoStar forecasts
forecast_all <- bind_rows(
  hospitality_forecast_cln,
  industrial_forecast_cln,
  multifamily_forecast_cln,
  office_forecast_cln,
  retail_forecast_cln
) %>%
  select(year, gyy, forecast_group)

# ── 4. Map Forecast Groups to CoStar Growth Rates ────────────────────────────

# Medical/Life Science uses Multifamily growth rates as proxy
# Map forecast_group → CoStar group for growth rate lookup
costar_mapping <- tribble(
  ~forecast_group,       ~costar_group,
  "Major Office",        "Major Office",
  "Industrial",          "Industrial",
  "Retail",              "Retail",
  "Hotels",              "Hotels",
  "Multifamily",         "Multifamily",
  "Medical/Life Science","Multifamily"
)

# Remap com_av_by_group to include CoStar group
com_av_mapped <- com_av_by_group %>%
  left_join(costar_mapping, by = "forecast_group") %>%
  mutate(costar_group = if_else(is.na(costar_group), "Retail", costar_group))
  # Fallback: unmatched groups use Retail growth

message("\n=== Forecast Group → CoStar Mapping ===")
print(com_av_mapped %>%
        select(forecast_group, costar_group, sum_val_base, n_parcels) %>%
        mutate(av_b = round(sum_val_base / 1e9, 2)))

# ── 5. Apply Growth Rates & Generate Forecast ────────────────────────────────

message("\nGenerating commercial AV forecast...")

growth_rates <- forecast_all %>%
  filter(year >= as.character(base_tax_year), year <= "2031") %>%
  select(forecast_group, year, gyy) %>%
  rename(costar_group = forecast_group) %>%
  pivot_wider(names_from = year, values_from = gyy, names_prefix = "g")

## Join AV base with growth rates via CoStar mapping
com_forecast_wide <- com_av_mapped %>%
  left_join(growth_rates, by = "costar_group")

## Compound AV forward by forecast group
g_cols <- names(com_forecast_wide)[str_detect(names(com_forecast_wide), "^g\\d{4}$")]
g_years <- as.numeric(str_replace(g_cols, "^g", ""))

com_forecast_long <- com_forecast_wide %>%
  mutate(!!str_c("av_", base_tax_year) := sum_val_base)

for (yr in g_years) {
  prev_col <- str_c("av_", yr)
  next_col <- str_c("av_", yr + 1)
  g_col <- str_c("g", yr)
  com_forecast_long <- com_forecast_long %>%
    mutate(!!next_col := .data[[prev_col]] * (1 + .data[[g_col]]))
}

## Pivot to long and sum across all groups for total
com_forecast_detail <- com_forecast_long %>%
  select(forecast_group, costar_group, starts_with("av_")) %>%
  pivot_longer(
    cols = starts_with("av_"),
    names_to = "tax_year",
    values_to = "assessed_value"
  ) %>%
  mutate(tax_year = str_replace(tax_year, "av_", ""))

## Detail by forecast group
message("\n=== Commercial AV Forecast by Group ($B) ===")
print(
  com_forecast_detail %>%
    mutate(av_b = round(assessed_value / 1e9, 2)) %>%
    select(tax_year, forecast_group, av_b) %>%
    pivot_wider(names_from = forecast_group, values_from = av_b)
)

## Total across all groups, then apply scenarios
com_total_baseline <- com_forecast_detail %>%
  group_by(tax_year) %>%
  summarise(baseline = sum(assessed_value), .groups = "drop") %>%
  arrange(tax_year)

com_total_baseline <- com_total_baseline %>%
  mutate(gyy_baseline = baseline / lag(baseline) - 1)

## Build scenarios
com_av_forecast <- com_total_baseline %>%
  mutate(
    optimistic = if_else(tax_year == as.character(base_tax_year), baseline, NA_real_),
    pessimistic = if_else(tax_year == as.character(base_tax_year), baseline, NA_real_)
  )

for (i in 2:nrow(com_av_forecast)) {
  gyy_b <- com_av_forecast$gyy_baseline[i]

  com_av_forecast$optimistic[i] <-
    com_av_forecast$optimistic[i - 1] * (1 + gyy_b + scenario_adj)

  com_av_forecast$pessimistic[i] <-
    com_av_forecast$pessimistic[i - 1] * (1 + gyy_b - scenario_adj)
}

commercial_av_forecast_final <- com_av_forecast %>%
  select(tax_year, baseline, optimistic, pessimistic)

message("\n=== Commercial AV Forecast (Total) ===")
print(commercial_av_forecast_final)

# ── 6. Write Out ──────────────────────────────────────────────────────────────

write_csv(
  commercial_av_forecast_final,
  here("data", "wrangled", str_c("commercial_av_forecast_", today(), ".csv"))
)

# Also export the detail by forecast group
write_csv(
  com_forecast_detail %>%
    mutate(tax_year = as.integer(tax_year)) %>%
    select(tax_year, forecast_group, costar_group, assessed_value),
  here("data", "wrangled", str_c("commercial_av_by_group_", today(), ".csv"))
)

message("Commercial AV forecast written to data/wrangled/")
