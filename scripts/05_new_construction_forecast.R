####  New Construction Forecast
####  Author: Sean Thompson (OERF)
####
####  This script forecasts new construction AV based on construction sales tax data.
####  Logic from the old model:
####    - Import quarterly TRS construction sales tax forecast (baseline/pessimistic/optimistic)
####    - Apply a 6-quarter lag: for each tax year, sum the 4 quarters of lagged sales
####      (Q3 and Q4 of two years prior + Q1 and Q2 of one year prior)
####    - Apply a realization rate (47%) to convert sales tax to AV
####    - Stub in certified new construction for 2025 and 2026
####    - Relevel forecast scenarios off the certified 2026 base using growth multiples
####
####  Output: tax_year | baseline | optimistic | pessimistic

library(tidyverse)
library(readxl)
library(here)

# ── Configuration ──────────────────────────────────────────────────────────────

# Realization rate: fraction of construction sales tax that converts to AV
realization_rate <- 0.47

# Certified new construction values (from KC worksheet)
# Update these when new certified figures are available
cert_new_construction <- tibble(
  tax_year = c("2025", "2026"),
  baseline    = c(3975729781, 4511865685),
  optimistic  = c(3975729781, 4511865685),
  pessimistic = c(3975729781, 4511865685)
)

# ── 1. Import Construction Sales Tax Forecast ─────────────────────────────────

message("Importing construction sales tax forecast...")

## Read the TRS construction file
## Structure: columns E-J around rows 185-211 contain the quarterly forecast
## Column layout: quarter | ... | baseline | pessimistic | optimistic
## Pre-forecast quarters (actuals) are in column H only

trs_raw <- read_excel(
  here("data", "CoStar and SPG", "2026-07_trs_construction.xlsx"),
  sheet = "TRS CON"
)

## Extract the quarterly forecast data from the obligation quarter columns
## The data lives in columns E (quarter label) and H-J (baseline/pessimistic/optimistic)
## We need to find where the forecast section starts

# Read using cell references matching the structure we found
# Actuals: column E = quarter, column H = value (rows ~98 to 185)
# Forecast: column E = quarter, columns H/I/J = baseline/pessimistic/optimistic (rows ~188 to 211)

# Strategy: read column E and H-J from the full sheet, filter to relevant quarters
trs_full <- read_excel(
  here("data", "CoStar and SPG", "2026-07_trs_construction.xlsx"),
  sheet = "TRS CON",
  col_names = FALSE
)

## Identify the quarterly obligation data
## Column E (5) has quarter labels like "2024Q4", "2025Q1", etc.
## Column H (8) has values for actuals and baseline forecast
## Columns I (9) and J (10) have pessimistic and optimistic for forecast rows

quarter_data <- trs_full %>%
  select(quarter = 5, baseline = 8, pessimistic = 9, optimistic = 10) %>%
  filter(!is.na(quarter), str_detect(as.character(quarter), "^\\d{4}Q[1-4]$")) %>%
  mutate(
    quarter = as.character(quarter),
    baseline = as.numeric(baseline),
    pessimistic = as.numeric(pessimistic),
    optimistic = as.numeric(optimistic),
    # Forecast rows are the ones where the workbook populates the scenario
    # columns (I is NA for actuals).  For PRE-forecast rows, force BOTH
    # scenario columns to baseline — column J carries a ~1.0 ratio (not a
    # level) in the actuals region, which is non-NA and therefore survived
    # the old is.na() backfill, deflating the optimistic lagged sums by
    # ~1000x and blowing up the relevel multiplier (mult_2027 ~ 960x).
    is_forecast = !is.na(pessimistic),
    pessimistic = if_else(is_forecast, pessimistic, baseline),
    optimistic  = if_else(is_forecast, optimistic,  baseline)
  )

## Guard: scenario levels should be the same order of magnitude as baseline
## in every quarter.  A ratio/index or unit mismatch shows up here first.
bad_q <- quarter_data %>%
  filter(baseline > 0,
         optimistic / baseline < 0.5 | optimistic / baseline > 2 |
         pessimistic / baseline < 0.5 | pessimistic / baseline > 2)
if (nrow(bad_q) > 0) {
  print(bad_q)
  stop("Scenario columns out of range vs baseline for the quarters above — ",
       "check TRS CON column layout (E/H/I/J) before proceeding.")
}
quarter_data <- quarter_data %>% select(-is_forecast)

message("  Quarters found: ", nrow(quarter_data))
message("  Range: ", min(quarter_data$quarter), " to ", max(quarter_data$quarter))

# ── 2. Stub in Recent Actuals ────────────────────────────────────────────────

## The old script manually stubbed in recent actual quarters before the forecast.
## These should already be in the data. Verify we have the quarters we need:
needed_quarters <- c("2024Q3", "2024Q4", "2025Q1", "2025Q2")
missing <- setdiff(needed_quarters, quarter_data$quarter)
if (length(missing) > 0) {
  warning("Missing quarters in TRS data: ", paste(missing, collapse = ", "))
}

# ── 3. Compute Lagged New Construction ───────────────────────────────────────

message("Computing lagged new construction forecast...")

## For each tax year, new construction = sum of 4 quarters with a 6-quarter lag
## Tax year N uses: Q3(N-2) + Q4(N-2) + Q1(N-1) + Q2(N-1)
## Equivalently: the Q1 of each year triggers the sum of the preceding 4 quarters
## offset by 2 (so lag 2 through lag 5 from each Q1)

new_con_long <- quarter_data %>%
  pivot_longer(
    cols = c(baseline, pessimistic, optimistic),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  arrange(scenario, quarter) %>%
  group_by(scenario) %>%
  mutate(
    # For each Q1, sum the values at lags 2, 3, 4, 5
    new_con_forecast = if_else(
      str_detect(quarter, "Q1"),
      lag(value, 2) + lag(value, 3) + lag(value, 4) + lag(value, 5),
      NA_real_
    ),
    tax_year = str_sub(quarter, 1, 4)
  ) %>%
  filter(!is.na(new_con_forecast)) %>%
  group_by(tax_year, scenario) %>%
  summarise(
    new_con_forecast = sum(new_con_forecast * realization_rate * 1e6),
    .groups = "drop"
  )

new_con_wide <- new_con_long %>%
  pivot_wider(names_from = scenario, values_from = new_con_forecast) %>%
  filter(tax_year >= "2027")  # 2025 and 2026 use certified values

message("  Raw new construction forecast (post-lag, pre-rebase):")
print(new_con_wide)

# ── 4. Relevel Off Certified Base ────────────────────────────────────────────

message("Releveling new construction forecast off certified 2026 base...")

## Compute growth multiples from the raw forecast
mult_tbl <- new_con_wide %>%
  pivot_longer(-tax_year, names_to = "scenario", values_to = "value") %>%
  arrange(scenario, tax_year) %>%
  group_by(scenario) %>%
  mutate(mult = value / lag(value)) %>%
  ungroup() %>%
  select(scenario, tax_year, mult)

## Build the releveled series:
## 2025 = certified, 2026 = certified, 2027+ = 2026_certified * cumulative multipliers

# Get all forecast years
all_years <- sort(unique(c("2025", "2026", new_con_wide$tax_year)))
scenarios <- c("baseline", "pessimistic", "optimistic")

skeleton <- expand_grid(
  tax_year = all_years,
  scenario = scenarios
)

cert_long <- cert_new_construction %>%
  pivot_longer(-tax_year, names_to = "scenario", values_to = "cert_value")

cert_2025 <- cert_long %>%
  filter(tax_year == "2025") %>%
  select(scenario, y2025 = cert_value)

cert_2026 <- cert_long %>%
  filter(tax_year == "2026") %>%
  select(scenario, y2026 = cert_value)

releveled <- skeleton %>%
  left_join(mult_tbl, by = c("scenario", "tax_year")) %>%
  left_join(cert_2025, by = "scenario") %>%
  left_join(cert_2026, by = "scenario") %>%
  arrange(scenario, tax_year) %>%
  group_by(scenario) %>%
  mutate(
    # For 2025 and 2026, mult factor = 1 (use certified directly)
    # For 2027+, use the growth multiples from the raw forecast
    mult_safe = case_when(
      tax_year <= "2026" ~ 1,
      TRUE ~ mult
    )
  )

## For 2027, the first mult in mult_tbl is relative to the raw 2027/2026 ratio.
## But since we've only got mults starting from 2028 (2027 has no lag in mult_tbl),
## we need the actual 2027 value from the raw forecast and compute its ratio to raw 2026.

## Get raw 2026 values to compute the 2027 multiplier
## 2026 in the raw forecast = the Q1 2026 lagged sum (before filtering to >= 2027)
raw_2026 <- new_con_long %>%
  filter(tax_year == "2026") %>%
  select(scenario, raw_2026 = new_con_forecast)

raw_2027 <- new_con_long %>%
  filter(tax_year == "2027") %>%
  select(scenario, raw_2027 = new_con_forecast)

mult_2027 <- raw_2027 %>%
  left_join(raw_2026, by = "scenario") %>%
  mutate(mult = raw_2027 / raw_2026) %>%
  select(scenario, mult)

## Now chain multiply from the certified 2026 base
new_construction_forecast <- releveled %>%
  left_join(mult_2027 %>% rename(mult_2027_val = mult), by = "scenario") %>%
  mutate(
    # Override the first forecast year mult with the raw ratio
    mult_final = case_when(
      tax_year == "2027" ~ mult_2027_val,
      tax_year <= "2026" ~ 1,
      TRUE ~ mult
    ),
    # Cumulative product from 2026 base
    factor = cumprod(mult_final),
    value = case_when(
      tax_year == "2025" ~ y2025,
      TRUE ~ y2026 * factor
    )
  ) %>%
  ungroup() %>%
  select(tax_year, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  arrange(tax_year)

message("\n=== New Construction Forecast ===")
print(new_construction_forecast)

# ── 5. Write Out ──────────────────────────────────────────────────────────────

write_csv(
  new_construction_forecast,
  here(str_c("OERF_New_Construction_Forecast_", today(), ".csv"))
)

message("New construction forecast written.")
