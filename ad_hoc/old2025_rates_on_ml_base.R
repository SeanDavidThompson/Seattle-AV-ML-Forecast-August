# old2025_rates_on_ml_base.R
# Applies the Apr/Aug/Oct 2025 old-model growth rates to the ML pipeline's
# tax-year-2025 base (matched existing parcels, by track) and computes the
# tax-year-2026 AV each vintage implies vs actual, in dollars.
#
# Year mapping: the forecast_all_* files are labelled by ASSESSMENT year,
# so file year Y = tax year Y + 1 (checked on residential history:
# file 2023 -7.3% ~ TY2024 actual -7.6%; file 2024 +7.6-8.7% ~ TY2025 actual +8.0%).
# Save to ad_hoc/ and source().

library(dplyr); library(readr); library(data.table); library(here)

read_bt <- function(f) {
  x <- readLines(here("data", "outputs", "backtest", f))
  fread(text = x[!startsWith(x, "#")])
}

group_map <- c(Hotels = "hospitality", Industrial = "industrial", Multifamily = "apt",
               `Major Office` = "office", Retail = "retail", Residential = "res")

files <- c(`Apr 2025` = "forecast_all_apr2026-09-29.csv",
           `Aug 2025` = "forecast_all_aug2026-09-29.csv",
           `Oct 2025` = "forecast_all_oct2026-09-29.csv")

old <- bind_rows(lapply(names(files), \(v)
  read_csv(here("data", "wrangled", files[[v]]), show_col_types = FALSE) |>
    transmute(vintage = v, group = forecast_group, tax_yr = year + 1, old_g = gyy)))

# Base + actuals from the backtest: for each tax year, take the latest origin's row
gy <- read_bt("growth_by_year.csv")[pop == "seed_observed" & anchored == TRUE]
base <- as_tibble(gy) |>
  group_by(track, tax_yr) |> slice_max(origin, n = 1) |> ungroup() |>
  transmute(track, tax_yr, av_base, act_g = g_act,
            ml_prev_origin = origin, ml_prev_h = horizon, ml_prev_g = g_pred)

cmp <- old |>
  mutate(track = unname(group_map[group])) |>
  filter(!is.na(track), tax_yr == 2026) |>          # only year with both a forecast and an actual
  inner_join(base, by = c("track", "tax_yr")) |>
  mutate(base_B   = av_base / 1e9,
         act_B    = av_base * (1 + act_g) / 1e9,
         old_B    = av_base * (1 + old_g) / 1e9,
         diff_B   = old_B - act_B,
         err_pp   = 100 * (old_g - act_g),
         ml_prev_diff_B = av_base * (ml_prev_g - act_g) / 1e9)

cmp |>
  select(vintage, group, base_B, act_g, old_g, err_pp, act_B, old_B, diff_B,
         ml_prev_g, ml_prev_diff_B) |>
  mutate(across(c(act_g, old_g, ml_prev_g), \(x) round(100 * x, 2)),
         across(where(is.double), \(x) round(x, 2))) |>
  print(n = Inf)

cmp |> group_by(vintage) |>
  summarise(net_diff_B = sum(diff_B), gross_diff_B = sum(abs(diff_B)), .groups = "drop") |>
  print()

fwrite(cmp, here("data", "outputs", "backtest", "old2025_vs_actual_ty2026.csv"))
