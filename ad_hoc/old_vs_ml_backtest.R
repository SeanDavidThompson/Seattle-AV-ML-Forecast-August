# old_vs_ml_backtest.R
# Scores the old OERF method (national growth rates + area reports, by prop type)
# against the ML backtest, both measured against the SAME actuals:
# matched existing-parcel revaluation growth by track from growth_by_year.csv.
# Old-model growth is taken within each vintage (year-over-year off its own base),
# so the group redefinitions between vintages don't contaminate the comparison.
# Save to ad_hoc/ and run with source() after av_fcst_hist is loaded (or let it load).

library(dplyr); library(tidyr); library(readxl); library(data.table); library(here)

if (!exists("av_fcst_hist")) {
  av_fcst_hist <- read_xlsx(here("data", "av_history_by_prop_type.xlsx"), sheet = "Sheet1")
}

read_bt <- function(f) {
  x <- readLines(here("data", "outputs", "backtest", f))
  fread(text = x[!startsWith(x, "#")])
}

gy  <- read_bt("growth_by_year.csv")[pop == "seed_observed" & anchored == TRUE]
act <- as_tibble(gy[, .(g_act = mean(g_act)), by = .(track, tax_yr)])
ml  <- as_tibble(gy[, .(origin, track, tax_yr, ml_g = 100 * g_pred, ml_cum_err = cum_err_pp)])

type_map <- c(residential = "res", condo = "condo", hospitality = "hospitality",
              apartment = "apt", retail = "retail", industrial = "industrial",
              office = "office")
last_actual <- max(act$tax_yr)

cmp <- av_fcst_hist |>
  mutate(vintage = as.Date(vintage),
         vy      = as.integer(format(vintage, "%Y")),
         track   = unname(type_map[prop_type])) |>
  filter(!is.na(track), year >= vy, year <= last_actual) |>
  arrange(vintage, prop_type, year) |>
  group_by(vintage, prop_type) |>
  mutate(h = year - vy,
         old_g   = 100 * (av / lag(av) - 1),
         old_cum = av / first(av)) |>
  filter(h >= 1) |>
  left_join(act, by = c("track", "year" = "tax_yr")) |>
  mutate(act_g       = 100 * g_act,
         act_cum     = cumprod(1 + g_act),
         old_cum_err = 100 * (old_cum - act_cum)) |>
  ungroup() |>
  left_join(ml, by = c("vy" = "origin", "track", "year" = "tax_yr")) |>
  select(vintage, prop_type, tax_yr = year, h, old_g, act_g, ml_g, old_cum_err, ml_cum_err)

fwrite(cmp, here("data", "outputs", "backtest", "old_vs_ml_by_vintage.csv"))

# 1) Headline: mean absolute cumulative error by type and horizon
summ <- cmp |>
  group_by(prop_type, h) |>
  summarise(n = n(),
            old_mae = mean(abs(old_cum_err)), ml_mae = mean(abs(ml_cum_err)),
            old_bias = mean(old_cum_err),     ml_bias = mean(ml_cum_err),
            .groups = "drop") |>
  mutate(across(where(is.double), \(x) round(x, 1)))
print(summ, n = Inf)

# 2) Fairer year-one view: October vintages only (old model had area reports by then)
cmp |> filter(h == 1, format(vintage, "%m") == "10") |>
  select(vintage, prop_type, old_g, act_g, ml_g) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |> print(n = Inf)
