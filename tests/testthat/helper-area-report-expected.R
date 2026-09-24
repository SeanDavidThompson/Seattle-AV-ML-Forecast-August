# Expected area-report values, read by hand from the KCA reports
# (population basis; percentages as printed).  `dp` is the number of decimals
# printed, which sets the tolerance (half a unit in the last place).
#   kind: res | com_geo | spec | spec_region | condo
#   key : area (res, com_geo), spec_area (spec), spec_region (spec_region),
#         report_id suffix (condo)
AR_EXPECTED <- tibble::tribble(
  ~year, ~kind,         ~key,     ~spec,  ~total,  ~land,  ~imps,  ~dp,
  # residential, single-area layout
  2019L, "res",         "48",     NA,     -1.1,     0.0,   -1.9,   1L,
  2020L, "res",         "21",     NA,     -2.1,    36.9,  -27.8,   1L,
  2021L, "res",         "82",     NA,     17.8,    40.4,   -0.5,   1L,
  # residential, multi-area layout
  2021L, "res",         "1",      NA,     15.4,      NA,     NA,   1L,
  2021L, "res",         "3",      NA,     14.7,      NA,     NA,   1L,
  2024L, "res",         "22",     NA,      7.4,      NA,     NA,   1L,
  2025L, "res",         "43",     NA,      7.4,      NA,     NA,   1L,
  # commercial geographic
  2019L, "com_geo",     "36",     NA,     11.60,  10.88,  13.27,   2L,
  2020L, "com_geo",     "25",     NA,      8.67,  13.70,   1.39,   2L,
  2022L, "com_geo",     "20",     NA,      9.29,     NA,     NA,   2L,
  # 2025 North: one "Change in Total Assessed Value" section per area
  2025L, "com_geo",     "10",     NA,      1.00,     NA,     NA,   2L,
  2025L, "com_geo",     "14",     NA,     13.63,     NA,     NA,   2L,
  2025L, "com_geo",     "17",     NA,     -0.82,     NA,     NA,   2L,
  2025L, "com_geo",     "19",     NA,      1.42,     NA,     NA,   2L,
  2025L, "com_geo",     "80",     NA,      1.47,     NA,     NA,   2L,
  2025L, "com_geo",     "85",     NA,     -0.85,     NA,     NA,   2L,
  2025L, "com_geo",     "90",     NA,      3.45,     NA,     NA,   2L,
  2025L, "com_geo",     "95",     NA,      1.09,     NA,     NA,   2L,
  # specialty headlines
  2019L, "spec",        "280",    280L,   10.25,     NA,     NA,   2L,
  2020L, "spec",        "280",    280L,    6.73,     NA,     NA,   2L,
  2021L, "spec",        "280",    280L,   -1.62,     NA,     NA,   2L,
  2022L, "spec",        "280",    280L,    3.06,     NA,     NA,   2L,
  2023L, "spec",        "280",    280L,   -4.20,     NA,     NA,   2L,
  2024L, "spec",        "280",    280L,  -32.08,     NA,     NA,   2L,
  2025L, "spec",        "280",    280L,   -5.75,     NA,     NA,   2L,
  2020L, "spec",        "250",    250L,    5.00,  13.15,  -8.84,   2L,
  2022L, "spec",        "160",    160L,   12.44,   4.98,  16.17,   2L,
  2024L, "spec",        "153",    153L,    5.45,     NA,     NA,   2L,
  2024L, "spec",        "174",    174L,   -1.60,     NA,     NA,   2L,
  2026L, "spec",        "500",    500L,   -3.50,     NA,     NA,   2L,
  2026L, "spec",        "510",    510L,   -5.4,      NA,     NA,   1L,
  # apartments (spec 100): regions 1 Central/North, 2 South, 3 East; County = headline
  2021L, "spec_region", "1",      100L,   -5.52,     NA,     NA,   2L,
  2021L, "spec_region", "2",      100L,    7.89,     NA,     NA,   2L,
  2021L, "spec_region", "3",      100L,    0.72,     NA,     NA,   2L,
  2021L, "spec",        "100",    100L,   -1.43,     NA,     NA,   2L,
  2024L, "spec_region", "1",      100L,   -9.27,     NA,     NA,   2L,
  2024L, "spec_region", "2",      100L,   -1.66,     NA,     NA,   2L,
  2024L, "spec_region", "3",      100L,  -10.33,     NA,     NA,   2L,
  2024L, "spec",        "100",    100L,   -7.97,     NA,     NA,   2L,
  # condo
  2019L, "condo",       "700_01", 700L,    3.9,    15.1,    0.5,   1L,
  2020L, "condo",       "700_01", 700L,  -10.4,    13.6,  -18.8,   1L
)

# Spec 280 has 20 submarkets in each of these years
AR_EXPECTED_280_SUBMARKETS <- tibble::tibble(year = 2019:2025, n = 20L)

# The one population-basis row an expectation refers to
ar_expected_row <- function(act, e) {
  pop <- act[act$basis == "population", ]
  switch(e$kind,
    res         = pop[pop$prop_type == "res" & pop$report_kind == "geo" &
                        pop$area %in% as.integer(e$key), ],
    com_geo     = pop[pop$prop_type == "com" & pop$report_kind == "geo" &
                        pop$area %in% as.integer(e$key), ],
    spec        = pop[pop$report_kind == "specialty" & pop$spec_area %in% e$spec, ],
    spec_region = pop[pop$report_kind == "specialty_region" & pop$spec_area %in% e$spec &
                        pop$spec_region %in% as.integer(e$key), ],
    condo       = pop[pop$prop_type == "condo" & grepl(paste0(e$key, "$"), pop$report_id), ])
}
