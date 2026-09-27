CONDITIONAL BACKTEST - NOT AN EX-ANTE FORECAST RECORD.
For each origin year T the parcel models were trained only on assessment
data through tax year T, and the forecast for T+1 onward was seeded from
values observed through T.  But every macro driver - NWMLS housing series,
OERF/econ series, CoStar market series, TRS construction-sales series - was
fed in at its current (2026) vintage for every forecast year, including
revisions.  The result measures how well the parcel models turn a KNOWN
macro path into assessed values.  It does not measure how well the pipeline
would have forecast in year T, when those paths were themselves forecasts.
Real-time accuracy will be worse than these numbers.  Published OEFA
forecasts placed alongside them were genuinely ex-ante.

RETROFIT HELD FIXED.  Missing historical residential values were imputed
once, by the production retrofit models fit on data through 2026, and those
imputed values are reused for every origin.  The imputed panel is the base
the extend and forecast steps work from, so imputation shapes the forecast
path wherever a parcel's history has gaps, even though the parcel models
were trained on observed values only.  A strict backtest would re-fit the
retrofit through T.  This is a known source of optimism.  Tables with
pop = seed_observed are restricted to parcels with an observed value at the
origin year, where imputation does not touch the seed.

KCA area-report anchors, where used (anchored = TRUE), are the year-T
reports.  Commercial/condo forward-fill of missing history uses <= T values
only (no backward fill).  Scoring is against observed AV in av_history_cln,
never a filled value.  Growth is Seattle, matched-parcel, existing-parcel
growth: it excludes new construction and parcels retired before 2026.
OEFA's number is King County total roll growth.

origins = 2019, 2020, 2021, 2022, 2023, 2024 | final_year = 2026 | scenario = baseline | git = unknown | generated = 2026-09-26 23:55:28
