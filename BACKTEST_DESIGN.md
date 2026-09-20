# Backtest harness — design

Branch `feature/backtest-harness`. Reviewed 2026-09-20; §11 records the
decisions. Line numbers are against `main` at `067a84a`.

Goal: forecast error by horizon for origins 2022–2025, so the pipeline's track
record can sit next to King County OEFA's published forecasts.

```
origin 2022 -> forecast TY2023-2026   (h = 1..4)
origin 2023 -> forecast TY2024-2026   (h = 1..3)
origin 2024 -> forecast TY2025-2026   (h = 1..2)
origin 2025 -> forecast TY2026        (h = 1)
```

Origin T means: every model is trained on rows with `tax_yr <= T`, the
observed history handed to the extend/forecast steps ends at T, the forecast
runs T+1 … 2026, and predictions are scored against observed AV from
`av_history_cln` (never against a filled or retrofitted value).

---

## 0. What the pipeline actually does (the parts that matter here)

Reading `main_ml.R`, `03_model_land.R`, `03_model_impr.R`,
`04_retrofitting_values.R`, `05_extend_panel_2026_2031.R`, and the comm/condo
extend and forecast scripts, these are the mechanics the harness has to work
with. Several of them constrain the design more than the decisions list does.

**Step order is 3 → 4 → 5b → 6 (train, retrofit, extend, forecast).**
Models are trained *before* the retro panels are built. That has two
consequences:

- Residential (`03_model_land.R:11-17`, `03_model_impr.R:11-17`) trains on the
  **raw** `panel_tbl_res.rds` — observed AV only, `drop_na(log_appr_land_val)`.
  The `04_retrofitting_values.R` imputations are **not** residential training
  targets. They enter the forecast only as the year-T seed
  (`appr_land_val_filled` at `hist_max_yr`, `06_..._sequential.R:297-316`) and
  the lag2 at T+1, for parcels that have no observed value at T. This changes
  the D4 estimate — see §D4.
- Commercial (`build_subgroup_model_data`, `main_ml.R:838`) trains on
  `panel_tbl_retro_<key>` **if it is already cached from a prior run**
  (`main_ml.R:1694-1720`), otherwise on the raw subgroup panel with its own
  defensive re-join and no locf fill. So production's steady state is
  "train on last run's locf-filled retro panel". A backtest that mirrors
  that has to put a truncated retro panel on disk **before** Step 3.

**Every "extend" step derives its base year from the data it is handed.**
`05_extend_panel_2026_2031.R:46` (`hist_max_yr <- max(panel_hist$tax_yr)`),
`05_extend_..._comm.R:36` and `_condo.R:27` (`base_yr <- max(tax_yr)`), and the
residential forecast (`06_..._sequential.R:297`,
`hist_max_yr <- max(tax_yr[!is.na(total_assessed_filled)])`) all freeze parcel
state at the last row present. The comm/condo forecasts instead take
`last_obs_yr <- forecast_start - 1` and seed the lag tracker from
`panel_ext[tax_yr == last_obs_yr]` (`06_..._comm.R:209-277`). So: truncating
the retro panels to `tax_yr <= T` **and** setting `forecast_start = T + 1`
together are what make an origin-T run coherent; either one alone breaks the
seeding.

**Forecast-year drivers come from scenario caches, not the panel.**
`05_extend_*.R` join `econ_fcst_2026_2031_<scenario>.rds` (rows `tax_yr > 2025`
only — `xx_econ_to_panel.R:151`), `nwmls_fcst_2026_2031_*.rds`,
`nwmls_condo_fcst_*.rds` and `costar_fcst_*.rds` by `tax_yr`; the per-subgroup
`cs_*` block is joined in Step 6 from `costar_<type>_fcst_*.rds`. For a backtest
horizon T+1…2026 the econ/NWMLS caches have no rows for 2023–2025, so the
extend step would leave those years NA. The realized values for those years
already exist as citywide constants in the production panel rows, and that is
where the harness takes them from (§D1). CoStar caches carry history and need
nothing.

**AV on the commercial and condo side is populated only by the defensive
re-joins.** The 2026-09-19 run fired "log_appr_land_val all-NA — re-joining AV"
in Step 4b/4c for all six subgroups, com_other and condo. There are four copies
of that repair: `main_ml.R:1932-1965` (subgroups), `:2010-2047` (com_other),
`:2081-2110` (condo), `:855-895` (Step 3, `build_subgroup_model_data`), plus
`03_model_condo_*.R:59-94` and `06_..._condo.R:47-89`. Leak analysis in §7.

**Step 4b/4c fill is `na.locf(na.locf(x), fromLast = TRUE)`** at
`main_ml.R:1970-1976`, `:2047-2053`, `:2114-2120`. The `fromLast` pass copies
later years into earlier NA rows. In production that means 2023–2026 values
land in pre-2023 rows for any parcel whose history starts late or has a gap.
Those filled rows are then commercial/condo training targets. §D3.

**Cache and path coupling.** `run_main_ml()` reads its inputs from `cache_dir`
and writes every intermediate (retro panels, extend panels, forecast panels,
training frames, stamped models) back to the same `cache_dir` / `model_dir` /
`output_dir`. Two scripts ignore those parameters:
`03_model_land.R:270-291` writes stamped models to `here("data","model")` and
training frames to `here("data","cache")`; both `03_model_land.R:11` and
`03_model_impr.R:11` read the panel from `here("data","cache")`. A backtest
that ran with a separate `model_dir` would still drop truncated land models
into `data/model/`, and `latest_model_file()` (`main_ml.R:569`, picks newest
mtime) would then serve them to the next production run. **Fixed on `main`
at `7b4be49`, independent of this branch** (paths now resolve from
`cache_dir`/`model_dir` with the old literals as standalone defaults).

**`main_ml.R` runs the pipeline when sourced.** Line 2712 is a bare
`run_main_ml(scenario = "baseline", prop_scope = "com", forecast_only = TRUE,
model_replicate = TRUE)`. A harness cannot `source()` the file to get the
function definitions without triggering that call. `xx_area_report_backtest.R`
already solves this with a `BT_DEFINE_ONLY` global; the same pattern goes on
`main_ml.R` (§9, change 1).

**Anchor semantics.** `area_report_actuals` rows carry
`assessment_yr = area_reports_year`; every consumer anchors
`tax_yr == assessment_yr + 1` (`06_..._sequential.R:361-365`,
`06_..._comm.R:393-395`, `xx_com_other_growth.R:235`). So year-T reports anchor
TY(T+1), the first forecast year of origin T — same relationship as production
(2026 reports → TY2027). With `forecast_start = T+1`, the default
`area_reports_year = forecast_start - 1` (`main_ml.R:321`) already equals T;
the harness passes it explicitly anyway and adds the loud failure (§D5).
Condo has no anchor path at all (`06_..._condo.R` never reads
`area_report_actuals`), so anchored and unanchored condo results will be
identical.

**Two things I found that are not on this branch's list but affect what the
backtest measures:**

1. `06_forecast_av_2026_2031_sequential_condo.R` is the same file pasted twice
   (copy 2 starts at line 362 — `diff <(sed -n 1,361p) <(sed -n 362,644p)`).
   Copy 2 lacks the land-only gate (`:143-194`, `:267-274`, `:291-296`) and
   runs last, so `panel_tbl_forecasted_condo` and the condo cache are the
   **ungated** output — the exact shape of the B1 defect that was fixed for the
   residential file (`BUGS_2026-09.md`). **Fixed on `fix/condo-forecast-dedupe`
   (PR #7)**: copy 2 removed, copy 1 kept verbatim. That PR merges before the
   backtest runs, so backtest and production measure the same condo code.
2. The post-Step-5a drop block (`main_ml.R:2199-2210`) names
   `model_data_comm_land_delta` / `model_data_comm_impr_delta`, which no
   longer exist. The six subgroups' 24 frames (`model_data_<key>_<tt>`) are
   reloaded "for diagnostics / reference" at `:1769-1777` right after training
   and are never dropped. That is most of the ~9 GB the OOM report mentions.
   The relocation you asked for (§9, change 7) includes them.

---

## 1. D1 — Conditional, not ex-ante

Implemented as: parcel models and the observed history are truncated at T;
macro drivers for T+1…2026 are the realized values at today's vintage.

Plain-language statement, written to `data/outputs/backtest/README_backtest.txt`
and printed at the top of every harness run and every per-origin log:

> **These are conditional backtest errors, not an ex-ante forecast record.**
> For each origin year T the parcel models were trained only on assessment
> data through tax year T, and the forecast for T+1 onward was seeded from
> values observed through T. But every macro driver — NWMLS housing series,
> OERF/econ series, CoStar market series, construction-sales series — was fed
> in at its current (2026) vintage for every forecast year, including
> revisions. The result measures how well the parcel models turn a *known*
> macro path into assessed values. It does not measure how well the pipeline
> would have forecast in year T, when those paths were themselves forecasts.
> Real-time accuracy will be worse than these numbers. Published OEFA
> forecasts placed alongside them were genuinely ex-ante.

Mechanics:

- The harness runs the production extend scripts unchanged (so parcel state is
  frozen at T exactly as production freezes it at 2026, including the
  `years_since_newconst` increment and frozen permit windows), then overwrites,
  in every extended-panel cache for rows `T < tax_yr <= 2026`, each
  **citywide** driver column with the realized value for that `tax_yr`.
- "Citywide" is determined empirically per track from the production panel
  rows: numeric columns whose value is constant within every `tax_yr`
  (`uniqueN == 1`, ignoring NA) and not all-NA, restricted to the allowlist
  `^(econ_|sea_|k_|costar_|cs_|con_sales_)` (including `.x`/`.y` twins, which
  `repair_suffixed()` then coalesces as usual). Any other constant-within-year
  column is printed as "left frozen" and not touched. The full swapped column
  list is written per origin so it can be checked.
- `con_sales_*` is included even though production freezes it at the last row
  (`05_extend_*.R` never join the `con_sales_fcst` cache). Realizing it is
  consistent with "given the macro path". Say so if you want it frozen instead.
- Per-subgroup `cs_*` columns are joined in Step 6 from the CoStar caches by
  `tax_yr`, which already carry realized quarters through 2026; nothing to do.

What is **not** realized (frozen at T, as production freezes it at 2026):
parcel-level features — permits, `kcap_*`, `hi_*`, `hlth_*`, building
characteristics. Realizing those would credit the model with knowing about
remodels and new construction it could not have known; that is the wrong
side of the line.

One more vintage caveat that goes in the README: static parcel attributes are
the 2026-07-28 extract's values for every historical year, in production
training as well as here. Not a backtest-specific issue and not changed.

---

## 2. D2 — `train_through_year`

New `CFG$train_through_year = NULL`, a `run_main_ml()` argument of the same
name, assigned into `.GlobalEnv` alongside `forecast_start` so sourced scripts
can read it with `get0()`. `NULL` = current behaviour at every site.

Injection points — one per training-frame builder:

| Track | File / site | Where the filter goes |
|---|---|---|
| res land | `03_model_land.R:57-66` | `panel_tbl_train <- panel_tbl_train %>% filter(tax_yr <= train_through_year)` right after the `train_res` filter, before lags are computed |
| res impr | `03_model_impr.R:104-113` | same |
| six com subgroups | `main_ml.R:845` (`build_subgroup_model_data`) | `dt <- dt[tax_yr <= train_through_year]` immediately after the `copy()`, i.e. **before** the defensive AV re-join, the dlog/lag shift, and the predictor scans |
| condo land | `03_model_condo_land.R:252-258` (`model_base`) | add `tax_yr <= .tty` to the existing `tax_yr >= 2005` filter |
| condo impr | `03_model_condo_impr.R:242-248` | same |

Notes:

- Placing the commercial filter at the top of `build_subgroup_model_data()`
  means the near-zero-variance scan, `usable_cols`, and the econ-level twin
  check all run on the truncated frame. That incidentally removes most of the
  D6 leak for the commercial side. The one scan that still sees post-T data
  is the `FCST_DEAD_CS` forecast-coverage probe (`main_ml.R:1010-1043`), which
  reads the extend-panel cache from disk; it is advisory
  (`auto_drop_dead_cs = FALSE`) and changes nothing. Noted in §D6, not changed.
- `make_rolling_year_folds()` and the residential median imputation operate on
  the truncated frame automatically.
- `train_subgroup_lgbm()`'s CV numbers are in-sample (`main_ml.R:1221-1224`,
  the comment says so). The harness never reads them; the only accuracy
  numbers it reports come from scoring Step 6 output against observed AV.
- Validation added to `run_main_ml()`: if `train_through_year` is set,
  `forecast_start` must equal `train_through_year + 1` (`stop()` otherwise).
  Any other combination seeds the comm/condo lag tracker from an empty year.

Why `train_through_year` alone is not enough: the retro panels the extend step
reads must also end at T. The harness owns that (§8.2, step 4) rather than
`run_main_ml()`, because Step 3 runs before Step 4 and the truncated retro
panels must already be on disk when training starts.

Byte-identical check: see §10.

---

## 3. D3 — Backward fill off

New `CFG$locf_backfill = TRUE`, a `run_main_ml()` argument, and an argument of
the new top-level helper `retro_fill_av()` (§9, change 2) that Step 4b/4c call.

- `TRUE` (default): `na.locf(na.locf(x), fromLast = TRUE)` exactly as now.
- `FALSE`: forward pass only.
- In both modes the helper computes and prints, per track and per column,
  `n_forward_filled` and `n_backfilled` (rows that were NA after the forward
  pass and would be non-NA after the backward pass). The harness collects
  these into `locf_backfill_counts.csv` (origin, track, column, n_rows,
  n_forward_filled, n_backfilled, share_of_rows). That is the magnitude you
  asked for; it cannot be computed in this checkout (the `data/` tree here is
  empty), so it is a harness output rather than a number in this document.

Backtest runs use `locf_backfill = FALSE`, and additionally the harness
truncates the raw panel to `tax_yr <= T` *before* the fill, so even the
forward pass only ever sees ≤T values.

---

## 4. D4 — Retrofit held fixed

Held fixed by construction: the harness takes the production
`panel_tbl_retro_res.rds` (fit through 2026), keeps rows `tax_yr <= T`, and
stages it as the residential retro panel for every origin. Step 4a is not
re-run. Commercial and condo have no model-based retrofit — their "retrofit"
is the locf fill above, rebuilt per origin from ≤T data.

Caveat text (README, run header, and the header rows of every metrics CSV):

> Missing historical residential values were imputed once, by the production
> retrofit models fit on data through 2026, and those imputed values are
> reused for every origin. The imputed panel is the base the extend and
> forecast steps work from, so imputation shapes the forecast path wherever a
> parcel's history has gaps, even though the parcel models themselves were
> trained on observed values only. A strict backtest would re-fit the
> retrofit through T. This is a known source of optimism. The tables split
> out the cohort of parcels with an observed value at the origin year, where
> imputation does not touch the seed.

**Where it enters, and what is isolated.** The residential models train on
the raw panel, so the ~670k/~651k imputed parcel-years are not training
targets (§0). But `04_retrofitting_values.R` writes `panel_tbl_retro_res`,
that is what Step 5b extends, and the extend output is what Step 6 forecasts
from. So imputation reaches the forecast through the base panel: the year-T
seed (`appr_land_val_filled` at `hist_max_yr`) and the lag2 at T+1 for any
parcel whose year-T or T−1 value is imputed, plus the frozen parcel state
where the retro step aligned columns. The `seed_observed` split isolates the
cohort where the *seed* is observed; it does not make the base panel
imputation-free, and the caveat is framed that way.

Who has an imputed seed? Mostly parcels created after T (the panel is a
2006–2026 spine over the 2026 extract's parcel universe, so a 2024
subdivision has imputed rows for 2006–2023) plus parcels with a filtered
year at T (exemption, special valuation). For Seattle residential that is on
the order of one to a few thousand parcels a year against ~170k, so I expect
the imputed-seed share of scored parcel-years to be low single-digit percent
at h=1, rising with horizon, and somewhat higher value-weighted because new
parcels skew to new construction. That is a guess; the harness measures it:

- every scored parcel-year carries `seed_observed` (observed AV > 0 at T in
  `av_history_cln`);
- all error tables are produced for `pop = "seed_observed"` (headline) and
  `pop = "all_scored"`;
- the citywide growth series uses the `seed_observed` cohort only, which is
  also the matched-parcel population `av_reconcile_certified.R` defines;
- `seed_coverage.csv` reports the count and AV share of imputed-seed rows by
  origin × horizon × track.

For commercial and condo the analogous seed issue is staleness, not optimism:
a parcel missing T is seeded from its last prior observed year by the forward
fill. Same `seed_observed` flag covers it.

---

## 5. D5 — Anchors from year-T reports, and anchored vs unanchored

- For every origin the harness passes `area_reports_year = T` explicitly.
- Before any compute, `run_backtest()` checks `data/kca/area_reports/<T>/`
  exists and contains at least one PDF for every origin in the anchored set,
  and stops listing the missing years. (Failing before hours of training is
  louder than failing at origin three; pass a subset of `origins` to run
  around a missing year.)
- New `run_main_ml(require_area_actuals = FALSE)` argument. When `TRUE` and
  `use_area_actuals = TRUE`, Step 0 `stop()`s — instead of the current
  `warning("… continuing without")` at `main_ml.R:483` — if
  `area_report_actuals` is absent, empty, or has any `assessment_yr !=
  area_reports_year`. The harness passes `TRUE`. Production default `FALSE`.
- `area_report_actuals_<T>.rds` is written to the origin's scratch cache, not
  `data/cache`; the CSV is copied to `data/outputs/backtest/anchors/`.
- The 2022–2025 report templates may not all parse the way the 2026 ones do.
  A year that parses to zero rows fails loudly (above). A year that parses
  partially cannot be distinguished from a year with fewer reports, so the
  harness writes `anchor_coverage.csv` (from `actuals_rate_coverage` for the
  commercial subgroups and `land_method`/`impr_method` counts for
  residential): parcels and prior-year AV by rate source, per origin × year ×
  track. Read that before reading the anchored error tables.

Both modes per origin: the harness trains and extends once per origin, then
runs Step 6 twice — `use_area_actuals = TRUE` and `FALSE` — from the same
models and extended panels. Every output table carries `anchored`, and the
by-horizon summary includes the anchored − unanchored difference per metric.
For condo the two are identical (no anchor path).

---

## 6. D6 — Feature-selection leakage (note only)

Scans that see post-T rows in production and are not changed:

- `usable_cols` / near-zero-variance (`main_ml.R:943-949`),
  `drop_single_level_and_nzv()` (`03_model_land.R:143`, `03_model_impr.R:161`)
  — under the D2 placement these now run on the truncated frame, so the leak
  is closed as a side effect for res and com. Condo's `delta_x_cols` are a
  fixed list intersected with names, no data scan.
- `FCST_DEAD_CS` coverage probe (`main_ml.R:1010-1043`) reads the extend-panel
  cache (full horizon). Advisory only; left alone.
- The hand-maintained `FCST_DEAD_CS` list and the econ-level twin rule were
  themselves chosen by looking at 2026-era forecast coverage. Second order,
  not changed.

---

## 7. The defensive re-join under truncation

Question: the Step 4b/4c repair (and its Step 3 twin) reads `av_history_cln`,
which runs 2001–2026. Does it leak post-T information into an origin-T run?

The join is `dt[av_fix, on = .(parcel_id, tax_yr), := ...]` — it writes
observed AV into rows that already exist in the panel, keyed by year. Post-T
values can only land in post-T rows. So the leak question is entirely about
what happens to post-T rows afterwards. Path by path:

| Site | What it does with post-T rows | Under the harness |
|---|---|---|
| Step 4b/4c re-join (`main_ml.R:1932`, `:2010`, `:2081`) | writes AV for all years | harness truncates the raw panel to ≤T **before** calling `retro_fill_av()`; post-T rows do not exist |
| Step 4b/4c backward locf | copies post-T values into earlier NA rows → training targets | closed twice: truncation removes the source rows; D3 flag disables the pass |
| Step 3 re-join in `build_subgroup_model_data()` (`:855-895`) | fires only if AV still all-NA | D2 filter sits before it (`dt[tax_yr <= T]`), so it can only fill ≤T rows; dlog/lag `shift()` then runs on ≤T rows only |
| `03_model_condo_*.R:59-94` | re-joins all years into the raw condo panel, then computes lags/delta | `shift(…, 1)` is backward-looking, so row T's lag/delta uses T−1 only; `model_base` is then filtered to ≤T. No ≤T row ever contains a post-T value |
| `06_..._condo.R:47-89` | fires only if AV at `last_obs_yr` is all-NA in the extended panel; fills all years | will not fire (the truncated retro panel carries observed AV at T). If it did fire it would place observed post-T AV into forecast-year rows; the delta/level paths never read a row's own `appr_*` (lags come from `prev_preds`), but the no-improvement-model ratio fallback (`:283-286`, `tax_yr < yr`) would see them. The harness asserts after each Step 6 that forecast-year rows of every track carry NA `appr_land_val`/`appr_imps_val` and warns loudly if not |

Verdict: the repair does not leak under truncation as long as the truncation
happens before it (Step 4 path) or before the shift/filter that follows it
(Step 3 and condo paths), which is where the D2 filter goes. Without the
harness-side truncation of the raw panels — i.e. if `run_main_ml()` were run
on the full cache with only `train_through_year` set — the backward locf
would leak, and the extend step would base itself on 2026. That is why the
harness, not `run_main_ml()`, owns the information set at T.

Not fixed: the panel-assembly defect that makes the repair load-bearing.

---

## 8. Architecture

### 8.1 Files

New:

- `scripts/ml/backtest_harness.R` — `run_backtest()` and helpers (`bt_*`).
- `scripts/ml/backtest_origin.R` — `Rscript` entry point for one origin
  (sources the harness, calls `bt_run_origin(T)`).
- `scripts/ml/test_backtest_retro_fill.R` — synthetic-data test that the
  `retro_fill_av()` refactor reproduces the inline Step 4b/4c code (§10,
  the verification plan).

Modified (all additive, all default to current behaviour): `main_ml.R`,
`03_model_land.R`, `03_model_impr.R`, `03_model_condo_land.R`,
`03_model_condo_impr.R`. Exact change list in §9.

### 8.2 Per-origin flow (child process)

`run_backtest()` launches one `Rscript --vanilla scripts/ml/backtest_origin.R
<T> …` per origin, sequentially, from the repo root, and waits. Each origin
therefore starts with an empty heap and finishes by exiting; nothing has to be
freed by hand between origins. (`run_in_process = TRUE` runs the same function
in the current session for debugging; it `rm()`s everything it created and
`gc()`s between origins, but the child process is the default.) Logs go to
`data/outputs/backtest/logs/origin_<T>.log`.

Inside `bt_run_origin(T)`:

1. **Define.** `MAIN_ML_DEFINE_ONLY <- TRUE; source("scripts/ml/main_ml.R")`.
   Gives `CFG`, `COM_SUBGROUP_KEYS`, `run_main_ml()`, `retro_fill_av()`.
2. **Scratch dirs.** `work = data/outputs/backtest/work/origin_<T>/{cache,model,outputs}`,
   created fresh. `cache_dir`, `model_dir`, `output_dir` for every
   `run_main_ml()` call point here. Nothing under `data/cache`, `data/model`
   or `data/outputs` outside `backtest/` is written.
3. **Stage inputs** into `work/cache` from the production cache, as NTFS hard
   links (`file.link()`, instant, no extra space) with a copy fallback:
   `panel_tbl_res.rds`, the six `panel_tbl_<key>.rds`,
   `panel_tbl_com_other.rds`, `panel_tbl_condo.rds`, `av_history_cln.rds`,
   `econ_fcst_*`, `nwmls_fcst_*`, `nwmls_condo_fcst_*`, `costar_fcst_*`,
   `costar_<type>_fcst_*` for the scenario. These are what Step 2
   (`panel_replicate = FALSE`) and the model scripts read. If the production
   cache is on a OneDrive-synced path, `stage_mode = "copy"` avoids OneDrive
   uploading link targets twice; that is a runtime option, default `"link"`.
4. **Build the origin's history.** Written to `work/cache`:
   - `panel_tbl_retro_res.rds` ← production retro panel, rows ≤T (D4).
   - `panel_tbl_retro_<key>.rds` ×6, `_com_other`, `_condo` ← raw panel rows
     ≤T → `retro_fill_av(backfill = FALSE)` → written. Backfill counts (D3)
     recorded. One track in memory at a time.
   - `realized_drivers_<track>.rds` ← per-`tax_yr` citywide values for
     T+1…2026, extracted from the raw panel rows *before* truncation (§D1).
   `av_history_cln` is read once here and released.
5. **Pass 1 — train and extend.**
   ```r
   run_main_ml(prop_scope = "all", scenario = "baseline",
               panel_replicate = FALSE, model_replicate = TRUE,
               retrofit_replicate = FALSE, extend_replicate = TRUE,
               forecast_only = FALSE, diagnostics_replicate = FALSE,
               train_through_year = T, locf_backfill = FALSE,
               forecast_start = T + 1L, forecast_end = 2026L,
               use_area_actuals = FALSE,
               cache_dir = work$cache, model_dir = work$model,
               output_dir = work$outputs,
               stop_after = "extend")
   ```
   Step 3 trains res/com/condo on ≤T frames; Step 4 finds every retro cache
   present and loads it; Step 5b extends to 2026; the call returns before
   Step 6 (`stop_after`, §9 change 4).
6. **Realize the drivers.** For every `panel_tbl_*_inputs_baseline_*.rds` in
   `work/cache` (horizon-named and legacy-named copies alike, since
   `06_..._condo.R` reads the legacy name), overwrite the allowlisted citywide
   columns for rows T < `tax_yr` ≤ 2026 from `realized_drivers_<track>.rds`,
   blank any `appr_*`/`log_appr_*`/`total_assessed*`/`pred_*` in those rows
   (defensive; the extend already does), write back.
7. **Pass 2 — forecast**, once per anchor mode:
   ```r
   run_main_ml(…same dirs/horizon…, forecast_only = TRUE,
               model_replicate = FALSE,
               use_area_actuals = anchored, area_reports_year = T,
               require_area_actuals = anchored)
   ```
   Step 0 imports the year-T reports (anchored) or removes any actuals object
   (unanchored); Step 3 reloads this origin's models from `work/model`;
   Step 5b loads the realized extended panels; Step 6 forecasts T+1…2026.
   After each pass: extract predictions (§8.3), capture
   `actuals_rate_coverage` and residential method counts, run the
   post-T-AV assertion from §7, drop the forecast panels, `gc()`.
8. **Write** `predictions/pred_origin<T>_<anchored|unanchored>.rds`,
   `origin_<T>_meta.rds` (timings, swapped columns, backfill counts, anchor
   coverage), copy the actuals CSV to `anchors/`. Delete `work/origin_<T>`
   unless `keep_work = TRUE`.

Disk: an origin's scratch holds truncated retro panels, extend panels and two
sets of forecast panels — roughly 1.5–2× the production cache footprint,
transiently. Time: one full `model_replicate = TRUE` production run per
origin plus two Step 6 passes; four origins run back to back.

### 8.3 What is extracted per parcel-year

From `panel_tbl_forecasted_res` (rows `tax_yr > T`): `appr_land_val_filled`,
`appr_imps_val_filled`, `total_assessed_filled`, `land_method`, `impr_method`.
From `panel_tbl_forecasted_com` (all subgroups + com_other, `com_subgroup`
column): `pred_appr_land_val`, `pred_appr_imps_val`, `pred_total_assessed`.
From `panel_tbl_forecasted_condo`: same `pred_*` columns.

Stored as `parcel_id` (dash-stripped), `track` (res, condo, apt, office,
industrial, retail, hospitality, medical, com_other), `origin`, `tax_yr`,
`horizon = tax_yr − origin`, `anchored`, `pred_land`, `pred_imps`,
`pred_total`, `method` (res only; com per-parcel rate source is not retained
by the forecast script and adding a column would change production output —
aggregate coverage comes from `actuals_rate_coverage` instead).

### 8.4 Scoring (parent process)

Actuals: `av_history_cln.rds` from the production cache, `appr_land_val`,
`appr_imps_val`, `total = land + imps` per `parcel_id × tax_yr`. Never a
filled column.

Join on `(parcel_id, tax_yr)`. A parcel-year is scored for a component when
observed > 0 and predicted > 0. `seed_observed` = observed total > 0 at
`tax_yr = origin`.

Error tables — cell = origin × horizon × track × anchored × component
(total, land, imps) × pop (seed_observed, all_scored):

| column | definition |
|---|---|
| `n`, `obs_sum` | rows scored, Σ observed |
| `RMSE_log` | sqrt(mean((log pred − log obs)²)) |
| `MAE_log` | mean(\|log pred − log obs\|) |
| `ME_log` | mean(log pred − log obs) — signed, per-parcel |
| `WAPE` | Σ\|pred − obs\| / Σ obs |
| `bias_pct` | Σ pred / Σ obs − 1 — signed, value-weighted |

Track roll-ups: `com` = six subgroups + com_other; `all` = res + com + condo.

Files:

- `errors_by_cell.csv` — every cell above.
- `errors_by_horizon.csv` — parcel-years pooled across origins, by horizon ×
  track × anchored × component × pop; plus `n_origins`. **Primary table.**
  Includes anchored − unanchored deltas.
- `errors_by_origin.csv` — origin × track × anchored, pooled over horizons.

Growth — the number that goes next to OEFA. Matched-parcel year-over-year
growth, the same definition `av_reconcile_certified.R:135-180` and
`xx_area_report_backtest.R` use, on the `seed_observed` cohort:

- for year y in T+1…2026, the matched set M_y is parcels in the cohort with
  observed > 0 in y−1 and y and a prediction in y (and in y−1 when y−1 > T);
- `g_pred_y = Σ_M pred_y / Σ_M base_{y−1} − 1`, where `base_T` is observed
  and `base_{y−1}` for y−1 > T is the prediction;
- `g_act_y = Σ_M obs_y / Σ_M obs_{y−1} − 1`;
- also cumulative from origin: `Σ pred_y / Σ obs_T − 1` vs `Σ obs_y / Σ obs_T − 1`
  over cohort parcels observed in both T and y.

Files: `growth_by_year.csv` (origin × tax_yr × horizon × track incl. `all` ×
anchored: `g_pred`, `g_act`, `err_pp`, `cum_pred`, `cum_act`, `cum_err_pp`,
`n_matched`, `av_base`) and `growth_by_horizon.csv` (mean and mean-absolute
`err_pp` across origins, by horizon × track × anchored, with `n_origins`).

Stated next to the growth tables: this is Seattle, existing-parcel,
matched-parcel growth. It excludes new construction and parcels retired
between T and 2026 (the panel is the 2026 extract's parcel universe, so
retired parcels are absent — a survivorship caveat). OEFA's number is King
County total roll growth. Growth rates rather than levels are the comparable
quantity, as you said; the population difference still has to travel with
the table.

Other outputs: `anchor_coverage.csv`, `locf_backfill_counts.csv`,
`seed_coverage.csv`, `README_backtest.txt` (D1/D4 text, run configuration,
origins, timestamps, git SHA), `logs/`.

`run_backtest(score_only = TRUE)` re-scores existing prediction files without
re-running anything.

### 8.5 Signature

```r
run_backtest(origins        = 2022:2025,
             anchored       = c(TRUE, FALSE),
             final_year     = 2026L,
             scenario       = "baseline",
             out_dir        = here::here("data", "outputs", "backtest"),
             prod_cache_dir = CFG$cache_dir,
             stage_mode     = c("link", "copy"),
             run_in_process = FALSE,
             keep_work      = FALSE,
             score_only     = FALSE,
             smoke_stop     = TRUE,      # stop after the first origin if the
             smoke_min_coverage = 0.5,   #   h=1 bracket check fails (§11)
             smoke_max_err_pp   = 10)
```

---

## 9. Change list

Every change is behind a new argument whose default reproduces current
behaviour, or replaces a path expression with one that evaluates to the same
path in production.

**`scripts/ml/main_ml.R`**

1. Wrap the top-level call at `:2712` in
   `if (!isTRUE(get0("MAIN_ML_DEFINE_ONLY", envir = .GlobalEnv, ifnotfound = FALSE)))`.
   Sourcing the file interactively still runs it.
2. Hoist the Step 4b/4c AV re-join + locf into a top-level function
   `retro_fill_av(dt, cache_dir, backfill = TRUE, label = "")` defined next
   to `COM_SUBGROUPS` (outside `run_main_ml()`, so the harness can call it).
   Step 4b (subgroups and com_other) and 4c call it. Behaviour-preserving:
   same dash handling, same log/total recomputation, same locf. Two details
   to be explicit about: (a) the three inline copies differ in whether they
   re-join when the column is *absent* (com_other does, the others only when
   present-and-all-NA); the helper re-joins in both cases. The absent case
   cannot occur for the subgroup/condo panels (the panel builder always
   creates the column), so production output is unchanged, but it is a
   unification, not a pure move. (b) The helper prints the D3 counts in both
   modes; that is new message output only.
3. `train_through_year = CFG$train_through_year` argument, `.GlobalEnv`
   assignment, `forecast_start == train_through_year + 1` validation, filter
   at `:845`.
4. `stop_after = NULL` argument (`NULL | "extend"`); after Step 5b's `gc()`
   and before Step 6, `if (identical(stop_after, "extend")) return(invisible(NULL))`.
5. `locf_backfill = CFG$locf_backfill` argument, passed to `retro_fill_av()`.
6. `require_area_actuals = FALSE` argument; Step 0 check as in §D5.
7. Relocate the training-frame drops (§0 finding 2): a `drop_if_exists()` of
   the residential, condo **and** subgroup frames (`model_data_<key>_<tt>` for
   every key and target) goes immediately before Step 4 (`:1871`); the
   three residential frames that `04_retrofitting_values.R` reloads into
   `.GlobalEnv` are dropped again right after Step 4a; the existing post-5a
   block (`:2199-2210`) stays as a no-op backstop. Every later consumer
   (`04_retrofitting_values.R:12-16, 197-207`; `05_eval_holdout_2025.R:177-223`;
   `06_..._sequential.R:232-235`) reloads from `cache_dir` when the object is
   absent, so output is unchanged; only peak memory moves.

**`scripts/ml/03_model_land.R`, `03_model_impr.R`**

8. Panel read: use `panel_tbl_res` from `.GlobalEnv` if present, else
   `file.path(cache_dir, "panel_tbl_res.rds")` (same fallback chain). In
   production Step 2 has just loaded that exact file into `panel_tbl_res`, so
   the object is identical and the second multi-GB read is avoided; under
   `forecast_only = TRUE` the disk path is unchanged. This is what lets the
   harness hand the model scripts a panel without writing anything to the
   production cache. (The write-path redirect that was part of this change
   landed on `main` at `7b4be49`.) D2 filter as in §D2.

**`scripts/ml/03_model_condo_land.R`, `03_model_condo_impr.R`**

9. D2 filter on `model_base`.

**Not changed:** `04_retrofitting_values.R`, all `05_extend_*`, all
`06_forecast_*`, panel assembly, `xx_area_report_backtest.R`.

---

## 10. Verifying that `train_through_year = NULL` is byte-identical

Code-level: each change in §9 is gated on a default-off argument or is a path
substitution that evaluates to the same string in production. I will list the
diff hunks against that claim in the PR.

Empirical, on the data machine (this checkout has no `data/`):

- `scripts/ml/test_backtest_retro_fill.R` — builds a small synthetic panel +
  `av_history_cln`, runs the pre-refactor inline Step 4b logic (copied
  verbatim into the test) and `retro_fill_av(backfill = TRUE)`, asserts
  `identical()`. Also checks `backfill = FALSE` equals a forward-only
  `na.locf`, and that the reported counts match a direct computation. Runs
  here without data.
- A `bt_compare_caches(a, b)` helper: given two cache directories, loads each
  named artefact and reports `identical()`. Intended use: copy
  `panel_tbl_retro_*.rds`, `model_data_*.rds` and
  `panel_tbl_*_inputs_*.rds` from a production run on `main`, run the same
  configuration on this branch, compare. Training frames, retro panels and
  extend panels are deterministic and are the strongest check; the stamped
  LightGBM models depend on thread count and are compared on
  `x_cols`/`best_iter` rather than bytes.

---

## 11. Decisions from review (2026-09-20)

1. **`con_sales_*`**: realized with the other citywide series. TRS
   construction sales is a realized, citywide, `tax_yr`-keyed series;
   treating it differently would be an inconsistency.
2. **Growth cohort**: matches `av_reconcile_certified.R` exactly.
3. **Condo duplicated forecast file**: fixed first, on
   `fix/condo-forecast-dedupe` (PR #7), to merge before the backtest runs.
4. **Prefer-GlobalEnv panel read**: yes — it keeps backtest artefacts out
   of `data/cache/`.
5. **Drop-block scope**: includes the subgroup frames.
6. **Child process per origin**: yes. Fresh heap per origin is the reason
   the OOM will not recur.

Two additions from review:

- **The D1/D4 header travels inside the metrics CSVs.** Every CSV the
  harness writes starts with `#`-prefixed comment lines carrying the
  conditional-backtest and retrofit caveats, the origins, the git SHA and
  the run timestamp, before the column header. `readr::read_csv(comment =
  "#")` / `data.table::fread()` skip them; a person opening the file sees
  them first.
- **Origin 2025 runs first, alone, as a smoke test.** `run_backtest()`
  processes origins in descending order and, after the first origin
  completes, scores it and prints a bracket check for h=1: the anchored and
  unanchored citywide growth against the observed TY2026 growth on the
  matched cohort, scored-parcel coverage, and per-track bias. If coverage is
  below `smoke_min_coverage` (default 0.5) or either mode's growth error
  exceeds `smoke_max_err_pp` (default 10 pp), the loop stops before the next
  origin starts (`smoke_stop = TRUE`). `run_backtest(origins = 2025)` runs
  just that.

Stated assumptions, unchanged: `scenario = "baseline"`; `prop_scope =
"all"`; `final_year = 2026` (last observed tax year in `av_history_cln`);
production defaults for every other `CFG` entry (`geo_actuals_scope`,
`specialty_actuals_policy`, `com_other_*`, `vacancy_monotone`, no revalue
shock weights); staging by hard link.
