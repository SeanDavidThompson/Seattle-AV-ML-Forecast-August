# Fix: geographic area rates were being applied to specialty parcels

## The defect

`06_forecast_av_2026_2031_sequential_comm.R` joined `area_report_actuals` on
`area` alone and applied the resulting growth rate to **every** parcel in that
geo area, land and improvements alike.

KCA's geographic district reports are computed on the **non-specialty**
population. Reconciling parcel-level AV against the published Central District
tables, Area 30's 2026 total of $8.853B matches parcels with `spec_area == 0`
to within 1.25% ($8.743B). Specialty parcels — Major Office 280, Major Retail
250, Warehouses 500, Hotels 160, Industrial 540, Biotech 800 — are valued by a
separate specialty appraiser and reported separately.

So Area 30's −31.04% was applied to every downtown major office parcel, even
though that figure was computed with those parcels removed. Because the office,
retail, industrial, hospitality and medical subgroups are all built from
`spec_area` crosswalk matches, the same defect affected all of them.

The failure mode was silence: a missing specialty rate did not error, it
inherited the geographic one.

## Changes

### `area_report_import.R`
- New `parse_spec_report()` reads specialty reports (Specialty 280 style),
  capturing both the specialty-wide total from *CHANGE IN TOTAL ASSESSED VALUE*
  and every per-submarket row from the *Specialty Area Breakdown* table.
  Verified against the 2025 Area 280 report: total −5.75%, all 20 submarkets.
- `.num()` now handles parenthesised negatives (`$ (1,889,223,800)`).
- Output schema gains `report_kind` (`geo` / `specialty` /
  `specialty_submarket`), `spec_area`, `spec_sub`.
- Dispatch tests for specialty templates before the geographic parser, since
  both say "Commercial Revalue".
- Logs which specialty areas got rates, and warns explicitly when none did.

### `06_forecast_av_2026_2031_sequential_comm.R`
- Joins `SpecArea` from `EXTR_Parcel.csv` onto the panel (mirrors the existing
  `Area` join, dash-format aware).
- Rate resolution now runs in precedence order, recording `rate_source`:
  1. `specialty_report` — matched on `spec_area`
  2. `geo_report` — **non-specialty parcels only**
  3. `specialty_hold_flat` / `specialty_prior_year` — per policy
  4. `model` — LightGBM / CoStar path
- **Guardrail**: the run stops if any specialty parcel ends up with a
  geographic rate (unless `geo_actuals_scope = "all"`).
- Per-year coverage report: parcels and prior-year AV by rate source, kept in
  `actuals_rate_coverage` for QA.

### `main_ml.R`
- New arguments `geo_actuals_scope` and `specialty_actuals_policy` (below).
- Step 0 reports geographic vs specialty rate counts and says out loud when a
  cycle has no specialty rates.
- Caches written before this change lack `report_kind` and are now detected
  and re-imported rather than silently reused.

## New arguments

| Argument | Values | Default | Effect |
|---|---|---|---|
| `geo_actuals_scope` | `"nonspecialty"`, `"all"` | `"nonspecialty"` | Which parcels geographic report rates may anchor. `"all"` reproduces the pre-fix behaviour. |
| `specialty_actuals_policy` | `"model"`, `"hold"`, `"prior"` | `"model"` | What a specialty parcel does when its specialty report has not been published. |

## Still open

- **Land vs improvements.** The 280 report states the geographic appraiser
  supplies the land value used by the specialty appraiser, so the Area 30 land
  change may legitimately flow to major office land even though the total does
  not. This is *not* implemented: the published Area 30 land table ($23.50B)
  exceeds every parcel population computed against it, including all parcels
  ($18.01B), so what that table measures is not yet established.
- **Seattle vs countywide.** Specialty populations are countywide. Area 280 was
  −5.75% countywide but −7.72% across the six Seattle Downtown submarkets. The
  parser now captures submarket rows, but the forecast still anchors on the
  specialty-wide rate. Building a Seattle-weighted rate from `spec_sub` is the
  natural next step.
- **CoStar lag.** Untouched. Still worth confirming the lag window does not
  overlap declines the assessor has already recognised.
