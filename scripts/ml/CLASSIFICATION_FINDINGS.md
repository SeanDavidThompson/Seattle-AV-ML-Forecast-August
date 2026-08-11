# Commercial subgroup assignment — code-system defect

**Date:** 2026-08-02
**Files:** `scripts/ml/main_ml.R` (lines 106–140, `COM_SUBGROUPS`), `split_com_panel_to_subgroups()` (line ~557)
**Evidence:** `EXTR_LookUp.csv`, `Parcel.doc`, `Commercial Building.doc` (Real Property Appraisal History data dictionary)

---

## Summary

`COM_SUBGROUPS$<key>$present_use_codes` is matched against `present_use`, which
`split_com_panel_to_subgroups()` reads from **`EXTR_Parcel.PresentUse`**. Per the
Parcel record description, `PresentUse` is **LookUp type 102**.

The code values currently in `COM_SUBGROUPS` are **LookUp type 118** values
(`SectionUse` / `PredominantUse`, the commercial building use system) — and
within type 118 they are also assigned to the wrong subgroups.

Two independent errors stacked on top of each other.

---

## Evidence

| subgroup | code | meaning in PresentUse (type 102) | meaning in SectionUse (type 118) |
|---|---|---|---|
| apt | 470 | *does not exist* | EQUIPMENT (SHOP) BUILDING |
| apt | 472 | *does not exist* | EQUIPMENT SHED |
| apt | 706 | *does not exist* | BASEMENT, PARKING |
| office | 300 | **Vacant(Single-family)** | APARTMENT |
| office | 309 | **Vacant(Commercial)** | CHURCH |
| industrial | 365 | *does not exist* | ELEMENTARY SCHOOL (ENTIRE) |
| industrial | 406 | *does not exist* | STORAGE WAREHOUSE |
| industrial | 407 | *does not exist* | WAREHOUSE, DISTRIBUTION |
| industrial | 528 | *does not exist* | GARAGE, SERVICE REPAIR |
| retail | 326 | **Open Space(Curr Use-RCW 84.34)** | GARAGE, STORAGE |
| retail | 348 | *does not exist* | Residence |
| retail | 349 | *does not exist* | FAST FOOD RESTAURANT |
| retail | 350 | *does not exist* | RESTAURANT, TABLE SERVICE |
| retail | 352 | *does not exist* | MULTIPLE RESIDENCE (LOW RISE) |
| retail | 353 | *does not exist* | RETAIL STORE |
| retail | 531 | *does not exist* | MINI-MART CONVENIENCE STORE |
| hospitality | 341 | **Rooming House** | MEDICAL OFFICE |
| hospitality | 344 | **Terminal (Freight Auto/Rail/Other)** | OFFICE BUILDING |
| hospitality | 386 | *does not exist* | MINI-WAREHOUSE |
| medical | 304 | *does not exist* | BANK |
| medical | 426 | *does not exist* | DAY CARE CENTER |

Read the last column on its own and the second error is visible without
reference to type 102 at all: `344 OFFICE BUILDING` is assigned to
**hospitality**, `300 APARTMENT` to **office**, `341 MEDICAL OFFICE` to
**hospitality**, `304 BANK` to **medical**, and `349/350` (the two restaurant
codes) to **retail** rather than hospitality.

---

## Consequences

**1. Pass 2 is almost entirely inert.** 16 of the 21 codes do not exist in type
102, so they match nothing. Every parcel that pass 1 (`spec_area_name`) misses
falls through to `com_other` and gets the flat carry-forward. This is the direct
mechanical cause of the oversized residual bucket.

**2. The five codes that *do* match, match the wrong things.** In particular:

> `office` picks up `300 Vacant(Single-family)` and `309 Vacant(Commercial)` —
> **vacant land parcels are being trained and forecast as Major Office.**

This is worth pausing on given the last week of office work. Vacant commercial
land is land-only, has no improvements, and reprices on a completely different
basis from a downtown tower. Those parcels are sitting in `model_data_office_*`
contributing land-delta and improvement-delta rows to a 396-parcel segment that
has already proved unstable to four successive modelling interventions. It is
consistent with the note in the pipeline record that office non-specialty
parcels number 46 in 2027 while specialty parcels number 396 — the 46 are the
population this list would produce.

`retail` picks up `326 Open Space (Current Use, RCW 84.34)`; `hospitality`
picks up `341 Rooming House` and `344 Freight Terminal`.

**3. Nothing here affects the residential or condo tracks**, and pass 1
(`spec_area_name` via `crosswalk.xlsx`) is unaffected — the specialty
assignments that drive most of the commercial AV are correct. This is a
pass-2 defect.

---

## Fix

`scripts/ml/xx_com_classify.R` contains `COM_SUBGROUPS_PU`, a corrected type-102
mapping verified line by line against `EXTR_LookUp.csv`, plus `classify_use118()`
for the type-118 fields where they are genuinely the right source
(`SectionUse`, `PredominantUse`).

Vacant-land codes (`299, 300, 301, 309, 316, 323–337, 339`) and parking codes
(`159, 180, 182, 277`) are held out of every built-space subgroup by an explicit
guardrail rather than by omission, so this cannot silently regress.

See `PATCH_2026-08-02.md` for the drop-in replacement block.

---

## Caveat

None of this has been executed — I have the scripts, the data dictionary and the
lookup table, not the extracts. The code lists are verified against
`EXTR_LookUp.csv`; the *effect* on parcel counts and AV is not. The first thing
worth running is the before/after subgroup count table in the patch QA
checklist, which will tell you the size of the reassignment before anything
touches a model.
