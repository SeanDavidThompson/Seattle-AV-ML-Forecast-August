# KCA area report formats, assessment years 2019–2026

Design note for extending `scripts/ml/area_report_import.R` so that the
backtest has area-report actuals for every origin year from 2019 to 2026.
Each finding (F1–F9) below lists the problem, the proposed fix and the code
it touches. Line numbers refer to `area_report_import.R` at `255c41f`.

**Status: approved and implemented.** The review decisions are listed near the
end. The string-based tests pass. The expected-value tests and the 2026
comparison are waiting for the PDFs.

---

## 0. Changes that apply to every finding

### 0.1 File discovery (new layout)

The PDFs live in `data/kca/area_reports/<year>/{residential,commercial}/`.
Area numbers overlap between the two subfolders (for example, `048.pdf` can be
in both).

| Where | Now | Proposed |
|---|---|---|
| `area_report_import.R:563` | `list.files(area_reports_dir, …)` on the year folder only, so it finds nothing | `list.files(…, recursive = TRUE)`. Each file keeps `subfolder` (the first path component under the year) as a hint. PDFs directly in the year folder, or in any other subfolder, are still parsed, with `subfolder = NA` and a warning. |
| `area_report_import.R:569,609` | `src <- basename(f)`; `report_id <- file_path_sans_ext(src)` | `source_file = "residential/048.pdf"`, `report_id = "residential/048"`, so the path is relative to the year folder. |
| `backtest_harness.R:396` (`bt_check_area_reports`) | Non-recursive `list.files`, so a correctly filled year looks empty and the harness stops | Add `recursive = TRUE`. Without this, the anchored backtest refuses every origin. |

The subfolder is used only as a hint. The report type comes from the content
(F1). When the content family (`res`/`condo` vs `com`) disagrees with the
subfolder, the importer warns and records the disagreement in the coverage
table. It does not skip the file.

### 0.2 Restructure into functions (so tests can call them)

The script currently runs at source time. Proposed structure:

- `ar_classify(pages, subfolder)` returns `list(family, kind, spec_nos, area, cover_year)` (F1, F4, F5, F9)
- `ar_parse_file(path, rel, folder_year)` returns `list(rows, status, reason)` and never throws. Errors become `status = "unparsed"` and `reason = conditionMessage(e)`.
- `ar_import_year(year, root)` returns `list(actuals, coverage)`
- `ar_coverage_table(coverage)` prints the per-year table (§0.4)
- The bottom of the script keeps today's behaviour for `main_ml.R`: import `area_reports_year`, assign `area_report_actuals` to `.GlobalEnv`, and write the cache and csv. Sourcing with `AREA_REPORT_DEFINE_ONLY <- TRUE` only defines the functions, which is the same pattern as `MAIN_ML_DEFINE_ONLY`.

`.pdf_lines()` becomes `.pdf_pages()`, which keeps the page boundaries. The
cover checks in F1, F4, F5 and F9 look at **page 1 only**, and the other
parsers use the flattened lines exactly as they do now.

### 0.3 Schema

The current columns stay unchanged, including `assessment_yr` (the folder
year). There are two new columns, placed right after it:

- `report_year` is the cover year.
- `tax_yr` is `assessment_yr + 1`. This is the same convention the consumers
  already use (`06_…sequential.R:364`, `06_…_comm.R:395`,
  `xx_com_other_growth.R:235`).

### 0.4 Coverage table (no file skipped silently)

Every PDF found gets exactly one coverage record:
`year, subfolder, file, family, kind, cover_year, status, n_rows, reason`.
The `status` values are:

- `parsed`
- `unparsed`, with a reason such as "no value table", "spec number not resolvable", "read error: …", or "cover year 2021 ≠ folder 2022"
- `known_gap`, only for the list in F8. It is never imputed.

Printed per year (the counts below only show the layout; they are not real):

```
year  found  parsed  unparsed  known_gap
2019     58      56         1          1
...
unparsed:
  2022 commercial/045.pdf  cover year 2021 != folder 2022
```

The table is also written to `<output_dir>/area_report_coverage_<year>.csv`.
The current per-file `✅` messages stay.

---

## F1 — Routing: bare "Specialty" misroutes geographic reports

**Problem.** `is_spec` (`:584`) is `grepl("Specialty", head_txt)` over the
first 200 lines. Every commercial cover letter says "geographic or
specialty", so before 2021 every single-area geographic report, which lacks
the words "Geographic Areas Report", is sent to `parse_spec_report`.

**Fix.** `ar_classify()` works on page 1 and decides in this order:

1. **condo** if the page matches `Residential\s+Condominium` or `Specialty\s*700\b` (colon optional; 2019 has none).
2. **specialty** if the page matches `Commercial\s+Revalue` and has **any**
   3-digit area number: `(Specialty(\s+Areas?)?|Areas?)\s*:?\s*\d{3}`.
   - More 3-digit numbers may follow, joined by `&`, `and`, `,` or `/`.
   - A number that is not in `KCA_SPECIALTY_AREAS` is still routed as a
     specialty, with a warning. It never falls back to geographic, because
     geographic areas are 2-digit in every report seen.
   - `Area: 048` on a residential cover never reaches this branch, because
     the cover has to say "Commercial".
3. **res** if the page matches `Residential\s+Revalue`.
4. **com_geo** if the page matches `Commercial\s+Revalue`.
5. Otherwise **unknown**. Every parser is tried as now, and the result is still reported in the coverage table.

`KCA_SPECIALTY_AREAS`, at the top of the script, is
`100, 153, 160, 174, 250, 280, 413, 500, 510, 520, 608, 625, 700`.

**Where.** `:576–600` are replaced by `ar_classify()`. The
`any(grepl("Specialty\\s*700\\s*:"…))` at `:582` is folded into step 1.

## F2 — Submarket false positives (2019 area 36)

**Problem.** A misrouted geographic report matched `sub_pat` (`:412`) on its
neighbourhood LAND table and produced "submarkets" of a specialty 36 that
does not exist. The spec number 36 came from `Area: 36` or from the file-name
fallback at `:311–314`.

**Fix.**
- A submarket row is kept only if `g[2] %in% KCA_SPECIALTY_AREAS` **and** `g[2] %in% spec_nos` (`:421`).
- The file-name fallback at `:311–314` is removed. If there is no spec number from the content, the file is `unparsed` with the reason "spec number not resolvable". The number is never guessed.
- F1 already stops the misroute. This check is a second guard.

## F3 — One tolerant value-table reader

**Problem.** The same block (two value rows plus a change row, with land,
imps and total) is parsed by two separate readers: `.spec_value_summary`
(`:196`) and `grab_table` inside `parse_condo_report` (`:488`). Each accepts
only one set of labels.

**Fix.** Add one helper, `.value_table(lines, hdr, window = 14, from = 1)`.

- **Header** alternation, case-insensitive, with the separators `\s*[-–:]?\s*`:
  `Total Population - Parcel Summary Data`, `TOTAL POPULATION SUMMARY DATA`,
  `Parcel Summary Data`, `Population - Improved Valuation Change Summary`,
  `Population - Improved Parcel Summary`, `Change in Total Assessed Value`,
  `Population Value Summary` (the existing one), and `Summary - Total Value - % Change` (F8).
  Callers pass a subset. For example, the res and condo population block must not match a `Sales -` header.
- **Value rows:** `^\s*(\d{4})\s+Val(?:ue|ues|uation)\s+` followed by 3 money columns, or by 1 money column for total-only tables. The money token accepts `$`, a sign on either side of `$`, spaces and parentheses (the same token as F6).
- **Change row:** `^\s*(%\s*Change|Percent\s+Change|Value\s+Increase)\s+` followed by 1–3 percentages. If a "Value Increase" row contains dollars rather than percentages, it is read as the delta and the rate is recomputed.
- **Output:** `prev`, `curr` (land, imps, total or NA), `pct_printed`, `pct_calc = curr/prev − 1`.
  `pct_change` = printed value, falling back to calculated. This keeps 2026 output identical, because the current code also prefers the printed value.
  If `|printed − calc| > 0.005`, the importer warns with the file, the component and both values.
- `from` lets F5 (2025 North) read the n-th occurrence.

**Where.** New helper. `.spec_value_summary` and condo `grab_table` become
thin wrappers around it. The recheck in the validation block (`:655`) stays
as it is.

## F4 — Residential: two layouts, 2021–2024 overlap

**Problem.** `parse_res_report` (`:111`) only reads the multi-area rows
`Area N Sales|Pop …`. Single-area reports (2019–2021, and Area 011 in 2024)
use the Sales and Population blocks from F3 and return NULL. In 2024 Area 22
the multi-area row says `Sale`.

**Fix.**
- Multi-area regex: `(Sales|Pop)` becomes `(Sales?|Pop)`.
- If there are no multi-area rows, fall back to the single-area layout. Read
  the `Population - Improved …` block (basis `population`) and the
  `Sales - Improved Valuation Change Summary` block (basis `sales`) with
  `.value_table()`. Take the area from page 1 with `\bArea\s*:?\s*0*(\d{1,3})\b`.
  If no cover area is found, the file is `unparsed` with the reason "no area number on cover".
- The layout is chosen by what matches, not by the year, because both layouts exist in 2021–2024.
- The single-area layout also fills `land_*`, `imps_*`, `pct_land` and `pct_imps`, which the expected values need (for example, 2019 A48 has 0.0 / −1.9).

**Where.** `parse_res_report`. The condo parser shares the same block
readers, so `parse_condo_report` gets simpler too.

## F5 — Commercial geographic

Three layouts are tried in order inside `parse_com_report` (`:146`):

1. **Geo Area table** (2021–2024, 2025 Central). This is today's code and is unchanged.
2. **Single area** (2019–2020). Read `.value_table()` with the Parcel-Summary
   and Total-Population headers. Take the area from page 1 with
   `(?i)\bArea\s*:?\s*(\d{1,3})\b`, which covers both "Area: 25" and "AREA 36".
   The row includes land and imps.
3. **2025 North** (no summary table).
   - Each area's pages start with a bare `Area NN` line. In order, these are
     areas 10, 14, 17, 19, 80, 85, 90 and 95. The heading regex is
     `^\s*Area\s+(\d{1,2})\s*$`, so a line such as "Area 10 covers …" is not
     a heading.
   - Each `Change in Total Assessed Value` table is assigned to the
     **nearest preceding** heading.
   - Prose matching `([+-]?\d+(\.\d+)?)%\s+in\s+Geographic\s+Area\s+(\d+)` is
     a cross-check only. It is worded differently for areas 19 and 95, so an
     area missing from the prose is fine. The importer warns if a prose
     figure differs from its section by more than 0.01pp, or if the prose
     names an area that has no section.
   - Warn if the number of sections is not 8. Warn if two sections map to the same area; in that case keep the first, and the second becomes a coverage note.

## F6 — Specialty headline sign bug

**Problem.** `tot_pat` (`:323`) needs `\$` first, so `- $ 504,585,300` (2026
area 500) and `-$436,452,750` (2026 area 510) do not match. Those headlines
are currently lost.

**Fix.** Use one shared money token for all three money columns:
`([-+]?\s*\$\s*[-+]?\s*\(?[\d,]+\)?)`. The sign can come before or after `$`,
with or without spaces. The percentage becomes `([-+]?\s*[\d.]+)\s?%`.
`.num()` is unchanged. It treats any `-` or `(` as negative, but it only
ever receives captured number tokens, never label text, so it did not need
tightening. The Unicode minus sign (U+2212) is converted to `-` when lines are
read.

**Where.** `tot_pat` at `:323`, plus the same token in `parse_com_report`
`row_pat` (`:150`) and `sub_pat` (`:412`).

### Specialty land/imps

- If a headline heading has value rows under it, it is read with the F3
  reader. 2022 spec 160 has `Change in Total Assessed Value` over
  "2021 Values" land/imps/total rows.
- A report with only a population table is read the same way. 2020 spec 250
  has only `Total Population - Parcel Summary Data` with a "Percent Change"
  row.
- When there is both a headline row and a value table (280 in 2019, 153/174
  in 2024):
  - `av_*`, `delta` and `pct_change` come from the headline.
  - `land_*`, `imps_*`, `pct_land` and `pct_imps` come from the table.
  - The importer warns if the two totals differ by more than 0.5pp.

## F7 — Multi-specialty files (153 & 174; 2024, 2026)

**Fix.**
- `ar_classify` collects **all** spec numbers on page 1, for example "153 & 174" or "153 and 174".
- When there is more than one, `parse_spec_report` splits the lines into
  segments at heading lines matching `^\s*(Specialty\s+Area\s+(\d{3})\s*[-–]|Spec\.?\s+(\d{3})\b)`,
  and runs the headline and submarket parsers once per segment.
- The result is one `specialty` row per spec number. `report_id` and `source_file` are shared.
- If a listed spec number has no segment or no headline, the file is still `parsed`, and the coverage reason says "spec 174: no headline".

## F8 — Apartments (spec 100)

**2021 layout.**
- The header alternation in `.spec_regions` (`:221`) gains `Summary\s*-\s*Total\s+Value\s*-\s*%\s*Change`.
- Region definitions: the existing `Name (n)` pattern gets an alternative `Region\s+(\d)\s*\(([A-Za-z/ ]+)\)`, so "Region 1 (Central)" becomes code 1 = central. Region rows such as "C/N" are matched to a code by name, as now.
- Definitions read "Region 1 (Central), Region 2 (South), Region 3 (East)".
- Inventory title: `Inventory - Regions and Neighborhoods`.
- Inventory layout:
  - A `Region 1 / Region 2 / Region 3` column header sits over an
    `NHD # / NHD Name` sub-header.
  - Rows hold number–name pairs, three per line, for example
    "5 Downtown   160 Seward Park   340 Mercer Island".
  - Each pair's column (its region) comes from its character position. The
    column edges come from the `NHD #` sub-header, or from the Region
    headers if the sub-header is missing.
  - The table ends at the first non-blank line with no pair.
- There are no project counts and no totals footer, so the R-total check
  (`:277`) is skipped for 2021, and the log says so.
- 2022–2026 are unchanged.

**2019.** There is no summary table. `KNOWN_GAPS <- tibble(year = 2019L, spec_area = 100L, reason = "no summary table in report")`.
The file is marked `known_gap`, and no specialty row is produced or imputed.
If a 2019 apartments file ever does parse, the importer warns that the known
gap is stale.

## F9 — Year from the cover

**Fix.**
- `cover_year` is read from page 1 with `for\s+(\d{4})\s+Assessment\s+Roll`, after collapsing whitespace so a wrapped line still matches.
- If there is no cover year, the file is still parsed, `report_year` is NA, and the importer warns.
- If the cover year and the folder year differ, the importer warns. The file
  is `unparsed` for that folder year with the reason
  `cover year 2021 != folder 2022`. This matches the two 2021 reports found
  in the 2022 folder.
- Rows never reach an origin's output with the wrong `assessment_yr`. The
  `main_ml.R:688` guard would stop the backtest in that case anyway.
- Output rows carry `assessment_yr` (the folder year), `report_year` (the
  cover year) and `tax_yr`.

---

## Testing

The tests will use `tests/testthat/` with testthat 3e.

- **`test-area-report-units.R`** needs no PDFs. It uses hand-written strings
  for the F1 cover letter ("geographic or specialty"), the F1 condo 2019 form,
  the F2 rejection, every F3 header, row and change label, every F6 sign form,
  F7 heading splitting, and the F9 cover year. It runs everywhere.
- **`test-area-report-expected.R`** runs `ar_import_year()` for each year and
  checks the expected-values table from the brief, stored as a
  `tibble::tribble` in `helper-area-report-expected.R`. A csv would be
  gitignored. Each file is looked up by content (prop_type, kind, area or
  spec, basis = population), not by file name.
  - Total rate tolerance: ±0.05pp for 1-decimal figures and ±0.005pp for 2-decimal figures, on the printed value.
  - Land and imps are checked where they are given.
  - Spec 280 must have 20 submarkets in each year from 2019 to 2025.
  - The file uses `skip_if_not(dir.exists(<year dir>))`, because the PDFs are never committed.
- **2026 regression.**
  - `ad_hoc/area_report_baseline_2026.R` runs the **unmodified** import from
    `376df49` (taken with `git show`, so the result does not depend on the
    working tree). It runs on a flat temporary copy of the 2026 PDFs, because
    that script does not look inside subfolders. The copies are named like
    `commercial__280.pdf`, so that same-numbered files do not collide.
  - The baseline is refused unless it has rows for res, commercial
    geographic, specialty, apartments and condo. It is saved to
    `data/cache/area_report_baseline_main_2026.rds`.
  - `ad_hoc/area_report_compare_2026.R` defines `ar_compare_actuals()`,
    which `test-area-report-2026-regression.R` also uses. It joins on
    `prop_type, report_kind, area, spec_area, spec_sub, spec_region, basis`.
  - `nbhds` is added to the join key when the key alone is not unique. This
    is the case for condo reports, where several reports share spec 700 with
    no area.
  - Every column is compared except `report_id`, `source_file`,
    `report_year` and `tax_yr`.
  - Each difference is classified, printed, and counted by kind at the end:

    | kind | meaning | result |
    |---|---|---|
    | `total_mismatch` | `av_prev`, `av_curr`, `delta`, `pct_change` or `pct_change_chk` differs in a row that is in both outputs | fail, in every spec |
    | `filled` | a land/imps column that was blank in the baseline now has a value | allowed |
    | `changed` | a column went from one non-blank value to another | fail outside 153/174, 500, 510 |
    | `cleared`, `other_filled` | non-blank to blank, or blank to non-blank in a column other than land/imps | fail outside 153/174, 500, 510 |
    | `added`, `removed` | a whole row | fail outside 153/174, 500, 510 |

## Decisions (review, 2026-09-23)

- **Year columns:** `assessment_yr` is the folder year and `report_year` is
  the cover year. A mismatch is reported as unparsed. `tax_yr =
  assessment_yr + 1` is an explicit column.
- **Specialty list:** 413, 520, 608 and 625 were added. Any 3-digit area
  number on a commercial cover is routed as a specialty, with a warning if it
  is not on the list. It never falls back to geographic.
- **Misfiled reports:** the two misfiled 2021 reports are being moved on disk.
  The importer only warns about wrong-year files.
- **Backtest check:** `bt_check_area_reports` now looks inside the
  subfolders (on this branch).

## Still to run (needs the PDFs, `pdftools` and `testthat`)

These commands must be run in this order, from the repo root.

1. Capture the baseline:
   ```
   Rscript ad_hoc/area_report_baseline_2026.R
   ```
2. Run all the tests. Each year's expected-value tests are skipped while that
   year's folder is missing.
   ```
   Rscript -e 'testthat::test_dir("tests/testthat")'
   ```
3. Print the 2026 differences:
   ```
   Rscript ad_hoc/area_report_compare_2026.R
   ```

The F5, F8 and specialty land/imps rules now follow the descriptions of the
real reports (2026-09-23). None of them has been run against the PDFs yet.

The land/imps merge fills `land_*`/`imps_*` on any **2026** specialty
headline that also has a population value table. This was accepted in
review: such differences are classified as `filled` and allowed, while totals
must still match exactly.
