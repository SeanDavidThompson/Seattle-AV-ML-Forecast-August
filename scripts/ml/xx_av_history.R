# xx_av_history.R
# Build av_history_cln: one row per parcel x tax_yr, deduplicated,
# covering tax_yr 2001–2026 (TY2026 = 2025 assessment, payable 2026).
# NOTE: In KCA convention, tax_yr is the billing/payable year, not the
# assessment year. TY2026 = assessed Jan 1 2025, taxes due in 2026.

# ---- Import -----------------------------------------------------------------
av_history_raw <- read_csv(
  here("data", "kca", kca_date_data_extracted,
       "value_history", "EXTR_ValueHistory_V.csv")
) %>%
  clean_names() %>%
  filter(
    levy_code %in% levy_code_list,
    tax_status == "T",
    is.na(tax_val_reason)
  )

# ---- Build parcel_id --------------------------------------------------------
av_history <- av_history_raw %>%
  mutate(
    parcel_id = paste0(
      str_pad(as.character(major), 6, "left", "0"),
      "-",
      str_pad(as.character(minor), 4, "left", "0")
    )
  )

# ---- Clean: deduplicate, keep latest change ---------------------------------
# Within each parcel x tax_yr, keep the row with the latest change_date.
#
# NOTE (2026-07): the previous version additionally required a FULL balanced
# history — parcel present in every year 2001..max — which silently excluded
# every parcel created after 2001 and any parcel with a single filtered year
# (exemption, special valuation).  Measured cost: only ~30-38% of commercial
# parcels and ~15% of condo units survived, capping delta-model training,
# retro fills, and actuals-anchor coverage across all tracks.  Delta targets
# tolerate gaps (a missing year just yields an NA delta row), so the
# balanced-panel requirement is dropped.  Last observed year is now derived
# from the configured horizon (forecast_start - 1) instead of hardcoded.
av_history_cln <- av_history %>%
  filter(tax_yr > 2000,
         tax_yr <= get0("forecast_start", envir = .GlobalEnv,
                        ifnotfound = 2027L) - 1L) %>%
  group_by(parcel_id, tax_yr) %>%
  slice_max(change_date, n = 1, with_ties = FALSE) %>%
  ungroup()

assign("av_history_cln", av_history_cln, envir = .GlobalEnv)
message("xx_av_history.R loaded — av_history_cln: ",
        nrow(av_history_cln), " rows | tax_yr ",
        min(av_history_cln$tax_yr), "–", max(av_history_cln$tax_yr))
