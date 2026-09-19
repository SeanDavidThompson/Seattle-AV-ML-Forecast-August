# =============================================================================
# xx_combine_res_comm_panel.R
# Merge residential and commercial parcel data into a single unified panel.
#
# Inputs (must exist in .GlobalEnv):
#   panel_tbl        — residential panel (from xx_combine_parcel_history_changes.R
#                      + xx_permits_to_panel.R + xx_nwmls_to_panel.R etc.)
#   parcel_comm_full — commercial parcel snapshot (from 02_transfrm_comm.R)
#   av_history_cln   — assessed value history for ALL parcels (res + comm)
#   chg_dt           — changes data.table from xx_tracking_changes.R
#
# Outputs:
#   panel_tbl_res    — residential rows only  (prop_type == "R")
#   panel_tbl_com    — commercial rows only   (prop_type == "C")
#   panel_tbl_all    — unified panel, all rows, all cols (superset)
# =============================================================================

# ---- Guards -----------------------------------------------------------------
stopifnot(
  exists("panel_tbl",        envir = .GlobalEnv),
  exists("parcel_res_full",  envir = .GlobalEnv),
  exists("parcel_comm_full", envir = .GlobalEnv)
)
# av_history_cln may have been dropped from GlobalEnv to free memory
# after xx_combine_parcel_history_changes.R ran. Reload from cache if needed.
if (!exists("av_history_cln", envir = .GlobalEnv)) {
  .av_path <- file.path(get("cache_dir", envir = .GlobalEnv), "av_history_cln.rds")
  if (!file.exists(.av_path))
    stop("av_history_cln not in memory and cache not found: ", .av_path)
  assign("av_history_cln", readRDS(.av_path), envir = .GlobalEnv)
  message("  Loaded av_history_cln from cache.")
  rm(.av_path)
}

panel_res <- as.data.table(copy(panel_tbl))
comm_snap <- copy(parcel_comm_full)
av_hist   <- copy(av_history_cln)
setDT(comm_snap)
setDT(av_hist)

# =============================================================================
# SECTION 0 — Cross-track deduplication: REMOVED (no longer reachable)
# =============================================================================
# This used to drop residential parcels whose Major also appeared in
# EXTR_CommBldg, to stop 2-4 unit small-MF parcels being forecast by both
# tracks.  The tracks now partition on PropType, so the overlap it cleaned up
# cannot occur:
#
#   02_transfrm.R        residential track keeps prop_type == "R"
#   01_import_comm.R     commercial universe is  prop_type == "C"
#                        (and the same levy_code_list)
#   EXTR_Parcel          one PropType per parcel
#   => res ids INTERSECT com ids = {} by construction
#
# Keeping it would now do harm rather than nothing: it matched on the 6-digit
# MAJOR, not on parcel_id, so a residential parcel sharing a Major with any
# commercial parcel was dropped from the residential panel even though it was
# never in the commercial panel.  Under the PropType split that is pure loss.
#
# The invariant it used to enforce is asserted for real in SECTION 5 below.
#
# is_small_mf itself is untouched (02_transfrm.R:159, qa_trajectory.R); only
# the derived is_small_mf_in_comm flag and the row drop are gone.


# =============================================================================
# SECTION 1 — Tag the residential panel
# =============================================================================
panel_res[, prop_type    := "R"]
panel_res[, prop_class   := "Residential"]   # coarse unified class

# parcel_res_full already has hbu_as_if_vacant_desc etc.; map to a
# common prop_class_detail field so downstream code has one column to use
panel_res[, prop_class_detail := hbu_as_if_vacant_desc]

# =============================================================================
# SECTION 2 — Build the commercial panel rows
# =============================================================================
# Commercial parcels span the same tax year range as residential.
# We create one row per parcel × tax_yr by cross-joining commercial snapshot
# with the residential year spine, then applying the same AV history merge.

# --- 2a. Year spine from residential panel
year_spine <- unique(panel_res[, .(tax_yr)])
setkey(year_spine, tax_yr)

# --- 2b. Expand commercial snapshot across years
comm_snap[, join_key := 1L]
year_spine[,  join_key := 1L]
panel_com_wide <- comm_snap[year_spine, on = "join_key", allow.cartesian = TRUE]
panel_com_wide[, join_key := NULL]
setDT(panel_com_wide)

# --- 2c. Attach AV history for commercial parcels
#   av_history_cln must contain: parcel_id, tax_yr, appr_land_val,
#   appr_imps_val, total_assessed (and log_ variants if present)
# Unlike the three-track combiner, av_hist is NOT normalised to no-dash here,
# so it already matches comm_snap's dashed parcel_id. Guard it anyway: a silent
# zero-match leaves the whole commercial panel with NA AV.
av_comm <- av_hist[parcel_id %chin% comm_snap$parcel_id]
message("  commercial AV history matched: ",
        uniqueN(av_comm$parcel_id), " of ", uniqueN(comm_snap$parcel_id),
        " parcels")
if (uniqueN(av_comm$parcel_id) == 0L)
  stop("No commercial parcel matched av_history_cln — parcel_id formats ",
       "disagree between comm_snap and av_history_cln.")
av_cols  <- intersect(
  names(av_comm),
  c("parcel_id","tax_yr","appr_land_val","appr_imps_val","total_assessed",
    "log_appr_land_val","log_appr_imps_val","log_total_assessed",
    "log_appr_land_val_lag1","log_appr_land_val_lag2")
)
av_comm <- av_comm[, ..av_cols]

panel_com_wide <- merge(panel_com_wide, av_comm,
                        by = c("parcel_id","tax_yr"), all.x = TRUE)

# --- 2d. Shared facet columns — fill with NA if not present in commercial
#   These come from the residential panel; commercial won't have all of them
#   We will union the columns so rows are comparable.
res_only_cols <- c(
  "current_zoning_3","hbu_as_if_vacant_desc","sq_ft_lot","nuisance_score",
  "mt_rainier","olympics","cascades","territorial","seattle_skyline",
  "puget_sound","lake_washington","seismic_hazard","landslide_hazard",
  "steep_slope_hazard","traffic_noise","airport_noise","power_lines",
  "other_nuisances","contamination","historic_site",
  "dist_to_public_km","dist_to_private_km","dist_to_lightrail_km",
  "unbuildable",
  # permit cols
  "permits_last_1yr","permits_last_3yr","permits_last_5yr",
  "val_last_3yr","val_last_5yr","sqft_last_3yr","units_last_3yr",
  "any_newconst","years_since_newconst","log_val_last_3yr","log_sqft_last_3yr",
  # market cols (NWMLS — not applicable to commercial)
  "sea_pmedesfh_f","sea_sesfhf","sea_alesfhf"
)

for (col in res_only_cols) {
  if (!col %in% names(panel_com_wide)) {
    panel_com_wide[, (col) := NA]
  }
}

# --- 2e. Prop type / class columns for commercial
panel_com_wide[, prop_type         := "C"]
panel_com_wide[, prop_class        := "Commercial"]
panel_com_wide[, prop_class_detail := predominant_use_desc]

# --- 2f. Ensure residential panel has commercial-specific cols as NA
comm_only_cols <- c(
  "n_comm_bldgs","total_gross_sqft","total_net_sqft","max_nbr_stories",
  "avg_bldg_quality","max_bldg_quality","predominant_use","predominant_use_desc",
  "constr_class","yr_built_first_comm","yr_built_last_comm","eff_yr_comm",
  "avg_age_comm","min_eff_age_comm","has_sprinklers","has_elevators",
  "pcnt_complete_comm","n_sections","n_distinct_uses","sect_gross_sqft",
  "sect_net_sqft","office_sqft","retail_sqft","industrial_sqft",
  "institutional_sqft","parking_sqft","avg_story_height",
  "parking_garage_sqft","surface_parking_sqft","canopy_sqft",
  "other_feature_sqft","total_feature_sqft","total_parking_sqft",
  "office_pct","retail_pct","industrial_pct","parking_pct",
  "net_to_gross_ratio","log_gross_sqft","log_net_sqft",
  "prop_class_comm","size_tier"
)

for (col in comm_only_cols) {
  if (!col %in% names(panel_res)) {
    panel_res[, (col) := NA]
  }
}

if (!"prop_class_detail" %in% names(panel_res))
  panel_res[, prop_class_detail := hbu_as_if_vacant_desc]

# =============================================================================
# SECTION 2e — Join econ macro variables onto commercial panel
# =============================================================================
# xx_econ_to_panel.R joins econ cols onto panel_tbl (residential) only.
# Commercial rows get NA-filled via add_missing() unless we join explicitly here.
# Load econ_vars_to_join from cache and merge by tax_yr.
{
  # Prefer the all-years econ cache (historical + forecast, written by xx_econ_to_panel.R)
  # Fall back to forecast-only cache if all-years not yet generated.
  econ_all_path <- file.path(cache_dir, paste0("econ_all_years_", scenario, ".rds"))
  if (file.exists(econ_all_path)) {
    econ_files <- econ_all_path
  } else {
    econ_cache_pat <- paste0("econ_fcst_.*_", scenario, "\\.rds$")
    econ_files     <- list.files(cache_dir, pattern = econ_cache_pat, full.names = TRUE)
    if (length(econ_files) > 0) {
      message("  ℹ️  econ_all_years cache not found — using forecast-only cache (historical rows will have NA econ)")
    }
  }
  if (length(econ_files) > 0) {
    econ_wide <- data.table::as.data.table(
      readRDS(econ_files[[1]])
    )
    if ("tax_yr" %in% names(econ_wide)) {
      # Drop any econ cols already present (all NA from prior steps)
      econ_col_names <- setdiff(names(econ_wide), "tax_yr")
      for (col in intersect(econ_col_names, names(panel_com_wide)))
        panel_com_wide[, (col) := NULL]
      panel_com_wide <- merge(panel_com_wide, econ_wide, by = "tax_yr", all.x = TRUE)
      message("  Econ columns joined to commercial panel: ", length(econ_col_names))
    }
  } else {
    message("  No econ forecast cache found — commercial panel will have NA econ cols.")
  }
}

# =============================================================================
# SECTION 3 — Stack into unified panel
# =============================================================================
# Find all columns present in either table, bind rows
all_cols <- union(names(panel_res), names(panel_com_wide))

add_missing <- function(dt, cols) {
  missing <- setdiff(cols, names(dt))
  for (col in missing) dt[, (col) := NA]
  dt
}

panel_res      <- add_missing(panel_res,      all_cols)
panel_com_wide <- add_missing(panel_com_wide, all_cols)

# Reorder to same column order
setcolorder(panel_res,      all_cols)
setcolorder(panel_com_wide, all_cols)

panel_tbl_all <- rbindlist(list(panel_res, panel_com_wide), use.names = TRUE)

# Key columns
setDT(panel_tbl_all)
setkeyv(panel_tbl_all, c("parcel_id","tax_yr"))

# =============================================================================
# SECTION 4 — Split back to typed subsets
# =============================================================================
panel_tbl_res <- panel_tbl_all[prop_type == "R"]
panel_tbl_com <- panel_tbl_all[prop_type == "C"]

message("  panel_tbl_all rows: ", nrow(panel_tbl_all),
        "  (res=", nrow(panel_tbl_res),
        ", com=", nrow(panel_tbl_com), ")")

# =============================================================================
# SECTION 5 — Partition assertions (res/com duplication guard)
# =============================================================================
# Defined in 00_init.R so this and xx_combine_res_comm_condo_panel.R share one
# copy. Hard-stops on any res/com parcel_id overlap, and on res + com failing
# to reconcile to the Seattle levy-code population of EXTR_Parcel by PropType.
# No condo track on this path, so none is passed.
if (!exists("assert_res_com_partition", envir = .GlobalEnv))
  stop("assert_res_com_partition() not found — source 00_init.R first.")
assert_res_com_partition(panel_tbl_res, panel_tbl_com)

# =============================================================================
# SECTION 6 — Export to GlobalEnv
# =============================================================================
assign("panel_tbl_res", panel_tbl_res, envir = .GlobalEnv)
assign("panel_tbl_com", panel_tbl_com, envir = .GlobalEnv)
assign("panel_tbl_all", panel_tbl_all, envir = .GlobalEnv)

message("xx_combine_res_comm_panel.R loaded.")
