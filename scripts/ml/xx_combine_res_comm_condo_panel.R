# =============================================================================
# xx_combine_res_comm_condo_panel.R
# Stack residential, commercial, and condo panels into a unified dataset.
# Replaces xx_combine_res_comm_panel.R — adds the condo (prop_type="D") track.
#
# Inputs (GlobalEnv):
#   panel_tbl          — residential panel (from xx_combine_parcel_history_changes.R)
#   parcel_comm_full   — commercial snapshot (from 02_transfrm_comm.R)
#   parcel_condo_full  — condo unit snapshot  (from 02_transfrm_condo.R)
#   av_history_cln     — AV history for ALL parcel types
#
# Outputs (GlobalEnv):
#   panel_tbl_res   — residential rows  (prop_type == "R")
#   panel_tbl_com   — commercial rows   (prop_type == "C")
#   panel_tbl_condo — condo rows        (prop_type == "D")
#   panel_tbl_all   — unified superset  (all rows, all cols)
# =============================================================================

stopifnot(
  exists("panel_tbl",         envir = .GlobalEnv),
  exists("parcel_res_full",   envir = .GlobalEnv),
  exists("parcel_comm_full",  envir = .GlobalEnv),
  exists("parcel_condo_full", envir = .GlobalEnv)
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

panel_res   <- as.data.table(copy(panel_tbl))
comm_snap   <- copy(parcel_comm_full)
condo_snap  <- copy(parcel_condo_full)
av_hist     <- copy(av_history_cln)
setDT(comm_snap)
setDT(condo_snap)
setDT(av_hist)
av_hist[, parcel_id := gsub("-", "", parcel_id)]  # normalize to no-dash (matches panel parcel_id format)

# ---- Helper ----------------------------------------------------------------
add_missing_cols <- function(dt, cols) {
  for (col in setdiff(cols, names(dt))) dt[, (col) := NA]
  dt
}

# ---- Year spine ------------------------------------------------------------
year_spine <- unique(panel_res[, .(tax_yr, join_key = 1L)])

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
# The invariant it used to enforce is asserted for real in SECTION 7 below,
# which stops the run on any res/com overlap.
#
# is_small_mf itself is untouched (02_transfrm.R:159, qa_trajectory.R); only
# the derived is_small_mf_in_comm flag and the row drop are gone.


# =============================================================================
# SECTION 1 — Tag residential panel
# =============================================================================
panel_res[, prop_type         := "R"]
panel_res[, prop_class        := "Residential"]
if (!"prop_class_detail" %in% names(panel_res))
  panel_res[, prop_class_detail := hbu_as_if_vacant_desc]

# =============================================================================
# SECTION 2 — Build commercial panel (unchanged from xx_combine_res_comm_panel.R)
# =============================================================================
comm_snap[, join_key := 1L]
panel_com_wide <- comm_snap[year_spine, on = "join_key", allow.cartesian = TRUE]
panel_com_wide[, join_key := NULL]
setDT(panel_com_wide)

# av_hist was normalised to no-dash above, which is right for the residential
# and condo panels but not for comm_snap: 01_import_comm.R builds a DASHED
# parcel_id, so matching the two as-is silently returns nothing and the whole
# commercial panel arrives with NA AV.  Match the commercial id format here.
com_dashed <- any(grepl("-", utils::head(comm_snap$parcel_id, 10L)))
av_comm_src <- if (com_dashed) {
  .a <- copy(av_hist)
  .a[, parcel_id := paste0(substr(parcel_id, 1, 6), "-", substr(parcel_id, 7, 10))]
  .a
} else av_hist
av_comm <- av_comm_src[parcel_id %chin% comm_snap$parcel_id]
message("  commercial AV history matched: ",
        uniqueN(av_comm$parcel_id), " of ", uniqueN(comm_snap$parcel_id),
        " parcels (id format: ", if (com_dashed) "dashed" else "no-dash", ")")
if (uniqueN(av_comm$parcel_id) == 0L)
  stop("No commercial parcel matched av_history_cln — parcel_id formats ",
       "disagree between comm_snap and av_history_cln.")
av_cols <- intersect(names(av_comm),
  c("parcel_id","tax_yr","appr_land_val","appr_imps_val","total_assessed",
    "log_appr_land_val","log_appr_imps_val","log_total_assessed",
    "log_appr_land_val_lag1","log_appr_land_val_lag2"))
panel_com_wide <- merge(panel_com_wide, av_comm[, ..av_cols],
                         by = c("parcel_id","tax_yr"), all.x = TRUE)

panel_com_wide[, prop_type         := "C"]
panel_com_wide[, prop_class        := "Commercial"]
panel_com_wide[, prop_class_detail := predominant_use_desc]

# =============================================================================
# SECTION 3 — Build condo panel
# =============================================================================
# Exclude condo units that already appear in CommBldg (mixed-use buildings)
# to prevent double-counting in panel_tbl_all.
# Those parcels are fully represented in panel_tbl_com.
condo_pure <- condo_snap[in_comm_bldg == FALSE | is.na(in_comm_bldg)]
n_excl <- nrow(condo_snap) - nrow(condo_pure)
if (n_excl > 0)
  message("  Excluding ", n_excl, " condo units already in CommBldg (mixed-use).")

condo_pure[, join_key := 1L]
panel_condo_wide <- condo_pure[year_spine, on = "join_key", allow.cartesian = TRUE]
panel_condo_wide[, join_key := NULL]
setDT(panel_condo_wide)

# Attach AV history for condo unit parcels.
# av_hist parcel_ids are normalized to no-dash above.
# panel_condo_wide parcel_ids are already no-dash (set in 02_transfrm_condo.R).
# No further normalization needed — both sides match.
condo_pure_ids_nodash <- gsub("-", "", condo_pure$parcel_id)
av_condo <- av_hist[parcel_id %in% condo_pure_ids_nodash]
panel_condo_wide <- merge(panel_condo_wide, av_condo[, ..av_cols],
                           by = c("parcel_id","tax_yr"), all.x = TRUE)

panel_condo_wide[, prop_type         := "D"]
panel_condo_wide[, prop_class        := "Condo"]
panel_condo_wide[, prop_class_detail := condo_segment]

# =============================================================================
# SECTION 4 — Define column universes and add stubs
# =============================================================================

# Columns that only exist on residential rows
res_only_cols <- c(
  "current_zoning_3","hbu_as_if_vacant_desc","sq_ft_lot","nuisance_score",
  "mt_rainier","olympics","cascades","territorial","seattle_skyline",
  "puget_sound","lake_washington","seismic_hazard","landslide_hazard",
  "steep_slope_hazard","traffic_noise","airport_noise","power_lines",
  "other_nuisances","contamination","historic_site",
  "dist_to_public_km","dist_to_private_km","dist_to_lightrail_km",
  "unbuildable",
  "permits_last_1yr","permits_last_3yr","permits_last_5yr",
  "val_last_3yr","val_last_5yr","sqft_last_3yr","units_last_3yr",
  "any_newconst","years_since_newconst","log_val_last_3yr","log_sqft_last_3yr",
  # NWMLS SFH market vars
  "sea_pmedesfh_f","sea_sesfhf","sea_alesfhf"
)

# Columns that only exist on commercial rows
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
  "prop_class_comm","size_tier",
  # CoStar market vars
  "costar_vr_nro","costar_vr_mf","costar_ar_mf",
  "costar_vr_nro_lag1","costar_vr_nro_lag2",
  "costar_vr_mf_lag1","costar_vr_mf_lag2",
  "costar_ar_mf_lag1","costar_ar_mf_lag2",
  "costar_vr_nro_delta","costar_vr_mf_delta","costar_ar_mf_delta","costar_geo"
)

# Columns that only exist on condo rows
condo_only_cols <- c(
  "major","complex_type","complex_type_desc","complex_descr",
  "nbr_units","avg_unit_size","land_per_unit","log_nbr_units",
  "nbr_bldgs","nbr_stories_cplx","constr_class_cplx",
  "bldg_quality_cplx","bldg_quality_desc","condition_cplx",
  "yr_built_cplx","eff_yr_cplx","age_cplx","eff_age_cplx",
  "pcnt_complete","pcnt_with_view","project_location","project_appeal",
  "has_elevators","has_security","has_fireplace","laundry_type","laundry_desc",
  "apt_conversion","is_apt_conversion","is_mfte","condo_land_type",
  "unit_sqft","unit_floor","nbr_bedrooms","nbr_baths",
  "view_utilization","storage","parking_nbr_spaces",
  "effective_unit_sqft","log_unit_sqft_eff","log_avg_unit_size",
  "is_penthouse","is_highrise","is_large_complex",
  "unit_size_tier","amenity_score","complex_has_view","appeal_location_score",
  "condo_segment","in_comm_bldg",
  # NWMLS condo market vars (added by xx_nwmls_condo_to_panel.R)
  "nwmls_condo_med_price","nwmls_condo_psf",
  "nwmls_condo_active_list","nwmls_condo_dom",
  "nwmls_condo_med_price_lag1","nwmls_condo_med_price_lag2",
  "nwmls_condo_psf_lag1","nwmls_condo_psf_lag2",
  "nwmls_condo_active_list_lag1","nwmls_condo_active_list_lag2",
  "nwmls_condo_med_price_delta","nwmls_condo_psf_delta",
  "nwmls_condo_active_list_delta"
)

# Add stubs to each panel for columns from the other tracks
for (col in c(comm_only_cols, condo_only_cols)) panel_res       <- add_missing_cols(panel_res,       col)
for (col in c(res_only_cols,  condo_only_cols)) panel_com_wide  <- add_missing_cols(panel_com_wide,  col)
for (col in c(res_only_cols,  comm_only_cols))  panel_condo_wide <- add_missing_cols(panel_condo_wide, col)

# =============================================================================
# SECTION 4b — Join econ macro variables onto commercial and condo panels
# =============================================================================
# xx_econ_to_panel.R only joins econ cols onto panel_tbl (residential backbone).
# Commercial and condo rows would otherwise get NA-filled via add_missing_cols.
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
      econ_col_names <- setdiff(names(econ_wide), "tax_yr")
      for (track_dt_name in c("panel_com_wide", "panel_condo_wide")) {
        track_dt <- get(track_dt_name)
        for (col in intersect(econ_col_names, names(track_dt)))
          track_dt[, (col) := NULL]
        track_dt <- merge(track_dt, econ_wide, by = "tax_yr", all.x = TRUE)
        assign(track_dt_name, track_dt)
      }
      message("  Econ columns joined to commercial and condo panels: ",
              length(econ_col_names))
    }
  } else {
    message("  No econ forecast cache found — comm/condo panels will have NA econ cols.")
  }
}

# =============================================================================
# SECTION 5 — Stack into unified panel
# =============================================================================
all_cols <- Reduce(union, list(
  names(panel_res),
  names(panel_com_wide),
  names(panel_condo_wide)
))

panel_res        <- add_missing_cols(panel_res,        all_cols)
panel_com_wide   <- add_missing_cols(panel_com_wide,   all_cols)
panel_condo_wide <- add_missing_cols(panel_condo_wide, all_cols)

setcolorder(panel_res,        all_cols)
setcolorder(panel_com_wide,   all_cols)
setcolorder(panel_condo_wide, all_cols)

panel_tbl_all <- rbindlist(
  list(panel_res, panel_com_wide, panel_condo_wide),
  use.names = TRUE
)

setDT(panel_tbl_all)
setkeyv(panel_tbl_all, c("parcel_id","tax_yr"))

# =============================================================================
# SECTION 6 — Typed subsets
# =============================================================================
panel_tbl_res   <- panel_tbl_all[prop_type == "R"]
panel_tbl_com   <- panel_tbl_all[prop_type == "C"]
panel_tbl_condo <- panel_tbl_all[prop_type == "D"]

message("  panel_tbl_all rows:   ", nrow(panel_tbl_all))
message("    res:   ",  nrow(panel_tbl_res))
message("    com:   ",  nrow(panel_tbl_com))
message("    condo: ",  nrow(panel_tbl_condo))

# =============================================================================
# SECTION 7 — Partition assertions (res/com duplication guard)
# =============================================================================
# Defined in 00_init.R so this and xx_combine_res_comm_panel.R share one copy.
# Hard-stops on any res/com parcel_id overlap, and on res + com failing to
# reconcile to the Seattle levy-code population of EXTR_Parcel by PropType.
if (!exists("assert_res_com_partition", envir = .GlobalEnv))
  stop("assert_res_com_partition() not found — source 00_init.R first.")
assert_res_com_partition(panel_tbl_res, panel_tbl_com, panel_tbl_condo)

# =============================================================================
# SECTION 8 — Export
# =============================================================================
assign("panel_tbl_res",   panel_tbl_res,   envir = .GlobalEnv)
assign("panel_tbl_com",   panel_tbl_com,   envir = .GlobalEnv)
assign("panel_tbl_condo", panel_tbl_condo, envir = .GlobalEnv)
assign("panel_tbl_all",   panel_tbl_all,   envir = .GlobalEnv)

message("xx_combine_res_comm_condo_panel.R loaded.")
