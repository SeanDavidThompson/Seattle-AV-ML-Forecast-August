# xx_combine_parcel_history_changes.R  ---------------------------------
# Build a full parcel-year panel, apply facet changes, and prep for retrofitting.

stopifnot(
  exists("attr_map_noupdate"),
  exists("coerce_to_target")
)

# -------------------------------------------------------------------
# 0) Load inputs (prefer in-memory; fallback to cache if needed)
# -------------------------------------------------------------------
cache_dir <- here::here("data", "cache")
cache_path <- function(name) file.path(cache_dir, paste0(name, ".rds"))

load_or_cache <- function(obj_name) {
  if (exists(obj_name, envir = .GlobalEnv)) {
    get(obj_name, envir = .GlobalEnv)
  } else {
    p <- cache_path(obj_name)
    if (!file.exists(p)) {
      stop("Missing object and cache: ", obj_name,
           "\nRun main_ml(replicate=TRUE) once.")
    }
    message("✅ loaded from cache: ", obj_name)
    readRDS(p)
  }
}

parcel_res_full   <- load_or_cache("parcel_res_full")
av_history_cln    <- load_or_cache("av_history_cln")
changes_long_tbl  <- load_or_cache("changes_long_tbl")

# Convert to data.table
parcel_dt  <- as.data.table(parcel_res_full)
av_dt      <- as.data.table(av_history_cln)
chg_dt     <- as.data.table(changes_long_tbl)

stopifnot(
  "parcel_id" %in% names(parcel_dt),
  "parcel_id" %in% names(av_dt),
  "parcel_id" %in% names(chg_dt),
  "tax_yr"    %in% names(av_dt)
)

# -------------------------------------------------------------------
# 1) Full parcel x year grid
# -------------------------------------------------------------------
# TY2026 = assessed Jan 1 2025, payable 2026 — the most recent certified year.
# In KCA convention tax_yr is the billing/payable year, not the assessment year.
years <- 2000:2026

panel_base <- CJ(
  parcel_id = unique(parcel_dt$parcel_id),
  tax_yr    = years,
  unique    = TRUE
)
setkey(panel_base, parcel_id, tax_yr)

# -------------------------------------------------------------------
# 2) Join AV history (robust total_assessed)
# -------------------------------------------------------------------
av_dt[, tax_yr := as.integer(tax_yr)]
# Normalize parcel_id format: av_history_cln uses "XXXXXX-XXXX" (dash) but
# parcel_dt (and panel_base) uses "XXXXXXXXXX" (no dash). Strip dashes so
# the keyed join on line below matches correctly.
av_dt[, parcel_id := gsub("-", "", parcel_id)]

av_keep <- av_dt[, .(
  parcel_id, tax_yr,
  appr_land_val,
  appr_imps_val,
  total_assessed = fifelse(
    is.na(appr_land_val) & is.na(appr_imps_val), NA_real_,
    fifelse(is.na(appr_land_val), appr_imps_val,
            fifelse(is.na(appr_imps_val), appr_land_val,
                    appr_land_val + appr_imps_val))
  )
)]
setkey(av_keep, parcel_id, tax_yr)

panel <- av_keep[panel_base]   # left join onto full grid

# -------------------------------------------------------------------
# 3) Prep changes table (mapped attrs only; exclude baseline-only area)
# -------------------------------------------------------------------
chg_dt <- chg_dt[attribute %chin% attr_map_noupdate$attribute]
chg_dt <- merge(chg_dt, attr_map_noupdate, by = "attribute", all.x = TRUE)

if (any(is.na(chg_dt$col_name))) {
  bad <- unique(chg_dt[is.na(col_name), attribute])
  stop("Unmapped change attributes found: ", paste(bad, collapse=", "))
}

chg_dt[, eff_yr := lubridate::year(update_date)]
chg_dt <- chg_dt[eff_yr >= min(years) & eff_yr <= max(years)]

# Coerce attribute_value to baseline facet type
cols_in_parcel <- names(parcel_dt)

for (i in seq_len(nrow(attr_map_noupdate))) {
  a  <- attr_map_noupdate$attribute[i]
  cn <- attr_map_noupdate$col_name[i]
  if (!cn %chin% cols_in_parcel) next
  
  chg_dt[attribute == a,
         attribute_value := coerce_to_target(attribute_value, parcel_dt[[cn]])]
}

# -------------------------------------------------------------------
# 4) Baseline facets (area + mapped facets + baseline distance cols)
# -------------------------------------------------------------------
facet_cols <- unique(attr_map_noupdate$col_name)
facet_cols <- facet_cols[facet_cols %chin% names(parcel_dt)]

dist_cols <- c("dist_to_public_km", "dist_to_private_km", "dist_to_lightrail_km")
dist_cols <- dist_cols[dist_cols %chin% names(parcel_dt)]

baseline_facets <- parcel_dt[, c("parcel_id", "area", facet_cols, dist_cols), with = FALSE]
setkey(baseline_facets, parcel_id)

panel <- baseline_facets[panel]   # left join baseline values

setkey(panel, parcel_id, tax_yr)
setorder(panel, parcel_id, tax_yr)

# -------------------------------------------------------------------
# 5) Fast apply_facet_changes (in-place update join + LOCF)
# -------------------------------------------------------------------
apply_facet_changes_fast <- function(panel_dt, chg_long, facet_name){
  
  subc <- chg_long[col_name == facet_name,
                   .(parcel_id, eff_yr, new_val = attribute_value)]
  if (nrow(subc) == 0) return(panel_dt)
  
  setorder(subc, parcel_id, eff_yr)
  subc <- subc[, .SD[.N], by = .(parcel_id, eff_yr)]  # last within year
  
  panel_dt[subc, (facet_name) := i.new_val,
           on = .(parcel_id, tax_yr = eff_yr)]
  
  panel_dt[, (facet_name) := zoo::na.locf(get(facet_name), na.rm = FALSE),
           by = parcel_id]
  
  panel_dt
}

for (cn in facet_cols) {
  message("Applying changes for: ", cn)
  panel <- apply_facet_changes_fast(panel, chg_dt, cn)
}

# -------------------------------------------------------------------
# 6) Final clean up (logs + numeric cast)
# -------------------------------------------------------------------
panel[appr_land_val <= 0, appr_land_val := NA_real_]
panel[appr_imps_val <= 0, appr_imps_val := NA_real_]
panel[total_assessed <= 0, total_assessed := NA_real_]

panel[, log_appr_land_val   := log(appr_land_val)]
panel[, log_appr_imps_val   := log(appr_imps_val)]
panel[, log_total_assessed  := log(total_assessed)]

for (v in c("log_appr_land_val","log_appr_imps_val","log_total_assessed")) {
  panel[is.infinite(get(v)) | is.nan(get(v)), (v) := NA_real_]
}

for (cn in dist_cols) {
  panel[, (cn) := as.numeric(get(cn))]
}

# Memory cleanup — drop all intermediates including GlobalEnv source objects.
# changes_long_tbl and av_history_cln have now been fully consumed; keeping them
# alive through xx_combine_res_comm_condo_panel.R wastes ~775 MB at peak.
rm(av_keep, baseline_facets, chg_dt, panel_base)
for (.nm in c("changes_long_tbl", "av_history_cln",
              "av_history", "av_history_raw")) {
  if (exists(.nm, envir = .GlobalEnv))
    rm(list = .nm, envir = .GlobalEnv)
}
rm(.nm)
gc()

message("xx_combine_parcel_history_changes.R loaded: panel ready for retrofitting.")
