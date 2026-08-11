# =============================================================================
# 01_import_comm.R
# Import and clean raw commercial building tables from King County Assessor.
# Produces: comm_bldg, comm_section, comm_feature (cleaned data.tables)
# These parallel the residential import in 01_import.R.
#
# Note on exempt parcels:
#   EXTR_RealPropAccount has tax_status (T=Taxable, X=Exempt, O=Operating)
#   and taxable_value_reason (EX/FS/CU/HP/NP/OP = various exemptions).
#   Exempt commercial parcels (churches, schools, govt, non-profits) have
#   AV set by a different process and must not enter model training.
#   The filter is applied in 02_transfrm_comm.R after the AV roll merge,
#   mirroring the residential treatment in 02_transfrm.R.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(janitor)
  library(dplyr)
  library(stringr)
  library(here)
})

# ---- Paths ------------------------------------------------------------------
# kca_date_data_extracted must be set in .GlobalEnv by run_main_ml()
kca_date  <- get("kca_date_data_extracted", envir = .GlobalEnv)
kca_root  <- here::here("data", "kca", kca_date)

comm_bldg_path    <- file.path(kca_root, "EXTR_CommBldg.csv")
comm_sect_path    <- file.path(kca_root, "EXTR_CommBldgSection.csv")
comm_feat_path    <- file.path(kca_root, "EXTR_CommBldgFeature.csv")

# ---- Helper -----------------------------------------------------------------
# KCA extracts are Windows-1252/Latin-1 encoded; encoding="Latin-1" converts
# to valid UTF-8 on read (matches the condo import). The iconv sweep scrubs
# any stray bytes that still slip through, so downstream trimws()/regex calls
# never hit "invalid UTF-8" errors.
read_kca <- function(path, ...) {
  if (!file.exists(path)) stop("Missing file: ", path)
  dt <- fread(file = path, na.strings = c("", "NA", " "),
              encoding = "Latin-1", ...)
  setDT(dt)
  chr_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (cc in chr_cols)
    set(dt, j = cc,
        value = iconv(dt[[cc]], from = "UTF-8", to = "UTF-8", sub = ""))
  clean_names(dt)
}

# =============================================================================
# 1. CommBldg  (one row per building)
# =============================================================================
message("  Reading EXTR_CommBldg ...")
comm_bldg_raw <- read_kca(comm_bldg_path)

comm_bldg <- comm_bldg_raw[, .(
  major              = str_pad(trimws(major),   6, "left", "0"),
  minor              = str_pad(trimws(minor),   4, "left", "0"),
  bldg_nbr           = as.integer(trimws(bldg_nbr)),
  nbr_bldgs          = as.integer(trimws(nbr_bldgs)),
  nbr_stories        = as.numeric(trimws(nbr_stories)),
  predominant_use    = as.integer(trimws(predominant_use)),
  shape              = as.integer(trimws(shape)),
  constr_class       = as.integer(trimws(constr_class)),
  bldg_quality       = as.integer(trimws(bldg_quality)),
  bldg_descr         = trimws(bldg_descr),
  bldg_gross_sqft    = as.numeric(trimws(bldg_gross_sq_ft)),
  bldg_net_sqft      = as.numeric(trimws(bldg_net_sq_ft)),
  yr_built           = as.integer(trimws(yr_built)),
  eff_yr             = as.integer(trimws(eff_yr)),
  pcnt_complete      = as.numeric(trimws(pcnt_complete)),
  heating_system     = as.integer(trimws(heating_system)),
  sprinklers         = trimws(sprinklers),
  elevators          = trimws(elevators)
)]

# Derived
comm_bldg[, parcel_id := paste0(major, "-", minor)]
comm_bldg[, age            := pmax(0, kca_date |> substr(1,4) |> as.integer() - yr_built,  na.rm = FALSE)]
comm_bldg[, eff_age        := pmax(0, kca_date |> substr(1,4) |> as.integer() - eff_yr,    na.rm = FALSE)]
comm_bldg[, has_sprinklers := as.integer(sprinklers == "Y")]
comm_bldg[, has_elevators  := as.integer(elevators  == "Y")]

# Quality labels (King County scale 1-9, low=1, high=9 for commercial)
# https://www.kingcounty.gov/assessor/forms/excelformats.aspx
comm_bldg[, bldg_quality_desc := fcase(
  bldg_quality == 1, "Very Low",
  bldg_quality == 2, "Low",
  bldg_quality == 3, "Fair",
  bldg_quality == 4, "Average",
  bldg_quality == 5, "Good",
  bldg_quality == 6, "Very Good",
  bldg_quality == 7, "Excellent",
  bldg_quality == 8, "Luxury",
  bldg_quality == 9, "Landmark",
  default = NA_character_
)]

# Construction class labels
comm_bldg[, constr_class_desc := fcase(
  constr_class == 0, "Unknown",
  constr_class == 1, "Steel",
  constr_class == 2, "Reinforced Concrete",
  constr_class == 3, "Masonry",
  constr_class == 4, "Wood/Frame",
  constr_class == 5, "Pre-Fabricated",
  default = NA_character_
)]

message("    comm_bldg rows: ", nrow(comm_bldg),
        " | unique parcels: ", uniqueN(comm_bldg$parcel_id))

# =============================================================================
# 2. CommBldgSection  (one row per section/use within a building)
# =============================================================================
message("  Reading EXTR_CommBldgSection ...")
comm_sect_raw <- read_kca(comm_sect_path)

comm_section <- comm_sect_raw[, .(
  major        = str_pad(trimws(major),   6, "left", "0"),
  minor        = str_pad(trimws(minor),   4, "left", "0"),
  bldg_nbr     = as.integer(trimws(bldg_nbr)),
  section_nbr  = as.integer(trimws(section_nbr)),
  section_use  = as.integer(trimws(section_use)),
  nbr_stories  = as.numeric(trimws(nbr_stories)),
  story_height = as.numeric(trimws(story_height)),
  gross_sqft   = as.numeric(trimws(gross_sq_ft)),
  net_sqft     = as.numeric(trimws(net_sq_ft)),
  section_descr = trimws(section_descr)
)]
comm_section[, parcel_id := paste0(major, "-", minor)]

message("    comm_section rows: ", nrow(comm_section),
        " | unique parcels: ", uniqueN(comm_section$parcel_id))

# =============================================================================
# 3. CommBldgFeature  (one row per amenity feature)
# =============================================================================
message("  Reading EXTR_CommBldgFeature ...")
comm_feat_raw <- read_kca(comm_feat_path)

comm_feature <- comm_feat_raw[, .(
  major        = str_pad(trimws(major), 6, "left", "0"),
  minor        = str_pad(trimws(minor), 4, "left", "0"),
  bldg_nbr     = as.integer(trimws(bldg_nbr)),
  feature_type = as.integer(trimws(feature_type)),
  gross_sqft   = as.numeric(trimws(gross_sq_ft)),
  net_sqft     = as.numeric(trimws(net_sq_ft))
)]
comm_feature[, parcel_id := paste0(major, "-", minor)]

# Feature type labels (KC codes)
comm_feature[, feature_type_desc := fcase(
  feature_type == 751, "Parking Garage",
  feature_type == 760, "Surface Parking",
  feature_type == 761, "Canopy/Carport",
  feature_type == 763, "Other Feature",
  !is.na(feature_type), paste0("Feature_", feature_type),
  default = NA_character_
)]

message("    comm_feature rows: ", nrow(comm_feature),
        " | unique parcels: ", uniqueN(comm_feature$parcel_id))

# =============================================================================
# 4. Collapse to parcel level for merging
# =============================================================================
message("  Building parcel-level commercial summary ...")

## 4a. From comm_bldg: aggregate per parcel (max/sum across buildings)
comm_bldg_parcel <- comm_bldg[, .(
  n_comm_bldgs         = .N,
  total_gross_sqft     = sum(bldg_gross_sqft,  na.rm = TRUE),
  total_net_sqft       = sum(bldg_net_sqft,    na.rm = TRUE),
  max_nbr_stories      = max(nbr_stories,       na.rm = TRUE),
  avg_bldg_quality     = mean(bldg_quality,     na.rm = TRUE),
  max_bldg_quality     = max(bldg_quality,      na.rm = TRUE),
  predominant_use      = first(predominant_use), # primary bldg
  constr_class         = first(constr_class),
  yr_built_first_comm  = min(yr_built,           na.rm = TRUE),
  yr_built_last_comm   = max(yr_built,           na.rm = TRUE),
  eff_yr_comm          = max(eff_yr,             na.rm = TRUE),
  avg_age_comm         = mean(age,               na.rm = TRUE),
  min_eff_age_comm     = min(eff_age,            na.rm = TRUE),
  has_sprinklers       = max(has_sprinklers,     na.rm = TRUE),
  has_elevators        = max(has_elevators,      na.rm = TRUE),
  pcnt_complete_comm   = mean(pcnt_complete,     na.rm = TRUE)
), by = parcel_id]

## 4b. From comm_section: section-use diversity and sqft by use category
# Broad use buckets (KCA predominant use codes)
# 300s = office/professional, 340s-360s = retail, 400s = industrial,
# 500s = institutional/special, 700s = parking, 800s+ = other
comm_section[, use_bucket := fcase(
  section_use >= 300 & section_use < 340, "office",
  section_use >= 340 & section_use < 400, "retail",
  section_use >= 400 & section_use < 500, "industrial",
  section_use >= 500 & section_use < 700, "institutional",
  section_use >= 700 & section_use < 800, "parking",
  section_use >= 800,                      "other",
  default = "unknown"
)]

comm_sect_parcel <- comm_section[, .(
  n_sections           = .N,
  n_distinct_uses      = uniqueN(section_use),
  sect_gross_sqft      = sum(gross_sqft, na.rm = TRUE),
  sect_net_sqft        = sum(net_sqft,   na.rm = TRUE),
  office_sqft          = sum(gross_sqft[use_bucket == "office"],       na.rm = TRUE),
  retail_sqft          = sum(gross_sqft[use_bucket == "retail"],       na.rm = TRUE),
  industrial_sqft      = sum(gross_sqft[use_bucket == "industrial"],   na.rm = TRUE),
  institutional_sqft   = sum(gross_sqft[use_bucket == "institutional"],na.rm = TRUE),
  parking_sqft         = sum(gross_sqft[use_bucket == "parking"],      na.rm = TRUE),
  avg_story_height     = weighted.mean(story_height, gross_sqft, na.rm = TRUE)
), by = parcel_id]

## 4c. From comm_feature: parking/canopy presence and sqft
comm_feat_parcel <- comm_feature[, .(
  parking_garage_sqft  = sum(gross_sqft[feature_type == 751], na.rm = TRUE),
  surface_parking_sqft = sum(gross_sqft[feature_type == 760], na.rm = TRUE),
  canopy_sqft          = sum(gross_sqft[feature_type == 761], na.rm = TRUE),
  other_feature_sqft   = sum(gross_sqft[feature_type == 763], na.rm = TRUE),
  total_feature_sqft   = sum(gross_sqft, na.rm = TRUE)
), by = parcel_id]

## 4d. Merge all three parcel summaries
comm_parcel_full <- comm_bldg_parcel |>
  merge(comm_sect_parcel, by = "parcel_id", all.x = TRUE) |>
  merge(comm_feat_parcel, by = "parcel_id", all.x = TRUE)

# Fill NAs from optional tables with 0
feat_cols <- c("parking_garage_sqft","surface_parking_sqft",
               "canopy_sqft","other_feature_sqft","total_feature_sqft",
               "n_sections","n_distinct_uses","sect_gross_sqft","sect_net_sqft",
               "office_sqft","retail_sqft","industrial_sqft",
               "institutional_sqft","parking_sqft","avg_story_height")
for (col in feat_cols) {
  if (col %in% names(comm_parcel_full)) {
    comm_parcel_full[is.na(get(col)), (col) := 0]
  }
}

# Use-mix ratios (share of gross sqft by category)
comm_parcel_full[sect_gross_sqft > 0, `:=`(
  office_pct      = office_sqft      / sect_gross_sqft,
  retail_pct      = retail_sqft      / sect_gross_sqft,
  industrial_pct  = industrial_sqft  / sect_gross_sqft,
  parking_pct     = parking_sqft     / sect_gross_sqft
)]
comm_parcel_full[sect_gross_sqft == 0, `:=`(
  office_pct = 0, retail_pct = 0, industrial_pct = 0, parking_pct = 0
)]

# Total parking sqft (garage + surface + canopy)
comm_parcel_full[, total_parking_sqft := parking_garage_sqft +
                   surface_parking_sqft + canopy_sqft]

# Efficiency ratio
comm_parcel_full[total_gross_sqft > 0,
  net_to_gross_ratio := total_net_sqft / total_gross_sqft]
comm_parcel_full[total_gross_sqft == 0, net_to_gross_ratio := NA_real_]

setDT(comm_parcel_full)
message("    comm_parcel_full rows: ", nrow(comm_parcel_full))

# Assign to GlobalEnv
assign("comm_bldg",        comm_bldg,        envir = .GlobalEnv)
assign("comm_section",     comm_section,     envir = .GlobalEnv)
assign("comm_feature",     comm_feature,     envir = .GlobalEnv)
assign("comm_parcel_full", comm_parcel_full, envir = .GlobalEnv)

message("01_import_comm.R loaded.")
