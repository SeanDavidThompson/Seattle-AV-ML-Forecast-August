# =============================================================================
# 02_transfrm_comm.R
# Transform and finalize the commercial parcel dataset.
# Produces: parcel_comm_full  — mirrors parcel_res_full structure where possible,
#           adding commercial-specific columns and a prop_class identifier.
# Requires: comm_parcel_full (from 01_import_comm.R)
#           parcel_res_full  (from 02_transfrm.R) for shared facet columns
#
# Fixes applied here (mirroring 02_transfrm.R where applicable):
#
#   1. Exempt parcel exclusion
#      Commercial parcels with tax_status != "T" (Exempt/Operating) or
#      taxable_value_reason in (EX/FS/CU/HP/NP/OP) are flagged model_com=0.
#      They remain in the panel for AV history continuity but are excluded
#      from model training.
#
#   2. nuisance_score decomposition
#      airport_noise is a raw DNL dB zone (~55-75); summing it with ordinal/
#      binary fields makes it dominate the composite. Rebuild nuisance_score
#      from traffic_noise + power_lines + other_nuisances only.
#      airport_noise kept as a standalone feature.
#
#   3. New Parcel features from KCA documentation
#      access, water_system, sewer_system, street_surface, topography,
#      pcnt_unusable, waterfront_location/footage, adjacent_golf_fairway,
#      adjacent_greenbelt, lot_depth_factor, erosion_hazard, critical_drainage,
#      hundred_yr_flood_plain, stream, wetland, species_of_concern, ngpe.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(janitor)
  library(dplyr)
  library(stringr)
  library(here)
})

if (!exists("comm_parcel_full", envir = .GlobalEnv))
  stop("Run 01_import_comm.R before 02_transfrm_comm.R")

comm <- copy(comm_parcel_full)

# ---- Predominant use lookup -------------------------------------------------
# King County commercial use codes (partial; covers the most common)
predominant_use_lkp <- data.table(
  predominant_use = c(
    300L, 304L, 309L, 311L, 326L, 341L, 344L, 348L, 349L, 350L,
    352L, 353L, 365L, 386L, 406L, 407L, 419L, 426L, 453L, 459L,
    470L, 472L, 494L, 528L, 531L, 551L, 706L, 820L, 830L, 840L,
    860L
  ),
  predominant_use_desc = c(
    "Office Building", "Medical/Dental Office", "Office Park", "Recreation/Club",
    "Gas Station", "Fast Food", "Restaurant/Lounge", "Car Wash", "Auto Dealer",
    "Shopping Center/Mall", "Retail Store", "Supermarket/Grocery",
    "Mini-Storage/Warehouse", "Hotel/Motel", "Warehouse",
    "Manufacturing/Industrial", "Nursing Home", "Hospital", "Church/Religious",
    "School/University", "Apartments (5+ units)", "Senior Housing",
    "Community Center", "Garage/Service Repair", "Car Lot/Dealer",
    "Parking Garage", "Mixed Use Commercial", "Park/Open Space",
    "Golf Course", "Cemetery", "Utility/Infrastructure"
  )
)

comm <- merge(comm, predominant_use_lkp, by = "predominant_use", all.x = TRUE)
comm[is.na(predominant_use_desc),
     predominant_use_desc := paste0("Use_", predominant_use)]

# ---- Broad property class ---------------------------------------------------
# Maps KCA use codes to broad CoStar-style property types
comm[, prop_class_comm := fcase(
  predominant_use >= 300 & predominant_use < 340, "Office",
  predominant_use >= 340 & predominant_use < 400, "Retail",
  predominant_use >= 400 & predominant_use < 500, "Industrial",
  predominant_use >= 460 & predominant_use < 480, "Multifamily",
  predominant_use >= 500 & predominant_use < 700, "Institutional",
  predominant_use >= 700 & predominant_use < 710, "Mixed Use",
  predominant_use >= 800,                          "Land/Special",
  default = "Other Commercial"
)]

# ---- Size tiers ------------------------------------------------------------
comm[, size_tier := fcase(
  total_gross_sqft <  5000,              "Small (<5k sqft)",
  total_gross_sqft <  20000,             "Medium (5-20k sqft)",
  total_gross_sqft <  100000,            "Large (20-100k sqft)",
  total_gross_sqft >= 100000,            "Very Large (100k+ sqft)",
  default = NA_character_
)]

# ---- Log transforms --------------------------------------------------------
comm[total_gross_sqft > 0,  log_gross_sqft := log(total_gross_sqft)]
comm[total_net_sqft   > 0,  log_net_sqft   := log(total_net_sqft)]
comm[total_gross_sqft == 0, log_gross_sqft := 0]
comm[total_net_sqft   == 0, log_net_sqft   := 0]

# ---- Unified prop_type flag (mirrors residential convention) ---------------
# parcel_res_full uses prop_type = "R"
# commercial will be "C" — consistent with KCA raw data
comm[, prop_type := "C"]

# ---- Rename to snake_case where needed (already done in import) -----------
setDT(comm)

# =============================================================================
# FIX: Exempt parcel exclusion
# =============================================================================
# model_com: 1 = include in training/forecasting, 0 = exclude.
comm[, model_com := 1L]

if ("tax_status" %in% names(comm)) {
  n_exempt <- sum(comm$tax_status != "T", na.rm = TRUE)
  message("    tax_status != 'T': ", n_exempt,
          " commercial parcels (Exempt/Operating) — setting model_com=0")
  comm[tax_status != "T", model_com := 0L]
}

if ("taxable_value_reason" %in% names(comm)) {
  exempt_reasons <- c("EX", "FS", "CU", "HP", "NP", "OP")
  n_reason <- sum(comm$taxable_value_reason %in% exempt_reasons, na.rm = TRUE)
  message("    taxable_value_reason exempt: ", n_reason,
          " commercial parcels — setting model_com=0")
  comm[taxable_value_reason %in% exempt_reasons, model_com := 0L]
}

message("  Commercial parcels excluded from training (model_com=0): ",
        sum(comm$model_com == 0L, na.rm = TRUE))

# =============================================================================
# FIX: nuisance_score decomposition
# =============================================================================
# airport_noise is a raw DNL dB zone (~55-75). Keep as standalone feature.
# Rebuild nuisance_score from ordinal/binary components only.

if ("airport_noise" %in% names(comm))
  comm[, airport_noise := as.integer(airport_noise)]

yn_to_int <- function(x) as.integer(toupper(trimws(x)) %in% c("Y", "1", "YES"))

tn  <- if ("traffic_noise"   %in% names(comm)) as.integer(comm$traffic_noise)   else 0L
pl  <- if ("power_lines"     %in% names(comm)) yn_to_int(comm$power_lines)      else 0L
on_ <- if ("other_nuisances" %in% names(comm)) yn_to_int(comm$other_nuisances)  else 0L

comm[, nuisance_score := tn + pl + on_]
comm[is.na(nuisance_score), nuisance_score := 0L]

message("  nuisance_score rebuilt for commercial (traffic + power + other)")

# =============================================================================
# FIX: New Parcel features from KCA documentation
# =============================================================================
# These fields are on EXTR_Parcel and join onto all property types.
# Clean/type them here if 01_import_comm.R read them in.

if ("access"          %in% names(comm)) comm[, access          := as.integer(access)]
if ("water_system"    %in% names(comm)) comm[, water_system    := as.integer(water_system)]
if ("sewer_system"    %in% names(comm)) comm[, sewer_system    := as.integer(sewer_system)]
if ("street_surface"  %in% names(comm)) comm[, street_surface  := as.integer(street_surface)]
if ("topography"      %in% names(comm)) comm[, topography      := as.integer(topography == 1L | topography == "1")]
if ("pcnt_unusable"   %in% names(comm)) comm[, pcnt_unusable   := as.numeric(pcnt_unusable)]
if ("lot_depth_factor" %in% names(comm)) comm[, lot_depth_factor := as.numeric(lot_depth_factor)]

if ("waterfront_location" %in% names(comm)) {
  comm[, waterfront_location := as.integer(waterfront_location)]
  comm[, is_waterfront := as.integer(!is.na(waterfront_location) & waterfront_location > 0L)]
}
if ("waterfront_footage" %in% names(comm))
  comm[, waterfront_footage := as.numeric(waterfront_footage)]

for (col in c("adjacent_golf_fairway", "adjacent_greenbelt", "erosion_hazard",
              "critical_drainage", "hundred_yr_flood_plain", "stream",
              "wetland", "species_of_concern",
              "native_growth_protection_easement")) {
  if (col %in% names(comm)) {
    new_name <- if (col == "native_growth_protection_easement") "ngpe" else col
    comm[, (new_name) := yn_to_int(get(col))]
    if (new_name != col) comm[, (col) := NULL]
  }
}

# ---- Export as parcel_comm_full --------------------------------------------
assign("parcel_comm_full", comm, envir = .GlobalEnv)

message("  parcel_comm_full rows: ", nrow(parcel_comm_full),
        " | prop classes: ")
print(parcel_comm_full[, .N, by = prop_class_comm][order(-N)])

message("02_transfrm_comm.R loaded.")
