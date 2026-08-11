# =============================================================================
# 02_transfrm_condo.R
# Transform and finalise the condo parcel dataset.
# Produces: parcel_condo_full — mirrors parcel_res_full / parcel_comm_full
# structure, adding condo-specific columns and prop_type = "D".
#
# Requires: condo_parcel_full (from 01_import_condo.R)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(here)
})

if (!exists("condo_parcel_full", envir = .GlobalEnv))
  stop("Run 01_import_condo.R before 02_transfrm_condo.R")

condo <- copy(condo_parcel_full)
setDT(condo)

current_year <- as.integer(substr(
  get("kca_date_data_extracted", envir = .GlobalEnv), 1, 4
))

# ---- Exclude timeshares from modelling ------------------------------------
n_timeshare <- sum(condo$complex_type == 3L, na.rm = TRUE)
if (n_timeshare > 0) {
  message("  Excluding ", n_timeshare, " timeshare units (ComplexType=3) from model panel.")
  condo <- condo[complex_type != 3L | is.na(complex_type)]
}

# ---- Condo segment labels --------------------------------------------------
# For the model, collapse into broad segments the NWMLS condo variables
# are likely aggregated to
condo[, condo_segment := fcase(
  complex_type == 1L,  "Standard Condo",
  complex_type == 4L,  "MFTE Condo",
  complex_type == 2L,  "Co-op",
  complex_type == 6L,  "Leasehold",
  complex_type == 10L, "Condo Hotel",
  default = "Other Condo"
)]

# ---- Size tiers (unit level, prefer unit_sqft; fall back to avg_unit_size)
condo[, effective_unit_sqft := fcase(
  !is.na(unit_sqft) & unit_sqft > 0, unit_sqft,
  !is.na(avg_unit_size) & avg_unit_size > 0, avg_unit_size,
  default = NA_real_
)]

condo[, unit_size_tier := fcase(
  effective_unit_sqft < 500,                           "Studio (<500)",
  effective_unit_sqft >= 500  & effective_unit_sqft < 800,  "1BR (500-800)",
  effective_unit_sqft >= 800  & effective_unit_sqft < 1200, "2BR (800-1200)",
  effective_unit_sqft >= 1200 & effective_unit_sqft < 1800, "3BR (1200-1800)",
  effective_unit_sqft >= 1800,                         "Large (1800+)",
  default = NA_character_
)]

# ---- Log transforms --------------------------------------------------------
condo[effective_unit_sqft > 0, log_unit_sqft_eff := log(effective_unit_sqft)]
condo[effective_unit_sqft <= 0 | is.na(effective_unit_sqft), log_unit_sqft_eff := NA_real_]

condo[nbr_units > 0, log_nbr_units := log(nbr_units)]
condo[nbr_units <= 0 | is.na(nbr_units), log_nbr_units := NA_real_]

# ---- Amenity score (simple additive) ---------------------------------------
# Each binary amenity contributes 1 point
condo[, amenity_score := (as.integer(has_elevators)  +
                           as.integer(has_security)   +
                           as.integer(has_fireplace)  +
                           as.integer(laundry_type >= 1L))]

# ---- View flag (pct_with_view > 0 at complex level) -----------------------
condo[, complex_has_view := as.integer(!is.na(pcnt_with_view) & pcnt_with_view > 0)]

# ---- Appeal / location composite (1-5 scale each; higher = better) --------
condo[, appeal_location_score := (as.numeric(project_appeal) +
                                   as.numeric(project_location))]

# ---- High-density flag (50+ units in complex) ------------------------------
condo[, is_large_complex := as.integer(!is.na(nbr_units) & nbr_units >= 50L)]

# ---- Prop type identifiers (final) ----------------------------------------
condo[, prop_type         := "D"]
condo[, prop_class        := "Condo"]
condo[, prop_class_detail := condo_segment]

# ---- Ensure parcel_id is correct ------------------------------------------
if (!"parcel_id" %in% names(condo))
  condo[, parcel_id := paste0(major, "-", minor)]

setDT(condo)
setkeyv(condo, c("parcel_id"))

message("  parcel_condo_full rows: ",  nrow(condo))
message("  Unique complexes (major): ", uniqueN(condo$major))
message("  Condo segments:")
print(condo[, .N, by = condo_segment][order(-N)])

# ---- model_condo flag: 1 = include in training/forecasting, 0 = exclude ----
# Timeshares (complex_type=3) are already dropped above; this flag provides
# a consistent exclusion hook for future use (mirrors model_res / model_com).
condo[, model_condo := 1L]
message("  Condo units flagged model_condo=1: ", sum(condo$model_condo == 1L, na.rm=TRUE))

assign("parcel_condo_full", condo, envir = .GlobalEnv)
message("02_transfrm_condo.R loaded.")
