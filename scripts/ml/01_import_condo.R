# =============================================================================
# 01_import_condo.R
# Import and clean KCA condo tables.
#
# KCA CONDO TABLE STRUCTURE:
#   EXTR_CondoComplex.csv  — one row per complex (keyed on Major only)
#                            5,248 complexes | 151,105 total units
#                            Complex-level: NbrUnits, AvgUnitSize, BldgQuality,
#                            ComplexType, amenities, YrBuilt, EffYr, views
#
#   EXTR_CondoUnit.csv     — one row per unit parcel (Major-Minor)
#                            These are the individually assessed parcels.
#                            Columns: unit size, floor, view, bedrooms, etc.
#
# KEY STRUCTURAL FACTS:
#   - CondoComplex joins to unit parcels via Major ONLY (no Minor)
#   - Each unit has its own Major-Minor parcel_id in the AV roll
#   - Units are NOT in EXTR_ResBldg or EXTR_CommBldg
#   - 16.3% of condo Majors appear in CommBldg — mixed-use buildings where
#     ground-floor commercial is assessed separately; those parcels are
#     already in parcel_comm_full and are flagged here to prevent double-count
#   - NWMLS condo variables match at the unit (Major-Minor) parcel level
#
# OUTPUTS (assigned to .GlobalEnv):
#   condo_complex_cln   — cleaned complex-level table (5,248 rows)
#   condo_unit_cln      — cleaned unit-level table (one row per unit parcel)
#   condo_parcel_full   — unit parcels enriched with complex attributes,
#                         ready for panel assembly (mirrors parcel_res_full
#                         and parcel_comm_full structure)
#   condo_mixed_use_majors — Major codes that also appear in CommBldg
#                            (use to flag overlap in downstream join)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(janitor)
  library(stringr)
  library(here)
})

kca_date  <- get("kca_date_data_extracted", envir = .GlobalEnv)
kca_root  <- here::here("data", "kca", kca_date)

complex_path <- file.path(kca_root, "EXTR_CondoComplex.csv")
unit_path    <- file.path(kca_root, "EXTR_CondoUnit.csv")

# Helper — always use file= to avoid UNC shell issues
read_kca <- function(path, ...) {
  if (!file.exists(path)) stop("Missing file: ", path)
  dt <- data.table::fread(file = path, na.strings = c("", "NA", " "),
                           encoding = "Latin-1", ...)
  janitor::clean_names(dt)
}

current_year <- as.integer(substr(kca_date, 1, 4))

# =============================================================================
# 1.  EXTR_CondoComplex  (complex-level, Major only)
# =============================================================================
message("  Reading EXTR_CondoComplex ...")
cplx_raw <- read_kca(complex_path)

condo_complex_cln <- cplx_raw[, .(
  major             = str_pad(trimws(major), 6, "left", "0"),

  # Identity
  complex_type      = as.integer(trimws(complex_type)),
  complex_descr     = trimws(complex_descr),
  apt_conversion    = trimws(apt_conversion) == "Y",
  condo_land_type   = as.integer(trimws(condo_land_type)),

  # Scale
  nbr_bldgs         = as.integer(trimws(nbr_bldgs)),
  nbr_stories_cplx  = as.integer(trimws(nbr_stories)),
  nbr_units         = as.integer(trimws(nbr_units)),
  avg_unit_size     = as.numeric(trimws(avg_unit_size)),
  land_per_unit     = as.numeric(trimws(land_per_unit)),

  # Quality / condition
  constr_class_cplx = as.integer(trimws(constr_class)),
  bldg_quality_cplx = as.integer(trimws(bldg_quality)),
  condition_cplx    = as.integer(trimws(condition)),
  pcnt_complete     = as.numeric(trimws(pcnt_complete)),

  # Age
  yr_built_cplx     = as.integer(trimws(yr_built)),
  eff_yr_cplx       = as.integer(trimws(eff_yr)),

  # Views / appeal
  project_location  = as.integer(trimws(project_location)),
  project_appeal    = as.integer(trimws(project_appeal)),
  pcnt_with_view    = as.numeric(trimws(pcnt_with_view)),

  # Amenities
  has_elevators     = trimws(elevators)    == "Y",
  has_security      = trimws(secty_system) == "Y",
  has_fireplace     = trimws(fireplace)    == "Y",
  laundry_type      = as.integer(trimws(laundry))   # 0=none,1=hookup,2=common
)]

# Derived
condo_complex_cln[, age_cplx     := pmax(0L, current_year - yr_built_cplx,  na.rm = FALSE)]
condo_complex_cln[, eff_age_cplx := pmax(0L, current_year - eff_yr_cplx,   na.rm = FALSE)]
condo_complex_cln[yr_built_cplx == 0L,  age_cplx     := NA_integer_]
condo_complex_cln[eff_yr_cplx   == 0L,  eff_age_cplx := NA_integer_]

# Complex type labels
condo_complex_cln[, complex_type_desc := fcase(
  complex_type == 1L,  "Standard Condo",
  complex_type == 2L,  "Co-op",
  complex_type == 3L,  "Timeshare",
  complex_type == 4L,  "MFTE",
  complex_type == 5L,  "Other",
  complex_type == 6L,  "Leasehold",
  complex_type == 10L, "Condo Hotel",
  !is.na(complex_type), paste0("Type_", complex_type),
  default = NA_character_
)]

# Laundry label
condo_complex_cln[, laundry_desc := fcase(
  laundry_type == 0L, "None",
  laundry_type == 1L, "Hookup",
  laundry_type == 2L, "Common",
  default = NA_character_
)]

# Size tier by average unit sqft
condo_complex_cln[, unit_size_tier := fcase(
  avg_unit_size < 600,                     "Micro (<600 sqft)",
  avg_unit_size >= 600  & avg_unit_size <  900, "Small (600-900)",
  avg_unit_size >= 900  & avg_unit_size < 1400, "Medium (900-1400)",
  avg_unit_size >= 1400 & avg_unit_size < 2000, "Large (1400-2000)",
  avg_unit_size >= 2000,                   "Extra Large (2000+)",
  default = NA_character_
)]

# Flag: complex eligible for MFTE rent limits
condo_complex_cln[, is_mfte := complex_type == 4L]

# Flag: converted from apartment building
condo_complex_cln[, is_apt_conversion := apt_conversion]

# Quality description (KCA 1-9 scale, same as commercial)
condo_complex_cln[, bldg_quality_desc := fcase(
  bldg_quality_cplx == 1L, "Very Low",
  bldg_quality_cplx == 2L, "Low",
  bldg_quality_cplx == 3L, "Fair",
  bldg_quality_cplx == 4L, "Average",
  bldg_quality_cplx == 5L, "Good",
  bldg_quality_cplx == 6L, "Very Good",
  bldg_quality_cplx == 7L, "Excellent",
  bldg_quality_cplx == 8L, "Luxury",
  bldg_quality_cplx == 9L, "Landmark",
  default = NA_character_
)]

setkey(condo_complex_cln, major)
message("    condo_complex_cln rows: ", nrow(condo_complex_cln))

# =============================================================================
# 2.  EXTR_CondoUnit  (unit-level, Major-Minor)
# =============================================================================
message("  Reading EXTR_CondoUnit ...")

if (!file.exists(unit_path)) {
  # CondoUnit may not be in the extraction depending on KCA vintage.
  # Build a stub from CondoComplex Majors so the pipeline can proceed —
  # complex-level attributes will still enrich the AV panel.
  warning(
    "EXTR_CondoUnit.csv not found at:\n  ", unit_path, "\n",
    "Building stub unit table from CondoComplex Majors.\n",
    "Individual unit attributes (floor, bedrooms, unit sqft) will be NA.\n",
    "Place EXTR_CondoUnit.csv in the kca_date folder to enable full unit detail."
  )
  condo_unit_cln <- condo_complex_cln[, .(
    major     = major,
    minor     = "0000",
    parcel_id = paste0(major, "-0000"),
    # Stub — unit-level fields populated from CondoUnit when available
    unit_sqft           = avg_unit_size,
    unit_floor          = NA_integer_,
    nbr_bedrooms        = NA_integer_,
    nbr_baths           = NA_real_,
    view_utilization    = NA_integer_,
    storage             = NA_integer_,
    parking_nbr_spaces  = NA_integer_
  )]

} else {
  unit_raw <- read_kca(unit_path)

  # Standard CondoUnit columns (KCA schema):
  # Major, Minor, UnitTypeCode, UnitNbr, Bldg, Floor, UnitSqFt,
  # NbrBedrooms, NbrBaths, ViewUtilization, Storage, ParkingNbrSpaces, ...
  unit_cols_avail <- names(unit_raw)

  condo_unit_cln <- unit_raw[, .(
    major    = str_pad(trimws(major), 6, "left", "0"),
    minor    = str_pad(trimws(minor), 4, "left", "0"),

    unit_sqft          = if ("unit_sqft"           %in% unit_cols_avail) as.numeric(trimws(unit_sqft))           else NA_real_,
    unit_floor         = if ("floor"               %in% unit_cols_avail) as.integer(trimws(floor))               else NA_integer_,
    nbr_bedrooms       = if ("nbr_bedrooms"        %in% unit_cols_avail) as.integer(trimws(nbr_bedrooms))        else NA_integer_,
    nbr_baths          = if ("nbr_baths"           %in% unit_cols_avail) as.numeric(trimws(nbr_baths))           else NA_real_,
    view_utilization   = if ("view_utilization"    %in% unit_cols_avail) as.integer(trimws(view_utilization))    else NA_integer_,
    storage            = if ("storage"             %in% unit_cols_avail) as.integer(trimws(storage))             else NA_integer_,
    parking_nbr_spaces = if ("parking_nbr_spaces"  %in% unit_cols_avail) as.integer(trimws(parking_nbr_spaces))  else NA_integer_
  )]

  condo_unit_cln[, parcel_id := paste0(major, "-", minor)]
}

setkey(condo_unit_cln, major, minor)
message("    condo_unit_cln rows: ", nrow(condo_unit_cln))

# =============================================================================
# 3.  Join complex attributes onto unit parcels
# =============================================================================
message("  Joining complex attributes onto unit parcels ...")

# Join is Major-only: every unit inherits its complex's attributes
condo_parcel_full <- merge(
  condo_unit_cln,
  condo_complex_cln,
  by     = "major",
  all.x  = TRUE
)

setDT(condo_parcel_full)

# Derived unit-level fields (only available when CondoUnit has data)
condo_parcel_full[!is.na(unit_sqft) & unit_sqft > 0,
  log_unit_sqft := log(unit_sqft)]

condo_parcel_full[!is.na(avg_unit_size) & avg_unit_size > 0,
  log_avg_unit_size := log(avg_unit_size)]

# Penthouse flag (top floor — requires floor + nbr_stories_cplx)
condo_parcel_full[!is.na(unit_floor) & !is.na(nbr_stories_cplx) & nbr_stories_cplx > 0,
  is_penthouse := as.integer(unit_floor == nbr_stories_cplx)]

# High-rise (8+ stories is conventional threshold)
condo_parcel_full[, is_highrise := as.integer(!is.na(nbr_stories_cplx) & nbr_stories_cplx >= 8L)]

# Prop type flag — consistent with "R" (res) and "C" (comm) convention
condo_parcel_full[, prop_type         := "D"]   # D = condo/Dwelling unit
condo_parcel_full[, prop_class        := "Condo"]
condo_parcel_full[, prop_class_detail := complex_type_desc]

# =============================================================================
# 4.  Flag mixed-use parcels already in CommBldg
#     (prevents double-counting in panel_tbl_all)
# =============================================================================
if (exists("parcel_comm_full", envir = .GlobalEnv)) {
  comm_majors <- unique(
    substr(get("parcel_comm_full", envir = .GlobalEnv)$parcel_id, 1, 6)
  )
  condo_parcel_full[, in_comm_bldg := major %in% comm_majors]
  n_overlap <- sum(condo_parcel_full$in_comm_bldg, na.rm = TRUE)
  message("    Condo units flagged as also in CommBldg: ", n_overlap,
          " (", round(100 * n_overlap / nrow(condo_parcel_full), 1), "%)")
} else {
  condo_parcel_full[, in_comm_bldg := FALSE]
  message("    parcel_comm_full not yet in GlobalEnv — in_comm_bldg set FALSE; ",
          "re-run after 01_import_comm.R if needed.")
}

condo_mixed_use_majors <- unique(
  condo_parcel_full[in_comm_bldg == TRUE, major]
)

setkey(condo_parcel_full, major, minor)

message("    condo_parcel_full rows: ", nrow(condo_parcel_full),
        " | unique majors (complexes): ", uniqueN(condo_parcel_full$major))

# =============================================================================
# 5.  Export
# =============================================================================
assign("condo_complex_cln",      condo_complex_cln,      envir = .GlobalEnv)
assign("condo_unit_cln",         condo_unit_cln,         envir = .GlobalEnv)
assign("condo_parcel_full",      condo_parcel_full,      envir = .GlobalEnv)
assign("condo_mixed_use_majors", condo_mixed_use_majors, envir = .GlobalEnv)

message("01_import_condo.R loaded.")
