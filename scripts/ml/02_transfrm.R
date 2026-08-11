# =============================================================================
# 02_transfrm.R
# =============================================================================

stopifnot(exists("parcel_res_full", envir = .GlobalEnv))

prf <- copy(parcel_res_full)
setDT(prf)

message("  02_transfrm.R: applying data quality fixes to parcel_res_full (",
        nrow(prf), " rows) ...")

if ("daylight_basement" %in% names(prf)) {
  # Normalise case
  n_lower <- sum(toupper(trimws(prf$daylight_basement)) != prf$daylight_basement &
                   !is.na(prf$daylight_basement), na.rm = TRUE)
  prf[, daylight_basement := toupper(trimws(daylight_basement))]

  # Remove any pre-existing column (data.table := preserves existing type,
  # which can coerce integer → factor if 01_import.R stored it as factor).
  if ("daylight_basement_any" %in% names(prf))
    prf[, daylight_basement_any := NULL]

  # Create fresh as integer (avoids type-coercion trap)
  prf[, daylight_basement_any := as.integer(
    !is.na(daylight_basement) & daylight_basement == "Y"
  )]
  message("    daylight_basement_any: ",
          sum(prf$daylight_basement_any, na.rm = TRUE), " flagged TRUE")
}

if ("pcnt_complete" %in% names(prf)) {
  # Treat 0 as "not set" (same as 100 — assessor omits for complete buildings)
  if ("is_incomplete_bldg" %in% names(prf)) prf[, is_incomplete_bldg := NULL]
  prf[, is_incomplete_bldg := as.integer(
    !is.na(pcnt_complete) & pcnt_complete > 0L & pcnt_complete < 100L
  )]
  n_inc <- sum(prf$is_incomplete_bldg, na.rm = TRUE)
  message("    is_incomplete_bldg: ", n_inc,
          " parcels flagged (pcnt_complete between 1 and 99)")
} else {
  if ("is_incomplete_bldg" %in% names(prf)) prf[, is_incomplete_bldg := NULL]
  prf[, is_incomplete_bldg := 0L]
}

# model_res: 0 = exclude from training/forecasting, 1 = include
# Start at 1 for all; fixes below set to 0 where appropriate
# model_res: drop any pre-existing column (could be chr/factor from 01_import.R)
# then create fresh as integer 1 = include, 0 = exclude from training.
if ("model_res" %in% names(prf)) prf[, model_res := NULL]
prf[, model_res := 1L]
prf[is_incomplete_bldg == 1L, model_res := 0L]

grade_col <- intersect(c("avg_bldg_grade", "bldg_grade"), names(prf))[1]

if (!is.na(grade_col)) {
  if ("is_exceptional_grade" %in% names(prf)) prf[, is_exceptional_grade := NULL]
  prf[, is_exceptional_grade := as.integer(get(grade_col) == 20L)]
  n_exc <- sum(prf$is_exceptional_grade, na.rm = TRUE)
  if (n_exc > 0) {
    message("    is_exceptional_grade: ", n_exc,
            " parcels with bldg_grade=20 ('Exceptional Properties') — capping at 13")
  }
  if ("bldg_grade_capped" %in% names(prf)) prf[, bldg_grade_capped := NULL]
  # grade_col is <chr> in real data (e.g. "7","11") — must go via as.numeric
  prf[, bldg_grade_capped := pmin(
    suppressWarnings(as.integer(as.numeric(trimws(get(grade_col))))),
    13L
  )]
} else {
  prf[, is_exceptional_grade := 0L]
  message("    bldg_grade column not found in parcel_res_full — skipping grade cap")
}

#
# The correct approach is to take bldg_grade from bldg_nbr == 1 only, and
# sum sq_ft_tot_living across all buildings.
#
# If 01_import.R already does this correctly, the re-aggregation below is
# a no-op (values will be identical). If it does not, this corrects it.

if (exists("res_bldg_raw", envir = .GlobalEnv)) {
  # res_bldg_raw = the un-aggregated EXTR_ResBldg table (if 01_import.R
  # assigned it). Use it to recompute primary-only grade.
  raw_bldg <- copy(res_bldg_raw)
  setDT(raw_bldg)
} else if (exists("res_bldg", envir = .GlobalEnv)) {
  # Some versions of 01_import.R assign as 'res_bldg' not 'res_bldg_raw'
  raw_bldg <- copy(res_bldg)
  setDT(raw_bldg)
} else {
  raw_bldg <- NULL
}

if (!is.null(raw_bldg)) {
  if ("bldg_nbr" %in% names(raw_bldg)) {
    # bldg_grade, condition, heat_system are <chr> in real data — must go via as.numeric
    chr_to_int_bldg <- function(x) suppressWarnings(as.integer(as.numeric(trimws(x))))

    raw_bldg[, min_bldg_nbr := min(bldg_nbr, na.rm = TRUE), by = parcel_id]
    primary <- raw_bldg[bldg_nbr == min_bldg_nbr]

    primary_agg <- primary[, .(
      avg_bldg_grade_primary = chr_to_int_bldg(bldg_grade),
      condition_primary      = chr_to_int_bldg(condition),
      heat_system_primary    = chr_to_int_bldg(heat_system),
      yr_built_primary       = as.integer(yr_built),
      stories_primary        = as.numeric(stories)
    ), by = parcel_id]

    total_sqft_agg <- raw_bldg[, .(
      total_living_sqft_all = sum(as.numeric(sq_ft_tot_living), na.rm = TRUE)
    ), by = parcel_id]

    prf <- merge(prf, primary_agg,    by = "parcel_id", all.x = TRUE)
    prf <- merge(prf, total_sqft_agg, by = "parcel_id", all.x = TRUE)

    if ("avg_bldg_grade" %in% names(prf))
      prf[!is.na(avg_bldg_grade_primary), avg_bldg_grade := avg_bldg_grade_primary]

    # Promote total_living_sqft_all → total_living_sqft regardless of whether
    # the column already exists.  This ensures FIX 10 (is_land_only) can find it.
    if ("total_living_sqft" %in% names(prf)) {
      prf[!is.na(total_living_sqft_all), total_living_sqft := total_living_sqft_all]
    } else {
      data.table::setnames(prf, "total_living_sqft_all", "total_living_sqft")
    }

    prf[, avg_bldg_grade_primary := NULL]
    if ("total_living_sqft_all" %in% names(prf))
      prf[, total_living_sqft_all := NULL]
    # min_bldg_nbr lives on raw_bldg, not prf — no need to drop from prf

    n_secondary <- nrow(raw_bldg) - nrow(primary)
    message("    Primary-building grade correction applied. ",
            n_secondary, " secondary building rows excluded from grade aggregation.")
  } else {
    message("    bldg_nbr not found in bldg table — skipping primary-building grade correction.")
  }
} else {
  message("    Residential building table not found in GlobalEnv — skipping primary-building ",
          "grade correction. Ensure 01_import.R assigns res_bldg or res_bldg_raw.")
}


bldg_tbl <- if (exists("res_bldg_raw", envir = .GlobalEnv)) {
  res_bldg_raw
} else if (exists("res_bldg", envir = .GlobalEnv)) {
  res_bldg
} else {
  NULL
}

if (!is.null(bldg_tbl) && "nbr_living_units" %in% names(bldg_tbl)) {
  setDT(bldg_tbl)
  lu_agg <- bldg_tbl[, .(max_living_units = max(as.numeric(nbr_living_units), na.rm = TRUE)),
                      by = parcel_id]
  prf <- merge(prf, lu_agg, by = "parcel_id", all.x = TRUE)
  prf[, is_small_mf := as.integer(!is.na(max_living_units) &
                                     max_living_units >= 2 & max_living_units <= 4)]
  message("    is_small_mf: ", sum(prf$is_small_mf, na.rm = TRUE),
          " parcels with 2-4 units (cross-track dedup applied later)")
  prf[, max_living_units := NULL]  # don't carry temp join column
} else {
  lu_col <- intersect(c("nbr_living_units", "max_living_units"), names(prf))[1]
  if (!is.na(lu_col)) {
    prf[, is_small_mf := as.integer(get(lu_col) >= 2L & get(lu_col) <= 4L)]
    message("    is_small_mf: ", sum(prf$is_small_mf, na.rm = TRUE),
            " parcels with 2-4 units flagged (cross-track dedup applied later)")
  } else {
    prf[, is_small_mf := 0L]
    message("    nbr_living_units not found in parcel or bldg table — is_small_mf set to 0")
  }
}


if ("airport_noise" %in% names(prf)) {
  prf[, airport_noise := as.integer(airport_noise)]
  # Values of 0 or NA mean no airport noise designation — leave as-is;
  # the model will learn the numeric scale directly.
  n_airport <- sum(!is.na(prf$airport_noise) & prf$airport_noise > 0, na.rm = TRUE)
  message("    airport_noise: ", n_airport, " non-zero parcels (kept as standalone feature)")
}

# Rebuild nuisance_score from the three ordinal/binary components only.
# IMPORTANT: all three columns are <chr> in real KCA data (e.g. "0","1","2","N","Y").
# as.integer() on a character vector in data.table coerces through factor → crash.
# Must extract to plain R vectors via as.numeric() first.
chr_to_int <- function(x) suppressWarnings(as.integer(as.numeric(trimws(x))))
yn_flag    <- function(x) as.integer(toupper(trimws(x)) %in% c("Y", "1", "YES"))

tn  <- if ("traffic_noise"   %in% names(prf)) chr_to_int(prf$traffic_noise)   else 0L
pl  <- if ("power_lines"     %in% names(prf)) yn_flag(prf$power_lines)         else 0L
on_ <- if ("other_nuisances" %in% names(prf)) yn_flag(prf$other_nuisances)     else 0L

tn[is.na(tn)]   <- 0L
pl[is.na(pl)]   <- 0L
on_[is.na(on_)] <- 0L

prf[, nuisance_score := tn + pl + on_]
prf[is.na(nuisance_score), nuisance_score := 0L]

message("    nuisance_score rebuilt (traffic + power + other; airport_noise separate)")
message("    nuisance_score range: ", min(prf$nuisance_score, na.rm = TRUE),
        " - ", max(prf$nuisance_score, na.rm = TRUE))


if ("prop_type" %in% names(prf)) {
  # Condo complex master records (prop_type=K, minor=0000) are ghost parcels
  # with no building attributes — one record per complex per KCA Parcel doc
  n_condo_master <- sum(prf$prop_type == "K" &
                          trimws(prf$minor) == "0000", na.rm = TRUE)
  if (n_condo_master > 0) {
    message("    Removing ", n_condo_master,
            " condo complex master records (prop_type=K, minor=0000)")
    prf <- prf[!(prop_type == "K" & trimws(minor) == "0000")]
  }

  # Non-modellable property types
  excl_types <- c("M", "N", "U", "X", "T")
  n_excl_type <- sum(prf$prop_type %in% excl_types, na.rm = TRUE)
  if (n_excl_type > 0) {
    message("    Removing ", n_excl_type,
            " parcels with non-modellable prop_type (M/N/U/X/T)")
    prf <- prf[!prop_type %in% excl_types]
  }
  message("    prop_type filter complete: ", nrow(prf), " parcels remain")
} else {
  message("    prop_type column not found — skipping exempt parcel type filter")
}

# Exempt flag from AV roll (if available — joined during 01_import.R or
# xx_av_history.R). model_res=0 for these; they stay in panel for completeness.
# Exempt flag from AV roll (if available — joined during 01_import.R or
# xx_av_history.R). model_res=0 for these; they stay in panel for completeness.
# Actual KCA column names: tax_stat (not tax_status), tax_val_reason (not taxable_value_reason)
tax_stat_col <- intersect(c("tax_stat", "tax_status"), names(prf))[1]
if (!is.na(tax_stat_col)) {
  n_exempt_status <- sum(prf[[tax_stat_col]] != "T", na.rm = TRUE)
  message("    ", tax_stat_col, " != 'T': ", n_exempt_status,
          " parcels (Exempt/Operating) — setting model_res=0")
  prf[get(tax_stat_col) != "T", model_res := 0L]
}

tax_reason_col <- intersect(c("tax_val_reason", "taxable_value_reason"), names(prf))[1]
if (!is.na(tax_reason_col)) {
  exempt_reasons <- c("EX", "FS", "CU", "HP", "NP", "OP")
  n_exempt_reason <- sum(prf[[tax_reason_col]] %in% exempt_reasons, na.rm = TRUE)
  message("    ", tax_reason_col, " exempt codes: ", n_exempt_reason,
          " parcels — setting model_res=0")
  prf[get(tax_reason_col) %in% exempt_reasons, model_res := 0L]
}


hbu_labels <- c(
  "1"  = "SINGLE FAMILY",      "2"  = "DUPLEX",
  "3"  = "TRIPLEX",            "4"  = "MOBILE HOME",
  "5"  = "OTHER SF DWELLING",  "6"  = "MULTI-FAMILY DWELLING",
  "7"  = "GROUP RESIDENCE",    "8"  = "TEMPORARY LODGING",
  "9"  = "PARK/RECREATION",    "10" = "AMUSEMENT/ENTERTAINMENT",
  "11" = "CULTURAL",           "12" = "EDUCATIONAL SERVICE",
  "13" = "COMMERCIAL SERVICE", "14" = "RETAIL/WHOLESALE",
  "15" = "MANUFACTURING",      "16" = "AGRICULTURAL",
  "17" = "FORESTRY",           "18" = "FISH & WILDLIFE MGMT",
  "19" = "MINERAL",            "20" = "REGIONAL LAND USE",
  "21" = "MIXED USE"
)

# hbu_as_if_vacant arrives as <chr> (e.g. "1", "6", "21") from 01_import.R.
# hbu_as_if_vacant_desc is already decoded by 01_import.R — no need to re-decode.
# CRITICAL: must use as.numeric() not as.integer() when converting chr to number;
# as.integer() on a character column in data.table goes through factor → crashes.

if ("hbu_as_if_vacant" %in% names(prf)) {
  # Ensure description exists (01_import.R already decodes, but guard anyway)
  if (!"hbu_as_if_vacant_desc" %in% names(prf)) {
    hbu_key <- trimws(prf$hbu_as_if_vacant)
    prf[, hbu_as_if_vacant_desc := hbu_labels[hbu_key]]
    message("    hbu_as_if_vacant decoded to hbu_as_if_vacant_desc")
  }

  # Non-SFR flag: codes 6-21.
  # hbu_as_if_vacant is <chr> — go via as.numeric(), never as.integer() on chr.
  hbu_num <- suppressWarnings(as.numeric(trimws(prf$hbu_as_if_vacant)))
  if ("is_non_sfr_hbu" %in% names(prf)) prf[, is_non_sfr_hbu := NULL]
  prf[, is_non_sfr_hbu := as.integer(!is.na(hbu_num) & hbu_num >= 6)]
  n_non_sfr <- sum(prf$is_non_sfr_hbu, na.rm = TRUE)
  message("    is_non_sfr_hbu: ", n_non_sfr,
          " parcels with non-residential HBU (code >= 6) — setting model_res=0")
  prf[is_non_sfr_hbu == 1L, model_res := 0L]
} else {
  prf[, is_non_sfr_hbu := 0L]
}

#
# train_res   = 1 for parcels used in model training (excludes land-only +
#               all existing model_res=0 exclusions).
# forecast_res = 1 for ALL parcels that should receive a forecast, including
#               land-only.  Non-modellable types (M/N/U/X/T) were already
#               hard-dropped in FIX 7, so every remaining parcel is eligible.

# Step 1: land-only flag
# Use total_living_sqft if present (most reliable); fall back to appr_imps_val.
# Both can be 0/NA simultaneously on a true vacant parcel.
has_sqft_col  <- "total_living_sqft" %in% names(prf)
has_imps_col  <- "appr_imps_val"     %in% names(prf)

if (has_sqft_col && has_imps_col) {
  prf[, is_land_only := as.integer(
    (is.na(total_living_sqft) | total_living_sqft == 0) &
    (is.na(appr_imps_val)     | appr_imps_val     == 0)
  )]
} else if (has_sqft_col) {
  prf[, is_land_only := as.integer(
    is.na(total_living_sqft) | total_living_sqft == 0
  )]
} else if (has_imps_col) {
  prf[, is_land_only := as.integer(
    is.na(appr_imps_val) | appr_imps_val == 0
  )]
} else {
  prf[, is_land_only := 0L]
  message("    is_land_only: neither total_living_sqft nor appr_imps_val found — set to 0")
}

n_land_only <- sum(prf$is_land_only, na.rm = TRUE)
message("    is_land_only: ", n_land_only,
        " parcels (no building sqft AND no improvement AV)")

# Step 2: train_res — model_res minus land-only parcels
# land-only parcels should not train the SFR improvement model.
prf[, train_res := as.integer(model_res == 1L & is_land_only == 0L)]

# Step 3: forecast_res — every parcel that should receive a forecast.
# All remaining rows (hard-drops already gone) get a forecast.
# model_res=0 parcels (exempt, non-SFR HBU, incomplete) still get land
# and improvement AV forecasted so we can sum to the certified total.
prf[, forecast_res := 1L]

message("    train_res=1 (used for model training): ",
        sum(prf$train_res,    na.rm = TRUE))
message("    forecast_res=1 (receive AV forecast):  ",
        sum(prf$forecast_res, na.rm = TRUE))


yn_to_int <- yn_flag  # alias — yn_flag defined above in FIX 6

# --- Access (LUType 55): 1=Restricted, 2=Legal/Undeveloped, 3=Private,
#     4=Public, 5=Walk-in. Lower codes = less accessible = lower value.
if ("access" %in% names(prf)) {
  prf[, access := chr_to_int(access)]
  message("    access: ", sum(!is.na(prf$access) & prf$access > 0), " non-zero")
}

# --- Water / Sewer system (LUTypes 56/57)
#     1=Private, 2=Public/District, 3=Private Restricted, 4=Public Restricted
if ("water_system" %in% names(prf))
  prf[, water_system := chr_to_int(water_system)]
if ("sewer_system"  %in% names(prf))
  prf[, sewer_system := chr_to_int(sewer_system)]

# --- Street surface (LUType 60): 1=Paved, 2=Gravel, 3=Dirt, 4=Undeveloped
if ("street_surface" %in% names(prf)) {
  prf[, street_surface := chr_to_int(street_surface)]
  n_unpaved <- sum(prf$street_surface > 1L, na.rm = TRUE)
  message("    street_surface: ", n_unpaved, " unpaved parcels")
}

# --- Topography (LUType 59): 1=YES (flag: hilly terrain)
if ("topography" %in% names(prf))
  prf[, topography := as.integer(chr_to_int(topography) == 1L)]

# --- Percent unusable
if ("pcnt_unusable" %in% names(prf)) {
  prf[, pcnt_unusable := as.numeric(pcnt_unusable)]
  n_unusable <- sum(prf$pcnt_unusable > 0, na.rm = TRUE)
  message("    pcnt_unusable > 0: ", n_unusable, " parcels")
}

# --- Waterfront (location code + footage)
if ("waterfront_location" %in% names(prf)) {
  prf[, waterfront_location := as.integer(waterfront_location)]
  prf[, is_waterfront := as.integer(!is.na(waterfront_location) &
                                       waterfront_location > 0L)]
  message("    is_waterfront: ", sum(prf$is_waterfront, na.rm = TRUE), " parcels")
}
if ("waterfront_footage" %in% names(prf))
  prf[, waterfront_footage := as.numeric(waterfront_footage)]

# --- Adjacent amenities
if ("adjacent_golf_fairway" %in% names(prf))
  prf[, adjacent_golf_fairway := yn_to_int(adjacent_golf_fairway)]
if ("adjacent_greenbelt" %in% names(prf))
  prf[, adjacent_greenbelt := yn_to_int(adjacent_greenbelt)]

# --- Lot depth factor (assessor's shape adjustment)
if ("lot_depth_factor" %in% names(prf))
  prf[, lot_depth_factor := as.numeric(lot_depth_factor)]

# --- Additional hazard flags (supplement existing seismic/landslide/steep_slope)
if ("erosion_hazard"      %in% names(prf))
  prf[, erosion_hazard      := yn_to_int(erosion_hazard)]
if ("critical_drainage"   %in% names(prf))
  prf[, critical_drainage   := yn_to_int(critical_drainage)]
if ("hundred_yr_flood_plain" %in% names(prf)) {
  prf[, hundred_yr_flood_plain := yn_to_int(hundred_yr_flood_plain)]
  message("    hundred_yr_flood_plain: ",
          sum(prf$hundred_yr_flood_plain, na.rm = TRUE), " parcels")
}
if ("stream"              %in% names(prf))
  prf[, stream              := yn_to_int(stream)]
if ("wetland"             %in% names(prf))
  prf[, wetland             := yn_to_int(wetland)]
if ("species_of_concern"  %in% names(prf))
  prf[, species_of_concern  := yn_to_int(species_of_concern)]
if ("native_growth_protection_easement" %in% names(prf))
  prf[, ngpe := yn_to_int(native_growth_protection_easement)]

new_feat_present <- intersect(
  c("access", "water_system", "sewer_system", "street_surface", "topography",
    "pcnt_unusable", "is_waterfront", "waterfront_footage",
    "adjacent_golf_fairway", "adjacent_greenbelt", "lot_depth_factor",
    "erosion_hazard", "critical_drainage", "hundred_yr_flood_plain",
    "stream", "wetland", "species_of_concern", "ngpe"),
  names(prf)
)
message("    New Parcel features present: ", length(new_feat_present),
        " of 18 — ", paste(new_feat_present, collapse = ", "))


message("  02_transfrm.R summary:")
message("    Total parcels:             ", nrow(prf))
message("    train_res=1 (train):       ", sum(prf$train_res    == 1L, na.rm = TRUE),
        "  (model training eligible)")
message("    forecast_res=1 (forecast): ", sum(prf$forecast_res == 1L, na.rm = TRUE),
        "  (all parcels receive AV forecast)")
message("    model_res=0 (flagged):     ", sum(prf$model_res == 0L, na.rm = TRUE),
        "  (incomplete + exempt + non-SFR HBU — forecasted but not trained)")
message("    is_land_only:              ",
        if ("is_land_only" %in% names(prf))
          sum(as.integer(prf$is_land_only), na.rm = TRUE) else "col not present",
        "  (land AV only; imps suppressed in retrofit/forecast)")
message("    Exceptional grade (=20):   ", sum(as.integer(prf$is_exceptional_grade), na.rm = TRUE))
message("    Small MF (2-4 units):      ", sum(as.integer(prf$is_small_mf), na.rm = TRUE),
        "  (cross-track dedup pending)")
message("    Daylight basement Y:       ",
        if ("daylight_basement_any" %in% names(prf))
          sum(as.integer(prf$daylight_basement_any), na.rm = TRUE) else "col not present")
message("    Non-SFR HBU (excl train):  ",
        if ("is_non_sfr_hbu" %in% names(prf))
          sum(as.integer(prf$is_non_sfr_hbu), na.rm = TRUE) else "col not present")
message("    airport_noise non-zero:    ",
        if ("airport_noise" %in% names(prf))
          sum(!is.na(prf$airport_noise) & prf$airport_noise > 0) else "col not present")
message("    New Parcel features added: ", length(new_feat_present), " of 18")

# =============================================================================
# Write back
# =============================================================================
assign("parcel_res_full", prf, envir = .GlobalEnv)
message("02_transfrm.R loaded.")
