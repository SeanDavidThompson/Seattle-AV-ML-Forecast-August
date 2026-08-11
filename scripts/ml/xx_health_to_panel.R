# xx_health_to_panel.R --------------------------------------------------------
# Match Public Health food inspection results to commercial parcels via situs
# address, build parcel-year hlth_* features, and join to panel_tbl_com.
#
# Requires in .GlobalEnv:
#   panel_tbl_com            (from panel assembly)
#   health_insp_raw          (from xx_health_inspections_import.R; if absent,
#                             hlth_* columns are stubbed NA and we return)
#   kca_date_data_extracted, cache_dir
#
# Parcel matching:
#   EXTR_CommBldg carries situs address components (building_number,
#   direction_prefix, street_name, street_type, direction_suffix, zip_code).
#   Two-pass match against normalized inspection addresses:
#     pass 1: zip5 + full normalized street string
#     pass 2: zip5 + house number + core street name (type/directionals
#             stripped), only where that key maps to a single parcel
#
# Year alignment: inspections in calendar year Y -> tax_yr Y+1
#   (matches xx_nwmls_to_panel.R's December-snapshot -> next tax_yr convention)
#
# Features (parcel-year, prefix hlth_; NOTE inspection_score = violation
# points, so HIGHER = WORSE):
#   hlth_n_estab        distinct establishments on parcel
#   hlth_n_insp         inspections that year
#   hlth_score_mean/max mean/worst inspection score
#   hlth_red_share      share of inspections with a red (critical) violation
#   hlth_unsat_share    share of inspections with unsatisfactory result
#   hlth_closed_n       inspections that closed the business
#   hlth_grade_mean     PH rating 1 (Excellent) .. 4 (Needs to improve)
#   hlth_score_mean_3yr 3-yr rolling mean of hlth_score_mean
#   hlth_has_data       1 = parcel-year has (or carries forward) health data
#
# After the observed values are joined, hlth_* are LOCF-filled forward per
# parcel across historical years (ratings persist between inspections; no
# backfill, so pre-data years stay honestly NA).  The extend script's
# last-year snapshot then carries the latest values into forecast years
# automatically.
#
# Model pickup: build_subgroup_model_data() auto-includes numeric panel
# columns, so hospitality/retail models gain these predictors with no model
# script changes — provided the column clears the trainer's <50% NA gate
# (NA share is reported below so this is visible).
# -----------------------------------------------------------------------------

stopifnot(exists("panel_tbl_com", envir = .GlobalEnv))

cache_dir <- get("cache_dir", envir = .GlobalEnv)
kca_date  <- get("kca_date_data_extracted", envir = .GlobalEnv)

health_yr_offset <- get0("health_yr_offset", envir = .GlobalEnv,
                         ifnotfound = 1L)   # calendar yr -> tax_yr
health_locf      <- get0("health_locf", envir = .GlobalEnv,
                         ifnotfound = TRUE) # carry ratings between inspections

message("\n--- xx_health_to_panel.R ---")

hlth_cols <- c("hlth_n_estab", "hlth_n_insp", "hlth_score_mean",
               "hlth_score_max", "hlth_red_share", "hlth_unsat_share",
               "hlth_closed_n", "hlth_grade_mean", "hlth_score_mean_3yr",
               "hlth_has_data")

.stub_and_exit <- function(why) {
  warning("xx_health_to_panel.R: ", why,
          " — stubbing hlth_* columns as NA.", call. = FALSE)
  pc <- get("panel_tbl_com", envir = .GlobalEnv)
  data.table::setDT(pc)
  for (col in hlth_cols) if (!col %in% names(pc)) pc[, (col) := NA_real_]
  assign("panel_tbl_com", pc, envir = .GlobalEnv)
  message("xx_health_to_panel.R complete (stubbed).")
}

# Load inspections (GlobalEnv first, then cache)
if (!exists("health_insp_raw", envir = .GlobalEnv)) {
  hc <- file.path(cache_dir, "health_inspections_raw.rds")
  if (file.exists(hc))
    assign("health_insp_raw", readRDS(hc), envir = .GlobalEnv)
}
if (!exists("health_insp_raw", envir = .GlobalEnv) ||
    is.null(get("health_insp_raw", envir = .GlobalEnv))) {
  .stub_and_exit("no health inspection data available")
} else {

insp <- data.table::copy(get("health_insp_raw", envir = .GlobalEnv))
data.table::setDT(insp)

# ============================================================================
# 1. Address normalization helpers
# ============================================================================
.abbrev <- c(
  "AVENUE" = "AVE", "AV" = "AVE", "STREET" = "ST", "BOULEVARD" = "BLVD",
  "DRIVE" = "DR", "ROAD" = "RD", "PLACE" = "PL", "COURT" = "CT",
  "LANE" = "LN", "HIGHWAY" = "HWY", "PARKWAY" = "PKWY", "TERRACE" = "TER",
  "CIRCLE" = "CIR", "SQUARE" = "SQ", "TRAIL" = "TRL", "WY" = "WAY",
  "NORTH" = "N", "SOUTH" = "S", "EAST" = "E", "WEST" = "W",
  "NORTHEAST" = "NE", "NORTHWEST" = "NW",
  "SOUTHEAST" = "SE", "SOUTHWEST" = "SW"
)
.type_dir_tokens <- unique(c(.abbrev,
  c("AVE","ST","BLVD","DR","RD","PL","CT","LN","HWY","PKWY","TER","CIR",
    "SQ","TRL","WAY","N","S","E","W","NE","NW","SE","SW")))

norm_addr <- function(x) {
  x <- toupper(trimws(as.character(x)))
  # Strip unit/suite designators and everything after them
  x <- sub("\\s+(STE|SUITE|UNIT|APT|BLDG|FL|FLOOR|RM|ROOM|#).*$", "", x)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  # Word-wise abbreviation standardization
  vapply(strsplit(trimws(x), " "), function(tok) {
    tok <- ifelse(tok %in% names(.abbrev), .abbrev[tok], tok)
    paste(tok, collapse = " ")
  }, character(1))
}

# Core key: house number + street tokens minus type/directional words
core_key <- function(norm) {
  vapply(strsplit(norm, " "), function(tok) {
    if (length(tok) < 2) return(NA_character_)
    num  <- tok[1]
    rest <- tok[-1]
    core <- rest[!rest %in% .type_dir_tokens]
    if (length(core) == 0) core <- rest      # e.g. numbered avenues: "4TH AVE"
    paste(c(num, core), collapse = " ")
  }, character(1))
}

zip5 <- function(z) {
  z <- gsub("[^0-9]", "", as.character(z))
  ifelse(nchar(z) >= 5, substr(z, 1, 5), NA_character_)
}

# ============================================================================
# 2. Parcel <- situs address lookup from EXTR_CommBldg
# ============================================================================
cb_path <- here::here("data", "kca", kca_date, "EXTR_CommBldg.csv")
if (!file.exists(cb_path)) {
  .stub_and_exit(paste0("EXTR_CommBldg.csv not found at ", cb_path))
} else {

cb <- data.table::fread(file = cb_path, encoding = "Latin-1",
                        na.strings = c("", "NA"), showProgress = FALSE)
data.table::setDT(cb)
for (cc in names(cb)[vapply(cb, is.character, logical(1))])
  data.table::set(cb, j = cc,
                  value = iconv(cb[[cc]], from = "UTF-8", to = "UTF-8",
                                sub = ""))
cb <- janitor::clean_names(cb)

addr_parts <- intersect(
  c("building_number", "fraction", "direction_prefix", "street_name",
    "street_type", "direction_suffix"),
  names(cb))
if (!all(c("building_number", "street_name") %in% addr_parts) ||
    !"zip_code" %in% names(cb)) {
  .stub_and_exit("EXTR_CommBldg lacks expected situs address columns")
} else {

# Blank out NAs so they don't paste as literal "NA" tokens
for (ap in addr_parts) {
  v <- as.character(cb[[ap]]); v[is.na(v)] <- ""
  data.table::set(cb, j = ap, value = v)
}
cb[, addr_str := do.call(paste, c(.SD, sep = " ")), .SDcols = addr_parts]
cb[, `:=`(
  major = stringr::str_pad(trimws(as.character(major)), 6, "left", "0"),
  minor = stringr::str_pad(trimws(as.character(minor)), 4, "left", "0"),
  addr_norm = norm_addr(addr_str),
  zip5      = zip5(zip_code)
)]
cb[, parcel_id_nodash := paste0(major, minor)]

# Match panel's parcel_id format (dash vs no-dash)
panel_com <- get("panel_tbl_com", envir = .GlobalEnv)
data.table::setDT(panel_com)
panel_dash <- any(grepl("-", head(panel_com$parcel_id, 25)))
cb[, parcel_id := if (panel_dash) paste0(major, "-", minor)
                  else parcel_id_nodash]

lkp <- unique(cb[!is.na(zip5) & nchar(addr_norm) > 3,
                 .(parcel_id, addr_norm, zip5)])
lkp[, k1 := paste(zip5, addr_norm)]
lkp[, k2 := paste(zip5, core_key(addr_norm))]

# Keys mapping to >1 parcel are ambiguous -> drop from that pass
k1_map <- lkp[, .(n = data.table::uniqueN(parcel_id),
                  parcel_id = parcel_id[1]), by = k1][n == 1]
k2_map <- lkp[!is.na(k2), .(n = data.table::uniqueN(parcel_id),
                            parcel_id = parcel_id[1]), by = k2][n == 1]
message("  Situs lookup: ", scales::comma(nrow(lkp)), " address rows | ",
        scales::comma(nrow(k1_map)), " unique full keys | ",
        scales::comma(nrow(k2_map)), " unique core keys")

# ============================================================================
# 3. Match establishments to parcels (two passes)
# ============================================================================
estab <- unique(insp[, .(estab_id, address, zip_code)])
estab[, `:=`(addr_norm = norm_addr(address), zip5 = zip5(zip_code))]
estab <- estab[!is.na(zip5) & nchar(addr_norm) > 3]
estab[, k1 := paste(zip5, addr_norm)]
estab[, k2 := paste(zip5, core_key(addr_norm))]

estab[k1_map, on = "k1", parcel_id := i.parcel_id]
n_p1 <- sum(!is.na(estab$parcel_id))
estab[is.na(parcel_id), parcel_id := k2_map[.SD, on = "k2", x.parcel_id]]
n_p2 <- sum(!is.na(estab$parcel_id)) - n_p1

message("  Establishment match: pass1=", scales::comma(n_p1),
        " | pass2=", scales::comma(n_p2), " | unmatched=",
        scales::comma(sum(is.na(estab$parcel_id))),
        "  (", round(100 * mean(!is.na(estab$parcel_id)), 1), "% matched)")

estab_match <- estab[!is.na(parcel_id), .(estab_id, parcel_id)]
# An establishment key should resolve to one parcel
estab_match <- unique(estab_match, by = "estab_id")

if (nrow(estab_match) == 0) {
  .stub_and_exit("no establishments matched to parcels")
} else {

# ============================================================================
# 4. Collapse violation rows -> inspection level -> parcel-year features
# ============================================================================
insp[estab_match, on = "estab_id", parcel_id := i.parcel_id]
insp <- insp[!is.na(parcel_id)]

has <- function(col) col %in% names(insp)

insp_lvl <- insp[, .(
  parcel_id = parcel_id[1],
  estab_id  = estab_id[1],
  insp_yr   = insp_yr[1],
  score     = if (has("inspection_score"))
                suppressWarnings(max(inspection_score, na.rm = TRUE))
              else NA_real_,
  red       = if (has("violation_type"))
                as.integer(any(grepl("red", violation_type,
                                     ignore.case = TRUE), na.rm = TRUE))
              else NA_integer_,
  unsat     = if (has("inspection_result"))
                as.integer(any(grepl("unsat", inspection_result,
                                     ignore.case = TRUE), na.rm = TRUE))
              else NA_integer_,
  closed    = if (has("inspection_closed_business"))
                as.integer(any(inspection_closed_business %in%
                               c(TRUE, "true", "TRUE", "Yes", "Y", "1"),
                               na.rm = TRUE))
              else NA_integer_,
  grade     = if (has("grade")) suppressWarnings(mean(grade, na.rm = TRUE))
              else NA_real_
), by = inspection_serial_num]
insp_lvl[!is.finite(score), score := NA_real_]
insp_lvl[!is.finite(grade), grade := NA_real_]

insp_lvl[, tax_yr := insp_yr + as.integer(health_yr_offset)]

health_parcel_year <- insp_lvl[, .(
  hlth_n_estab     = data.table::uniqueN(estab_id),
  hlth_n_insp      = .N,
  hlth_score_mean  = mean(score, na.rm = TRUE),
  hlth_score_max   = suppressWarnings(max(score, na.rm = TRUE)),
  hlth_red_share   = mean(red, na.rm = TRUE),
  hlth_unsat_share = mean(unsat, na.rm = TRUE),
  hlth_closed_n    = sum(closed, na.rm = TRUE),
  hlth_grade_mean  = mean(grade, na.rm = TRUE)
), by = .(parcel_id, tax_yr)]

for (col in c("hlth_score_mean", "hlth_score_max", "hlth_red_share",
              "hlth_unsat_share", "hlth_grade_mean"))
  health_parcel_year[!is.finite(get(col)), (col) := NA_real_]

data.table::setkeyv(health_parcel_year, c("parcel_id", "tax_yr"))
health_parcel_year[, hlth_score_mean_3yr :=
  data.table::frollmean(hlth_score_mean, 3, na.rm = TRUE,
                        align = "right", algo = "exact"),
  by = parcel_id]
health_parcel_year[, hlth_has_data := 1]

message("  Parcel-year features: ",
        scales::comma(nrow(health_parcel_year)), " rows | ",
        scales::comma(data.table::uniqueN(health_parcel_year$parcel_id)),
        " parcels | tax_yr ", min(health_parcel_year$tax_yr), "-",
        max(health_parcel_year$tax_yr))

saveRDS(health_parcel_year,
        file.path(cache_dir, "health_parcel_year.rds"))
assign("health_parcel_year", health_parcel_year, envir = .GlobalEnv)
message("  \U1f4be cached: health_parcel_year.rds")

# ============================================================================
# 5. Join to panel_tbl_com (+ optional historical LOCF)
# ============================================================================
# Drop stale hlth_ columns before joining (rerun safety)
stale <- intersect(hlth_cols, names(panel_com))
if (length(stale) > 0) panel_com[, (stale) := NULL]

panel_com[health_parcel_year, on = .(parcel_id, tax_yr),
          (hlth_cols) := mget(paste0("i.", hlth_cols))]

if (isTRUE(health_locf)) {
  data.table::setkeyv(panel_com, c("parcel_id", "tax_yr"))
  for (col in hlth_cols)
    panel_com[, (col) := zoo::na.locf(get(col), na.rm = FALSE),
              by = parcel_id]
  message("  LOCF applied: ratings carried forward between inspection years")
}

na_share <- round(100 * mean(is.na(panel_com$hlth_score_mean)), 1)
message("  Joined to panel_tbl_com: hlth_score_mean NA share = ",
        na_share, "% of all com rows",
        " (hospitality/retail coverage will be much higher; ",
        "trainer drops features with >50% NA within a subgroup)")

assign("panel_tbl_com", panel_com, envir = .GlobalEnv)

message("xx_health_to_panel.R complete.")
}}}}
