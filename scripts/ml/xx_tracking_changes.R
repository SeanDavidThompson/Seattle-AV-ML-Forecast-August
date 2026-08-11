### Change history cleanup (58M rows) with UTF8-safe ingest
hist_path   <- here("data", "kca", kca_date_data_extracted, "changes", "EXTR_ChangeHist_V.csv")
detail_path <- here("data", "kca", kca_date_data_extracted, "changes", "EXTR_ChangeHistDetail_V.csv")

# ---------------------------
# Attributes we care about (from your prior models/text)
# RAW names here; we normalize to match
# ---------------------------
attrs_keep <- c(
  "Area",
  "CurrentZoning", "Current Zoning", "CurrentZoning_3",
  "HBUAsIfVacant", "HBU As If Vacant", "HBUAsIfVacantDesc", "HBUAsIfVacant_Desc",
  "Unbuildable",
  "SqFtLot", "Sq Ft Lot", "SqFt_Lot",
  "NuisanceScore", "Nuisance Score",
  "MtRainier", "Olympics", "Cascades", "Territorial",
  "SeattleSkyline", "PugetSound", "LakeWashington",
  "SeismicHazard", "LandslideHazard", "SteepSlopeHazard",
  "TrafficNoise", "AirportNoise", "PowerLines", "OtherNuisances",
  "Contamination",
  "HistoricSite"
)

attrs_keep_norm <- attrs_keep |>
  str_squish() |>
  str_replace_all("\\s+", "_") |>
  str_to_lower()

# ---------------------------
# 1) Read header fast with fread (tolerant)
# ---------------------------
changes_raw <- fread(
  file = hist_path,
  select = c("Major","Minor","Type","EventDate","EventPerson","DocId","PropStatus","EventId"),
  encoding = "Latin-1",     # safest for weird bytes
  showProgress = TRUE
)
setnames(changes_raw, tolower(names(changes_raw)))

# ---------------------------
# 2) Read detail with vroom (handles bad UTF8 + long lines)
#    Select only needed columns
# ---------------------------
detail_tbl <- vroom::vroom(
  detail_path,
  col_select = c(Major, Minor, EventId, Id, Attribute, AttributeValue, UpdateDate, UpdatedBy),
  locale = vroom::locale(encoding = "latin1"),   # <- FIXED (or "ISO-8859-1")
  altrep = FALSE,
  progress = TRUE
)

changes_detail_raw <- as.data.table(detail_tbl)
setnames(changes_detail_raw, tolower(names(changes_detail_raw)))


# ---------------------------
# 3) Normalize ONLY the string columns we need to UTF8
# ---------------------------
for (col in c("attribute","attributevalue","updatedate","updatedby")) {
  set(changes_detail_raw, j = col,
      value = stringi::stri_enc_toutf8(changes_detail_raw[[col]],
                                       is_unknown_8bit = TRUE))
}

# ---------------------------
# 4) Standardize keys + parse dates (header)
# ---------------------------
changes_raw[, major := str_pad(major, 6, pad = "0")]
changes_raw[, minor := str_pad(minor, 4, pad = "0")]
changes_raw[, parcel_id := paste0(major, "-", minor)]
changes_raw[, event_date := ymd_hms(eventdate, tz = "America/Los_Angeles")]

changes <- changes_raw[, .(
  parcel_id, major, minor,
  event_id = eventid,
  event_type = type,
  event_date,
  event_person = eventperson,
  doc_id = docid,
  prop_status = propstatus
)]

# ---------------------------
# 5) Standardize keys + normalize attribute + EARLY FILTER (detail)
# ---------------------------
changes_detail_raw[, major := str_pad(major, 6, pad = "0")]
changes_detail_raw[, minor := str_pad(minor, 4, pad = "0")]
changes_detail_raw[, parcel_id := paste0(major, "-", minor)]

# MEMORY OPTIMIZATION: filter to relevant attributes BEFORE expensive string ops.
# Do a cheap tolower + gsub on the attribute column, then filter.
# This avoids str_squish + str_replace_all on 60M rows.
changes_detail_raw[, attribute := tolower(gsub("\\s+", "_", trimws(attribute)))]

# filter to only attributes you care about
changes_detail_raw <- changes_detail_raw[attribute %chin% attrs_keep_norm]
message("  Filtered to ", nrow(changes_detail_raw), " rows matching tracked attributes")

# parse update_date AFTER filtering (big speed win)
changes_detail_raw[, update_date := parse_date_time(
  updatedate,
  orders = c("b d Y I:M p","b d Y H:M p","Y-m-d H:M:S","Y-m-d"),
  tz = "America/Los_Angeles"
)]

# clean attribute_value
changes_detail_raw[, attribute_value := str_squish(attributevalue)]
changes_detail_raw[attribute_value %in% c("", "(unknown)"), attribute_value := NA_character_]

changes_detail <- changes_detail_raw[, .(
  parcel_id, major, minor,
  event_id = eventid,
  component_id = id,
  attribute,
  attribute_value,
  update_date,
  updated_by = updatedby
)]

# ---------------------------
# 6) Join detail to header (fast keyed merge)
# ---------------------------
setkey(changes, parcel_id, major, minor, event_id)
setkey(changes_detail, parcel_id, major, minor, event_id)

changes_long <- merge(
  changes_detail, changes,
  by = c("parcel_id","major","minor","event_id"),
  all.x = TRUE,
  allow.cartesian = TRUE
)

# ---------------------------
# 7) Deduplicate (keep last update per event/attribute)
# ---------------------------
setorder(changes_long, parcel_id, event_id, component_id, attribute, update_date)
changes_long <- changes_long[, .SD[.N], by = .(parcel_id, event_id, component_id, attribute)]

# ---------------------------
# 8) Attribute counts (filtered set)
# ---------------------------
attr_counts <- changes_long[, .N, by = attribute][order(-N)]
print(attr_counts)

# ---------------------------
# 9) Write out
# ---------------------------
# ------------------------------------------------------------------
# 9) Write full changes (all property types) to CSV
# ------------------------------------------------------------------
out_path <- here("data","wrangled", paste0("changes_long_", today(), ".csv"))
fwrite(changes_long, out_path)
message("Wrote: ", out_path)

# NOTE: Do NOT filter to parcel_res_full here. changes_long_tbl must contain
# ALL property types so that apply_changes_to_panel() in main_ml.R can apply
# facet changes to commercial and condo panels as well.
# Downstream scripts (xx_combine_parcel_history_changes.R) handle the
# residential subset internally via the parcel_dt join.

changes_long_tbl <- as_tibble(changes_long)

message("  changes_long_tbl: ", nrow(changes_long_tbl), " rows (all property types)")

