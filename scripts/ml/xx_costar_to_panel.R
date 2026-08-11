# xx_costar_to_panel.R — attach CoStar market variables to panel_tbl_com.

stopifnot(exists("panel_tbl_com", envir = .GlobalEnv))

scenario  <- get("scenario",  envir = .GlobalEnv)
cache_dir <- get("cache_dir", envir = .GlobalEnv)

# ---- Locate input file ------------------------------------------------------
costar_dir   <- here::here("data", "costar")

costar_csv <- list.files(
  costar_dir,
  pattern    = paste0("costar_q_.*_", scenario, "\\.csv$"),
  full.names = TRUE
)
costar_rds <- list.files(
  costar_dir,
  pattern    = paste0("costar_q_.*_", scenario, "\\.rds$"),
  full.names = TRUE
)

# ---- Graceful fallback ------------------------------------------------------
if (length(costar_csv) == 0 && length(costar_rds) == 0) {
  warning(
    "\n⚠️  No CoStar file found for scenario '", scenario, "'.\n",
    "Expected pattern in: ", costar_dir, "\n",
    "  costar_q_<date>_", scenario, ".csv\n",
    "Commercial panel will have NA for all costar_* market columns.\n",
    "Place the file and re-run with extend_replicate=TRUE."
  )
  stub_cols <- c(
    "costar_vr_nro",       "costar_vr_mf",       "costar_ar_mf",
    "costar_vr_nro_lag1",  "costar_vr_nro_lag2",
    "costar_vr_mf_lag1",   "costar_vr_mf_lag2",
    "costar_ar_mf_lag1",   "costar_ar_mf_lag2",
    "costar_vr_nro_delta", "costar_vr_mf_delta", "costar_ar_mf_delta"
  )
  panel_com <- get("panel_tbl_com", envir = .GlobalEnv)
  setDT(panel_com)
  for (col in stub_cols) panel_com[, (col) := NA_real_]
  panel_com[, costar_geo := NA_character_]
  assign("panel_tbl_com", panel_com,    envir = .GlobalEnv)
  assign("costar_fcst",   data.table(), envir = .GlobalEnv)
  assign("costar_annual", data.table(), envir = .GlobalEnv)
  message("xx_costar_to_panel.R loaded (scenario=", scenario,
          ") — no CoStar data, stub NAs added.")
  return(invisible(NULL))
}

# ---- Load -------------------------------------------------------------------
if (length(costar_rds) > 0) {
  f <- costar_rds[which.max(file.info(costar_rds)$mtime)]
  message("  Reading CoStar from RDS: ", basename(f))
  raw <- as.data.table(readRDS(f))
} else {
  f <- costar_csv[which.max(file.info(costar_csv)$mtime)]
  message("  Reading CoStar from CSV: ", basename(f))
  raw <- as.data.table(
    read_csv(f, show_col_types = FALSE, na = c("", "NA"))
  )
}
message("  CoStar raw: ", nrow(raw), " rows  |  cols: ",
        paste(names(raw), collapse = ", "))

# ---- Parse yearq → tax_yr + quarter ----------------------------------------
# "1990 Q1" → tax_yr=1990, quarter=1
raw[, tax_yr  := as.integer(str_extract(yearq, "^\\d{4}"))]
raw[, quarter := as.integer(str_extract(yearq, "\\d$"))]

# ---- Coerce all-lgl columns to numeric (AVNRO / AVRMF — all NA in data) ---
lgl_cols <- names(raw)[vapply(raw, is.logical, logical(1))]
for (col in lgl_cols) raw[, (col) := as.numeric(get(col))]

# ---- Geography waterfall helper --------------------------------------------
# Returns best available value and a label showing which geo was used.
geo_waterfall <- function(sea, ksp, us) {
  val <- fifelse(!is.na(sea), sea,
         fifelse(!is.na(ksp), ksp,
                 us))
  geo <- fifelse(!is.na(sea), "SEA",
         fifelse(!is.na(ksp), "KSP",
         fifelse(!is.na(us),  "US",
                 NA_character_)))
  list(val = val, geo = geo)
}

# VR_NRO  (office/NRO vacancy rate)
vr_nro <- geo_waterfall(
  raw[["SEA_VRNRO"]], raw[["KSP_VRNRO"]], raw[["US_VRNRO"]]
)
raw[, vr_nro_q  := vr_nro$val]
raw[, vr_nro_geo := vr_nro$geo]

# VR_RMF  (multifamily vacancy rate)
vr_mf <- geo_waterfall(
  raw[["SEA_VRRMF"]], raw[["KSP_VRRMF"]], raw[["US_VRRMF"]]
)
raw[, vr_mf_q := vr_mf$val]

# AR_RMF  (multifamily asking rent $/unit/month)
ar_mf <- geo_waterfall(
  raw[["SEA_ARRMF"]], raw[["KSP_ARRMF"]], raw[["US_ARRMF"]]
)
raw[, ar_mf_q := ar_mf$val]

message("  Geography used for NRO vacancy rate:")
print(raw[!is.na(vr_nro_geo), .N, by = vr_nro_geo])

# ---- Annual averages --------------------------------------------------------
# Mean of available quarters within each calendar year.
# Partial years (e.g. forecast year with only 1 quarter) use what's available.
costar_annual <- raw[, .(
  costar_vr_nro  = mean(vr_nro_q, na.rm = TRUE),
  costar_vr_mf   = mean(vr_mf_q,  na.rm = TRUE),
  costar_ar_mf   = mean(ar_mf_q,  na.rm = TRUE),
  costar_geo     = vr_nro_geo[!is.na(vr_nro_q)][1L],  # geo of first non-NA Q
  n_quarters     = sum(!is.na(vr_nro_q))
), by = tax_yr]

# NaN → NA (years where all quarters are NA)
for (col in c("costar_vr_nro","costar_vr_mf","costar_ar_mf")) {
  costar_annual[is.nan(get(col)), (col) := NA_real_]
}

setorder(costar_annual, tax_yr)

message("  Annual series: ", nrow(costar_annual), " years  (",
        min(costar_annual$tax_yr), " – ", max(costar_annual$tax_yr), ")")
message("  Last 5 years with data:")
print(costar_annual[!is.na(costar_vr_nro)][
  (.N - 4):.N,
  .(tax_yr, costar_vr_nro, costar_vr_mf, costar_ar_mf, costar_geo, n_quarters)
])

# ---- Lags (shift on ordered series) ----------------------------------------
setorder(costar_annual, tax_yr)

costar_annual[, costar_vr_nro_lag1 := shift(costar_vr_nro, 1L)]
costar_annual[, costar_vr_nro_lag2 := shift(costar_vr_nro, 2L)]
costar_annual[, costar_vr_mf_lag1  := shift(costar_vr_mf,  1L)]
costar_annual[, costar_vr_mf_lag2  := shift(costar_vr_mf,  2L)]
costar_annual[, costar_ar_mf_lag1  := shift(costar_ar_mf,  1L)]
costar_annual[, costar_ar_mf_lag2  := shift(costar_ar_mf,  2L)]

# ---- YoY deltas ------------------------------------------------------------
costar_annual[, costar_vr_nro_delta := costar_vr_nro - costar_vr_nro_lag1]
costar_annual[, costar_vr_mf_delta  := costar_vr_mf  - costar_vr_mf_lag1]
costar_annual[, costar_ar_mf_delta  := costar_ar_mf  - costar_ar_mf_lag1]

# ---- Merge onto panel_tbl_com ----------------------------------------------
panel_com <- copy(get("panel_tbl_com", envir = .GlobalEnv))
setDT(panel_com)

# Drop any stale costar_ cols from a prior run of this script
old_cols <- grep("^costar_", names(panel_com), value = TRUE)
if (length(old_cols) > 0) panel_com[, (old_cols) := NULL]

merge_cols <- setdiff(names(costar_annual), "n_quarters")
panel_com  <- merge(panel_com,
                    costar_annual[, ..merge_cols],
                    by = "tax_yr", all.x = TRUE)

setDT(panel_com)
setkeyv(panel_com, c("parcel_id", "tax_yr"))

# ---- Coverage check --------------------------------------------------------
costar_added <- grep("^costar_", names(panel_com), value = TRUE)
message("  costar_* columns attached (", length(costar_added), "):\n    ",
        paste(costar_added, collapse = ", "))

chk <- panel_com[tax_yr >= 2005 & tax_yr <= 2025 & !is.na(costar_vr_nro),
                  uniqueN(tax_yr)]
message("  Years 2005-2025 with non-NA costar_vr_nro: ", chk)

# ---- Cache -----------------------------------------------------------------
# Annual series → used by 05_extend and 06_forecast_comm
cache_ann <- file.path(
  cache_dir,
  paste0("costar_fcst_",
         min(costar_annual$tax_yr), "_",
         max(costar_annual$tax_yr), "_",
         scenario, ".rds")
)
saveRDS(costar_annual, cache_ann)
message("  💾 CoStar annual cached: ", basename(cache_ann))

# Raw quarterly → reference / diagnostics
saveRDS(raw, file.path(cache_dir,
                        paste0("costar_raw_qtrly_", scenario, ".rds")))

# ---- Export to GlobalEnv ---------------------------------------------------
assign("panel_tbl_com", panel_com,     envir = .GlobalEnv)
assign("costar_fcst",   costar_annual, envir = .GlobalEnv)
assign("costar_annual", costar_annual, envir = .GlobalEnv)

message("xx_costar_to_panel.R loaded (scenario = ", scenario, ")")
