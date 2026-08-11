# =============================================================================
# xx_costar_import_utils.R
# =============================================================================
# Shared helper functions for importing CoStar submarket data across all
# commercial property types: multifamily, office, industrial, retail,
# hospitality.
#
# Each property type has its own import script (xx_costar_<type>_to_panel.R)
# that calls these shared functions with type-specific column mappings.
#
# Two data formats are supported:
#   1. Quarterly  ("YYYY QN")  —  office, industrial, retail, multifamily
#   2. Monthly    (datetime)   —  hospitality
#
# Annualization rule: tax year N uses Q1 (or January) values for stock/rate
# variables, and trailing 4-quarter (or 12-month) sums for flow variables.
# =============================================================================

# ---- read_costar_quarterly() ------------------------------------------------
# Reads a quarterly CoStar DataExport sheet and returns a cleaned data.table
# with year, qtr, submarket, and renamed value columns.
# Arguments:
#   file_path   — path to the .xlsx file
#   sheet_name  — sheet name (default "DataExport")
#   var_map     — named character vector: original_name = new_name
#   geo_col     — column containing geography name
#   prefix_strip — regex to strip from geography name
# Returns: data.table with columns: year, qtr, submarket, <new_names>
# --------------------------------------------------------------------------
read_costar_quarterly <- function(file_path,
                                  sheet_name   = "DataExport",
                                  var_map,
                                  geo_col      = "Geography Name",
                                  prefix_strip = "^Seattle - WA USA - ") {
  raw <- data.table::as.data.table(
    readxl::read_excel(file_path, sheet = sheet_name)
  )
  message("    Raw: ", scales::comma(nrow(raw)), " rows, ",
          ncol(raw), " cols, ",
          data.table::uniqueN(raw[[geo_col]]), " submarkets")

  # Parse "YYYY QN" period
  raw[, period_str := as.character(Period)]
  raw[, year := as.integer(stringr::str_extract(period_str, "^\\d{4}"))]
  raw[, qtr  := as.integer(stringr::str_extract(period_str, "(?<=Q)\\d"))]
  raw <- raw[!is.na(year) & !is.na(qtr)]

  # Clean submarket
  raw[, submarket := stringr::str_replace(get(geo_col), prefix_strip, "")]

  # Select and rename
  avail <- intersect(names(var_map), names(raw))
  if (length(avail) == 0)
    stop("No expected columns matched in ", basename(file_path))
  message("    Matched ", length(avail), " / ", length(var_map), " variables")

  out <- raw[, c("year", "qtr", "submarket", avail), with = FALSE]
  data.table::setnames(out, avail, var_map[avail])

  # Coerce to numeric
  for (col in var_map[avail])
    out[, (col) := as.numeric(get(col))]

  out
}

# ---- read_costar_monthly() --------------------------------------------------
# Reads a monthly CoStar AnalyticExport sheet (hospitality uses datetimes).
# Returns: data.table with year, month, submarket, <new_names>
# --------------------------------------------------------------------------
read_costar_monthly <- function(file_path,
                                sheet_name   = "AnalyticExport",
                                var_map,
                                geo_col      = "Geography Name",
                                prefix_strip = "^Seattle - WA USA - ") {
  raw <- data.table::as.data.table(
    readxl::read_excel(file_path, sheet = sheet_name)
  )
  message("    Raw: ", scales::comma(nrow(raw)), " rows, ",
          ncol(raw), " cols, ",
          data.table::uniqueN(raw[[geo_col]]), " submarkets")

  # Period is a datetime — extract year and month
  raw[, period_dt := as.Date(Period)]
  raw[, year  := as.integer(format(period_dt, "%Y"))]
  raw[, month := as.integer(format(period_dt, "%m"))]
  raw <- raw[!is.na(year) & !is.na(month)]

  # Clean submarket
  raw[, submarket := stringr::str_replace(get(geo_col), prefix_strip, "")]

  # Select and rename
  avail <- intersect(names(var_map), names(raw))
  if (length(avail) == 0)
    stop("No expected columns matched in ", basename(file_path))
  message("    Matched ", length(avail), " / ", length(var_map), " variables")

  out <- raw[, c("year", "month", "submarket", avail), with = FALSE]
  data.table::setnames(out, avail, var_map[avail])

  for (col in var_map[avail])
    out[, (col) := as.numeric(get(col))]

  out
}

# ---- annualize_quarterly() --------------------------------------------------
# Annualizes quarterly CoStar data to tax-year level.
# Stock/rate vars: Q1 snapshot.
# Flow vars: trailing 4-quarter sum (Q2 prior year through Q1 current year).
# Arguments:
#   dt         — data.table from read_costar_quarterly()
#   val_cols   — all value column names
#   flow_cols  — subset of val_cols that are flow variables
# Returns: data.table with tax_yr, submarket, <val_cols>
# --------------------------------------------------------------------------
annualize_quarterly <- function(dt, val_cols, flow_cols = character(0)) {
  stock_cols <- setdiff(val_cols, flow_cols)

  # Q1 snapshot for stock/rate variables
  q1 <- dt[qtr == 1L]
  q1[, tax_yr := year]

  # Flow variables: sum Q2(yr-1) + Q3(yr-1) + Q4(yr-1) + Q1(yr)
  if (length(flow_cols) > 0 && any(flow_cols %in% names(dt))) {
    flow_present <- intersect(flow_cols, names(dt))
    flow_dt <- dt[, c("year", "qtr", "submarket", flow_present), with = FALSE]
    flow_dt[, tax_yr := data.table::fifelse(qtr == 1L, year, year + 1L)]

    flow_annual <- flow_dt[, lapply(.SD, sum, na.rm = TRUE),
                           by = .(tax_yr, submarket),
                           .SDcols = flow_present]

    # Only keep years with all 4 quarters
    qtr_count <- flow_dt[, .N, by = .(tax_yr, submarket)]
    full_yrs  <- qtr_count[N == 4L, .(tax_yr, submarket)]
    flow_annual <- flow_annual[full_yrs, on = .(tax_yr, submarket), nomatch = NULL]

    q1 <- merge(q1, flow_annual, by = c("tax_yr", "submarket"),
                all.x = TRUE, suffixes = c("", "_flow"))
    for (fc in flow_present) {
      fn <- paste0(fc, "_flow")
      if (fn %in% names(q1))
        q1[, (fc) := get(fn)][, (fn) := NULL]
    }
  }

  q1[, c("year", "qtr") := NULL]
  if ("period_str" %in% names(q1)) q1[, period_str := NULL]
  q1
}

# ---- annualize_monthly() ----------------------------------------------------
# Annualizes monthly CoStar data to tax-year level.
# Stock/rate vars: January (month 1) snapshot.
# Flow vars: trailing 12-month sum (Feb prior year through Jan current year).
# --------------------------------------------------------------------------
annualize_monthly <- function(dt, val_cols, flow_cols = character(0)) {
  stock_cols <- setdiff(val_cols, flow_cols)

  # January snapshot for stock/rate variables
  jan <- dt[month == 1L]
  jan[, tax_yr := year]

  # Flow variables: sum M2(yr-1) through M1(yr) → 12 months ending Jan
  if (length(flow_cols) > 0 && any(flow_cols %in% names(dt))) {
    flow_present <- intersect(flow_cols, names(dt))
    flow_dt <- dt[, c("year", "month", "submarket", flow_present), with = FALSE]
    flow_dt[, tax_yr := data.table::fifelse(month == 1L, year, year + 1L)]

    flow_annual <- flow_dt[, lapply(.SD, sum, na.rm = TRUE),
                           by = .(tax_yr, submarket),
                           .SDcols = flow_present]

    month_count <- flow_dt[, .N, by = .(tax_yr, submarket)]
    full_yrs    <- month_count[N == 12L, .(tax_yr, submarket)]
    flow_annual <- flow_annual[full_yrs, on = .(tax_yr, submarket), nomatch = NULL]

    jan <- merge(jan, flow_annual, by = c("tax_yr", "submarket"),
                 all.x = TRUE, suffixes = c("", "_flow"))
    for (fc in flow_present) {
      fn <- paste0(fc, "_flow")
      if (fn %in% names(jan))
        jan[, (fc) := get(fn)][, (fn) := NULL]
    }
  }

  jan[, c("year", "month") := NULL]
  if ("period_dt" %in% names(jan)) jan[, period_dt := NULL]
  jan
}

# ---- add_city_wide() --------------------------------------------------------
# Appends a CITY_WIDE row (aggregated across all submarkets) for each tax_yr.
# Rates/indices are averaged; counts/volumes are summed.
# --------------------------------------------------------------------------
add_city_wide <- function(dt, val_cols) {
  rate_cols  <- grep("rate|pct|growth|return|index|rent|cap|price|adr|revpar|occupancy",
                     val_cols, ignore.case = TRUE, value = TRUE)
  count_cols <- setdiff(val_cols, rate_cols)

  agg_list <- list()
  if (length(rate_cols) > 0)
    agg_list <- c(agg_list, lapply(
      stats::setNames(rate_cols, rate_cols),
      function(col) substitute(mean(x, na.rm = TRUE), list(x = as.name(col)))
    ))
  if (length(count_cols) > 0)
    agg_list <- c(agg_list, lapply(
      stats::setNames(count_cols, count_cols),
      function(col) substitute(sum(x, na.rm = TRUE), list(x = as.name(col)))
    ))

  # Simpler approach
  city <- dt[, c(
    if (length(rate_cols) > 0)
      lapply(.SD[, ..rate_cols], mean, na.rm = TRUE),
    if (length(count_cols) > 0)
      lapply(.SD[, ..count_cols], sum, na.rm = TRUE)
  ), by = tax_yr]
  city[, submarket := "CITY_WIDE"]

  rbind(dt, city, fill = TRUE)
}

# ---- join_costar_to_panel() -------------------------------------------------
# Joins annualized CoStar data to a panel data.table.
# Uses CITY_WIDE aggregate for all parcels (can be refined with
# a submarket crosswalk when geocoding is available).
# Arguments:
#   panel_name  — name of panel object in .GlobalEnv (e.g. "panel_tbl_office")
#   cs_wide     — annualized CoStar data.table with submarket + CITY_WIDE
#   cs_prefix   — column prefix to identify CoStar columns (e.g. "cs_off_")
# --------------------------------------------------------------------------
join_costar_to_panel <- function(panel_name, cs_wide, cs_prefix) {
  if (!exists(panel_name, envir = .GlobalEnv)) {
    message("  \u2139\ufe0f  ", panel_name, " not in memory — skipping join")
    return(invisible(NULL))
  }

  panel <- get(panel_name, envir = .GlobalEnv)
  data.table::setDT(panel)

  # Use city-wide for all parcels
  cs_join <- cs_wide[submarket == "CITY_WIDE"]
  cs_join[, submarket := NULL]

  # Drop existing CoStar columns
  existing_cs <- grep(paste0("^", cs_prefix), names(panel), value = TRUE)
  if (length(existing_cs) > 0) {
    panel[, (existing_cs) := NULL]
    message("  Dropped ", length(existing_cs), " existing ", cs_prefix, "* columns")
  }

  val_cols <- setdiff(names(cs_join), "tax_yr")
  panel <- merge(panel, cs_join, by = "tax_yr", all.x = TRUE)

  assign(panel_name, panel, envir = .GlobalEnv)
  coverage <- sum(!is.na(panel[[val_cols[1]]]))
  message("  \u2705 joined ", length(val_cols), " CoStar variables to ", panel_name)
  message("     Coverage: ", scales::comma(coverage),
          " / ", scales::comma(nrow(panel)), " rows")

  invisible(NULL)
}

message("  \u2705 xx_costar_import_utils.R loaded")
