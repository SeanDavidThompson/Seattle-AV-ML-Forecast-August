# =============================================================================
# xx_costar_hospitality_to_panel.R
# =============================================================================
# Reads CoStar Hospitality submarket data, annualizes monthly observations
# to tax year, and joins to the hospitality panel (panel_tbl_hospitality).
#
# CoStar file: "CoStar_for_AV_-_hospitality_-_full_dataset_by_submarket_-_*.xlsx"
#   Sheet "AnalyticExport", MONTHLY (datetime), 1 submarket (Seattle CBD),
#   81 columns.
#
# NOTE: Unlike other property types, hospitality uses monthly frequency
# and has hotel-specific metrics (ADR, RevPAR, occupancy, supply/demand
# in room-nights).  The AnalyticExport sheet has a different column layout
# than the DataExport sheets used by office/industrial/retail.
#
# Key predictors for Hospitality AV:
#   - ADR (average daily rate) — primary revenue driver
#   - RevPAR (revenue per available room) — blends occupancy and ADR
#   - Occupancy rate — utilization
#   - Market cap rate / price per room — valuation benchmarks
#   - Supply (room inventory) & demand (room-nights sold)
# =============================================================================

message("\n--- xx_costar_hospitality_to_panel.R ---")

library(data.table)
library(readxl)
library(stringr)
library(here)

source(here("scripts", "ml", "xx_costar_import_utils.R"))

if (!exists("cache_dir")) cache_dir <- here("data", "cache")
if (!exists("scenario"))  scenario  <- "baseline"

# ---- 1. Locate file ---------------------------------------------------------
costar_dir <- here("data", "costar")
hosp_file <- list.files(costar_dir,
                        pattern = "hospitality.*\\.xlsx$",
                        full.names = TRUE, ignore.case = TRUE)
if (length(hosp_file) == 0)
  stop("No CoStar hospitality file found in ", costar_dir)
hosp_file <- hosp_file[length(hosp_file)]
message("  Reading: ", basename(hosp_file))

# ---- 2. Variable mapping ----------------------------------------------------
# Hospitality uses different column names than office/industrial/retail.
# The "12 Mo" prefixed columns are trailing-12-month aggregates computed by
# CoStar — we read them directly rather than summing monthly values.
var_map_hospitality <- c(
  # Rate / price metrics (monthly point-in-time → January snapshot)
  "ADR"                              = "cs_hosp_adr",
  "RevPAR"                           = "cs_hosp_revpar",
  "Occupancy"                        = "cs_hosp_occupancy",
  "Market Cap Rate"                  = "cs_hosp_market_cap_rate",
  "Market Sale Price/Room"           = "cs_hosp_sale_price_room",
  "Market Sale Price/Room Growth"    = "cs_hosp_sale_price_room_growth",
  "Market Cap Rate Growth"           = "cs_hosp_cap_rate_growth",
  "Asset Value"                      = "cs_hosp_asset_value",
  "Avg Rooms Per Building"           = "cs_hosp_avg_rooms_bldg",
  "Inventory Rooms"                  = "cs_hosp_inventory_rooms",
  "Existing Buildings"               = "cs_hosp_existing_bldgs",
  "Under Construction Buildings"     = "cs_hosp_under_constr_bldgs",
  "Under Construction Rooms"         = "cs_hosp_under_constr_rooms",
  # Trailing 12-month aggregates (pre-computed by CoStar)
  "12 Mo ADR"                        = "cs_hosp_12m_adr",
  "12 Mo RevPAR"                     = "cs_hosp_12m_revpar",
  "12 Mo Occupancy"                  = "cs_hosp_12m_occupancy",
  "12 Mo Revenue"                    = "cs_hosp_12m_revenue",
  "12 Mo Demand"                     = "cs_hosp_12m_demand",
  "12 Mo Supply"                     = "cs_hosp_12m_supply",
  "12 Mo ADR Chg"                    = "cs_hosp_12m_adr_chg",
  "12 Mo RevPAR Chg"                 = "cs_hosp_12m_revpar_chg",
  "12 Mo Occupancy Chg"              = "cs_hosp_12m_occupancy_chg",
  "12 Mo Revenue Chg"                = "cs_hosp_12m_revenue_chg",
  "12 Mo Demand Chg"                 = "cs_hosp_12m_demand_chg",
  "12 Mo Supply Chg"                 = "cs_hosp_12m_supply_chg",
  "12 Mo Delivered Rooms"            = "cs_hosp_12m_delivered_rooms",
  "12 Mo Inventory Growth"           = "cs_hosp_12m_inventory_growth",
  "12 Mo Sales Volume"               = "cs_hosp_12m_sales_volume",
  "12 Mo Transactions"               = "cs_hosp_12m_transactions",
  # YoY change metrics
  "ADR Chg (YOY)"                    = "cs_hosp_adr_chg_yoy",
  "RevPAR Chg (YOY)"                 = "cs_hosp_revpar_chg_yoy",
  "Occupancy Chg (YOY)"              = "cs_hosp_occupancy_chg_yoy",
  "Revenue Chg (YOY)"                = "cs_hosp_revenue_chg_yoy",
  "Demand Chg (YOY)"                 = "cs_hosp_demand_chg_yoy",
  "Supply Chg (YOY)"                 = "cs_hosp_supply_chg_yoy",
  # Monthly flow
  "Revenue"                          = "cs_hosp_revenue",
  "Demand"                           = "cs_hosp_demand",
  "Supply"                           = "cs_hosp_supply"
)

# All hospitality metrics that represent flows (monthly → annual sum)
# The "12 Mo" versions are already annualized by CoStar, so we DON'T sum them.
# We only sum raw monthly Revenue, Demand, Supply.
flow_cols_hospitality <- c(
  "cs_hosp_revenue",
  "cs_hosp_demand",
  "cs_hosp_supply"
)

# ---- 3. Read & annualize (monthly → tax year) -------------------------------
raw_hosp <- read_costar_monthly(hosp_file,
                                sheet_name = "AnalyticExport",
                                var_map = var_map_hospitality)
val_cols <- intersect(unname(var_map_hospitality), names(raw_hosp))

annual_hosp <- annualize_monthly(raw_hosp, val_cols,
                                 flow_cols = intersect(flow_cols_hospitality, val_cols))

message("  Annualized: ", scales::comma(nrow(annual_hosp)), " submarket-year rows")
message("  Year range: ", min(annual_hosp$tax_yr), "-", max(annual_hosp$tax_yr))

# ---- 4. Add city-wide aggregate & cache -------------------------------------
# Hospitality only has 1 submarket (Seattle CBD), so city-wide = that submarket.
# We still add the CITY_WIDE row for consistency with other scripts.
costar_hospitality_wide <- add_city_wide(annual_hosp, val_cols)

cache_name <- paste0("costar_hospitality_fcst_", min(annual_hosp$tax_yr), "_",
                     max(annual_hosp$tax_yr), "_", scenario, ".rds")
saveRDS(costar_hospitality_wide, file.path(cache_dir, cache_name))
assign("costar_hospitality_wide", costar_hospitality_wide, envir = .GlobalEnv)
message("  \U1f4be cached: ", cache_name)

# ---- 5. Join to panel -------------------------------------------------------
join_costar_to_panel("panel_tbl_hospitality", costar_hospitality_wide, "cs_hosp_")

rm(raw_hosp, annual_hosp)
gc(verbose = FALSE)
message("--- xx_costar_hospitality_to_panel.R complete ---\n")
