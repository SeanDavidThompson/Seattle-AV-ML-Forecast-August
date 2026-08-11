# =============================================================================
# xx_costar_apt_to_panel.R
# =============================================================================
# Reads CoStar Multi-Family submarket data, annualizes quarterly observations
# to tax year, and joins to the apartment panel (panel_tbl_apt).
#
# CoStar file: "CoStar_for_AV_-_multifamily_-_full_dataset_by_submarket_-_*.xlsx"
#   Sheet "DataExport", quarterly, 9 Seattle submarkets, 73 columns.
#
# Key predictors for Apartment AV:
#   - Vacancy & occupancy rates (income approach driver)
#   - Asking/effective rent per unit (direct rent roll proxy)
#   - Cap rates (capitalization rate = AV anchor)
#   - Absorption / deliveries (supply-demand balance)
#   - Asset value & sale price per unit (comparable sales benchmarks)
#   - Rent growth indices (trend signal)
# =============================================================================

message("\n--- xx_costar_apt_to_panel.R ---")

library(data.table)
library(readxl)
library(stringr)
library(here)

source(here("scripts", "ml", "xx_costar_import_utils.R"))

if (!exists("cache_dir")) cache_dir <- here("data", "cache")
if (!exists("scenario"))  scenario  <- "baseline"

# ---- 1. Locate file ---------------------------------------------------------
costar_dir <- here("data", "costar")
apt_file <- list.files(costar_dir,
                       pattern = "multifamily.*\\.xlsx$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(apt_file) == 0)
  stop("No CoStar multifamily file found in ", costar_dir)
apt_file <- apt_file[length(apt_file)]
message("  Reading: ", basename(apt_file))

# ---- 2. Variable mapping ----------------------------------------------------
var_map_apt <- c(
  # Vacancy & occupancy
  "Vacancy Rate"                     = "cs_apt_vacancy_rate",
  "Occupancy Rate"                   = "cs_apt_occupancy_rate",
  "Stabilized Vacancy"               = "cs_apt_stabilized_vacancy",
  # Rents
  "Market Asking Rent/Unit"          = "cs_apt_asking_rent_unit",
  "Market Effective Rent/Unit"       = "cs_apt_effective_rent_unit",
  "Market Asking Rent/SF"            = "cs_apt_asking_rent_sf",
  "Market Effective Rent/SF"         = "cs_apt_effective_rent_sf",
  "Market Asking Rent Growth"        = "cs_apt_asking_rent_growth",
  "Market Effective Rent Growth"     = "cs_apt_eff_rent_growth",
  "Market Asking Rent Index"         = "cs_apt_asking_rent_index",
  # Rents by bedroom count
  "Market Asking Rent/Unit Studio"     = "cs_apt_rent_studio",
  "Market Asking Rent/Unit 1 Bedroom"  = "cs_apt_rent_1br",
  "Market Asking Rent/Unit 2 Bedroom"  = "cs_apt_rent_2br",
  "Market Asking Rent/Unit 3 Bedroom"  = "cs_apt_rent_3br",
  "Market Effective Rent/Unit Studio"     = "cs_apt_eff_rent_studio",
  "Market Effective Rent/Unit 1 Bedroom"  = "cs_apt_eff_rent_1br",
  "Market Effective Rent/Unit 2 Bedroom"  = "cs_apt_eff_rent_2br",
  "Market Effective Rent/Unit 3 Bedroom"  = "cs_apt_eff_rent_3br",
  # Cap rates & returns
  "Cap Rate"                         = "cs_apt_cap_rate",
  "Market Cap Rate"                  = "cs_apt_market_cap_rate",
  "Median Cap Rate"                  = "cs_apt_median_cap_rate",
  "Capital Value Index"              = "cs_apt_cap_value_index",
  "NOI Index"                        = "cs_apt_noi_index",
  "Total Return"                     = "cs_apt_total_return",
  "Appreciation Return"              = "cs_apt_appreciation_return",
  "Income Return"                    = "cs_apt_income_return",
  # Pricing
  "Asset Value"                      = "cs_apt_asset_value",
  "Market Sale Price Per Unit"       = "cs_apt_sale_price_unit",
  "Market Sale Price Index"          = "cs_apt_sale_price_index",
  "Market Sale Price Growth"         = "cs_apt_sale_price_growth",
  "Median Price/Unit"                = "cs_apt_median_price_unit",
  "Median Price/Bldg SF"             = "cs_apt_median_price_sf",
  # Supply & demand (flow variables)
  "Absorption %"                     = "cs_apt_absorption_pct",
  "Absorption Units"                 = "cs_apt_absorption_units",
  "Net Delivered Units"              = "cs_apt_net_delivered_units",
  "Construction Starts Units"        = "cs_apt_constr_starts_units",
  "Under Construction Units"         = "cs_apt_under_constr_units",
  "Demand Units"                     = "cs_apt_demand_units",
  "Inventory Units"                  = "cs_apt_inventory_units",
  # Demographics
  "Total Employment"                 = "cs_apt_total_employment",
  "Office Employment"                = "cs_apt_office_employment",
  "Industrial Employment"            = "cs_apt_industrial_employment",
  "Population"                       = "cs_apt_population",
  "Median Household Income"          = "cs_apt_median_hh_income",
  "Households"                       = "cs_apt_households"
)

flow_cols_apt <- c(
  "cs_apt_absorption_units",
  "cs_apt_net_delivered_units",
  "cs_apt_constr_starts_units",
  "cs_apt_demand_units"
)

# ---- 3. Read & annualize ----------------------------------------------------
raw_apt <- read_costar_quarterly(apt_file, var_map = var_map_apt)
val_cols <- intersect(unname(var_map_apt), names(raw_apt))

annual_apt <- annualize_quarterly(raw_apt, val_cols,
                                  flow_cols = intersect(flow_cols_apt, val_cols))

message("  Annualized: ", scales::comma(nrow(annual_apt)), " submarket-year rows")
message("  Year range: ", min(annual_apt$tax_yr), "-", max(annual_apt$tax_yr))

# ---- 4. Add city-wide aggregate & cache -------------------------------------
costar_apt_wide <- add_city_wide(annual_apt, val_cols)

cache_name <- paste0("costar_apt_fcst_", min(annual_apt$tax_yr), "_",
                     max(annual_apt$tax_yr), "_", scenario, ".rds")
saveRDS(costar_apt_wide, file.path(cache_dir, cache_name))
assign("costar_apt_wide", costar_apt_wide, envir = .GlobalEnv)
message("  \U1f4be cached: ", cache_name)

# ---- 5. Join to panel -------------------------------------------------------
join_costar_to_panel("panel_tbl_apt", costar_apt_wide, "cs_apt_")

rm(raw_apt, annual_apt)
gc(verbose = FALSE)
message("--- xx_costar_apt_to_panel.R complete ---\n")
