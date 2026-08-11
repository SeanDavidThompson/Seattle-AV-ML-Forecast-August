# =============================================================================
# xx_costar_retail_to_panel.R
# =============================================================================
# Reads CoStar Retail submarket data, annualizes to tax year, and joins to
# the retail panel (panel_tbl_retail).
#
# CoStar file: "CoStar_for_AV_-_retail_-_full_dataset_by_submarket_-_*.xlsx"
#   Sheet "DataExport", quarterly, 9 Seattle submarkets, 81 columns.
#
# Key predictors for Retail AV:
#   - NNN rent (most retail leases are triple-net)
#   - Vacancy & availability (storefront vacancy is a major AV driver)
#   - Cap rates (retail cap rates tend to be higher, reflecting risk)
#   - Net absorption & deliveries (supply signal)
#   - Population / households (demand proxy for neighborhood retail)
# =============================================================================

message("\n--- xx_costar_retail_to_panel.R ---")

library(data.table)
library(readxl)
library(stringr)
library(here)

source(here("scripts", "ml", "xx_costar_import_utils.R"))

if (!exists("cache_dir")) cache_dir <- here("data", "cache")
if (!exists("scenario"))  scenario  <- "baseline"

# ---- 1. Locate file ---------------------------------------------------------
costar_dir <- here("data", "costar")
ret_file <- list.files(costar_dir,
                       pattern = "retail.*\\.xlsx$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(ret_file) == 0)
  stop("No CoStar retail file found in ", costar_dir)
ret_file <- ret_file[length(ret_file)]
message("  Reading: ", basename(ret_file))

# ---- 2. Variable mapping ----------------------------------------------------
var_map_retail <- c(
  # Vacancy & occupancy
  "Vacancy Rate"                     = "cs_ret_vacancy_rate",
  "Occupancy Rate"                   = "cs_ret_occupancy_rate",
  "Availability Rate"                = "cs_ret_availability_rate",
  "Availability Rate % Direct"       = "cs_ret_avail_rate_direct",
  "Availability Rate % Sublet"       = "cs_ret_avail_rate_sublet",
  # Rents
  "NNN Rent Overall"                 = "cs_ret_nnn_rent",
  "NNN Rent Direct"                  = "cs_ret_nnn_rent_direct",
  "All Service Type Rent Overall"    = "cs_ret_all_svc_rent",
  "All Service Type Rent Direct"     = "cs_ret_all_svc_rent_direct",
  "Market Asking Rent/SF"            = "cs_ret_asking_rent_sf",
  "Market Asking Rent Growth"        = "cs_ret_asking_rent_growth",
  "Market Asking Rent Index"         = "cs_ret_asking_rent_index",
  # Cap rates & returns
  "Cap Rate"                         = "cs_ret_cap_rate",
  "Market Cap Rate"                  = "cs_ret_market_cap_rate",
  "Median Cap Rate"                  = "cs_ret_median_cap_rate",
  "Capital Value Index"              = "cs_ret_cap_value_index",
  "NOI Index"                        = "cs_ret_noi_index",
  "Total Return"                     = "cs_ret_total_return",
  "Appreciation Return"              = "cs_ret_appreciation_return",
  "Income Return"                    = "cs_ret_income_return",
  # Pricing
  "Asset Value"                      = "cs_ret_asset_value",
  "Market Sale Price Per SF"         = "cs_ret_sale_price_sf",
  "Market Sale Price Index"          = "cs_ret_sale_price_index",
  "Market Sale Price Growth"         = "cs_ret_sale_price_growth",
  "Median Price/Bldg SF"             = "cs_ret_median_price_sf",
  # Supply & demand
  "Net Absorption SF"                = "cs_ret_net_absorption_sf",
  "Net Delivered SF"                 = "cs_ret_net_delivered_sf",
  "Construction Starts SF"           = "cs_ret_constr_starts_sf",
  "Under Construction SF"            = "cs_ret_under_constr_sf",
  "Inventory SF"                     = "cs_ret_inventory_sf",
  "Demand SF"                        = "cs_ret_demand_sf",
  # Available / vacant space
  "Available SF Total"               = "cs_ret_available_sf",
  "Vacant SF Total"                  = "cs_ret_vacant_sf",
  # Leasing
  "Leasing SF Total"                 = "cs_ret_leasing_sf",
  # Demographics
  "Total Employment"                 = "cs_ret_total_employment",
  "Population"                       = "cs_ret_population",
  "Median Household Income"          = "cs_ret_median_hh_income",
  "Households"                       = "cs_ret_households"
)

flow_cols_retail <- c(
  "cs_ret_net_absorption_sf",
  "cs_ret_net_delivered_sf",
  "cs_ret_constr_starts_sf",
  "cs_ret_demand_sf",
  "cs_ret_leasing_sf"
)

# ---- 3. Read & annualize ----------------------------------------------------
raw_ret <- read_costar_quarterly(ret_file, var_map = var_map_retail)
val_cols <- intersect(unname(var_map_retail), names(raw_ret))

annual_ret <- annualize_quarterly(raw_ret, val_cols,
                                  flow_cols = intersect(flow_cols_retail, val_cols))

message("  Annualized: ", scales::comma(nrow(annual_ret)), " submarket-year rows")
message("  Year range: ", min(annual_ret$tax_yr), "-", max(annual_ret$tax_yr))

# ---- 4. Add city-wide aggregate & cache -------------------------------------
costar_retail_wide <- add_city_wide(annual_ret, val_cols)

cache_name <- paste0("costar_retail_fcst_", min(annual_ret$tax_yr), "_",
                     max(annual_ret$tax_yr), "_", scenario, ".rds")
saveRDS(costar_retail_wide, file.path(cache_dir, cache_name))
assign("costar_retail_wide", costar_retail_wide, envir = .GlobalEnv)
message("  \U1f4be cached: ", cache_name)

# ---- 5. Join to panel -------------------------------------------------------
join_costar_to_panel("panel_tbl_retail", costar_retail_wide, "cs_ret_")

rm(raw_ret, annual_ret)
gc(verbose = FALSE)
message("--- xx_costar_retail_to_panel.R complete ---\n")
