# =============================================================================
# xx_costar_office_to_panel.R
# =============================================================================
# Reads CoStar Office submarket data, annualizes to tax year, and joins to
# the major office panel (panel_tbl_office).
#
# CoStar file: "CoStar_for_AV_-_office_-_full_dataset_by_submarket_-_*.xlsx"
#   Sheet "DataExport", quarterly, 9 Seattle submarkets, 80 columns.
#
# Key predictors for Major Office AV:
#   - Vacancy & availability rates (driver of net operating income)
#   - Asking/gross rents (direct income approach input)
#   - Cap rates (capitalization rate = AV anchor)
#   - Net absorption (demand signal)
#   - Asset value & sale price indices (market value benchmarks)
#   - Under construction / deliveries (supply pipeline)
# =============================================================================

message("\n--- xx_costar_office_to_panel.R ---")

library(data.table)
library(readxl)
library(stringr)
library(here)

source(here("scripts", "ml", "xx_costar_import_utils.R"))

if (!exists("cache_dir")) cache_dir <- here("data", "cache")
if (!exists("scenario"))  scenario  <- "baseline"

# ---- 1. Locate file ---------------------------------------------------------
costar_dir <- here("data", "costar")
off_file <- list.files(costar_dir,
                       pattern = "office.*\\.xlsx$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(off_file) == 0)
  stop("No CoStar office file found in ", costar_dir)
off_file <- off_file[length(off_file)]
message("  Reading: ", basename(off_file))

# ---- 2. Variable mapping ----------------------------------------------------
var_map_office <- c(
  # Vacancy & occupancy
  "Vacancy Rate"                     = "cs_off_vacancy_rate",
  "Occupancy Rate"                   = "cs_off_occupancy_rate",
  "Availability Rate"                = "cs_off_availability_rate",
  "Availability Rate % Direct"       = "cs_off_avail_rate_direct",
  "Availability Rate % Sublet"       = "cs_off_avail_rate_sublet",
  # Rents
  "Market Asking Rent/SF"            = "cs_off_asking_rent_sf",
  "Market Asking Rent Growth"        = "cs_off_asking_rent_growth",
  "Market Asking Rent Index"         = "cs_off_asking_rent_index",
  "Office Base Rent Overall"         = "cs_off_base_rent",
  "Office Base Rent Direct"          = "cs_off_base_rent_direct",
  "Office Gross Rent Overall"        = "cs_off_gross_rent",
  "Office Gross Rent Direct"         = "cs_off_gross_rent_direct",
  # Cap rates & returns
  "Cap Rate"                         = "cs_off_cap_rate",
  "Market Cap Rate"                  = "cs_off_market_cap_rate",
  "Median Cap Rate"                  = "cs_off_median_cap_rate",
  "Capital Value Index"              = "cs_off_cap_value_index",
  "NOI Index"                        = "cs_off_noi_index",
  "Total Return"                     = "cs_off_total_return",
  "Appreciation Return"              = "cs_off_appreciation_return",
  "Income Return"                    = "cs_off_income_return",
  # Pricing
  "Asset Value"                      = "cs_off_asset_value",
  "Market Sale Price Per SF"         = "cs_off_sale_price_sf",
  "Market Sale Price Index"          = "cs_off_sale_price_index",
  "Market Sale Price Growth"         = "cs_off_sale_price_growth",
  "Median Price/Bldg SF"             = "cs_off_median_price_sf",
  # Supply & demand
  "Net Absorption SF"                = "cs_off_net_absorption_sf",
  "Net Delivered SF"                 = "cs_off_net_delivered_sf",
  "Construction Starts SF"           = "cs_off_constr_starts_sf",
  "Under Construction SF"            = "cs_off_under_constr_sf",
  "Inventory SF"                     = "cs_off_inventory_sf",
  "Demand SF"                        = "cs_off_demand_sf",
  # Available space
  "Available SF Total"               = "cs_off_available_sf",
  "Vacant SF Total"                  = "cs_off_vacant_sf",
  # Leasing
  "Leasing SF Total"                 = "cs_off_leasing_sf",
  # Demographics
  "Office Employment"                = "cs_off_office_employment",
  "Total Employment"                 = "cs_off_total_employment",
  "Population"                       = "cs_off_population",
  "Median Household Income"          = "cs_off_median_hh_income",
  "Households"                       = "cs_off_households"
)

# Flow variables (trailing 4Q sum rather than Q1 snapshot)
flow_cols_office <- c(
  "cs_off_net_absorption_sf",
  "cs_off_net_delivered_sf",
  "cs_off_constr_starts_sf",
  "cs_off_demand_sf",
  "cs_off_leasing_sf"
)

# ---- 3. Read & annualize ----------------------------------------------------
raw_off <- read_costar_quarterly(off_file, var_map = var_map_office)
val_cols <- intersect(unname(var_map_office), names(raw_off))

annual_off <- annualize_quarterly(raw_off, val_cols,
                                  flow_cols = intersect(flow_cols_office, val_cols))

message("  Annualized: ", scales::comma(nrow(annual_off)), " submarket-year rows")
message("  Year range: ", min(annual_off$tax_yr), "-", max(annual_off$tax_yr))

# ---- 4. Add city-wide aggregate & cache -------------------------------------
costar_office_wide <- add_city_wide(annual_off, val_cols)

cache_name <- paste0("costar_office_fcst_", min(annual_off$tax_yr), "_",
                     max(annual_off$tax_yr), "_", scenario, ".rds")
saveRDS(costar_office_wide, file.path(cache_dir, cache_name))
assign("costar_office_wide", costar_office_wide, envir = .GlobalEnv)
message("  \U1f4be cached: ", cache_name)

# ---- 5. Join to panel -------------------------------------------------------
join_costar_to_panel("panel_tbl_office", costar_office_wide, "cs_off_")

rm(raw_off, annual_off)
gc(verbose = FALSE)
message("--- xx_costar_office_to_panel.R complete ---\n")
