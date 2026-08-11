# =============================================================================
# xx_costar_industrial_to_panel.R
# =============================================================================
# Reads CoStar Industrial submarket data, annualizes to tax year, and joins
# to the industrial panel (panel_tbl_industrial).
#
# CoStar file: "CoStar_for_AV_-_industrial_-_full_dataset_by_submarket_-_*.xlsx"
#   Sheet "DataExport", quarterly, 16 Seattle submarkets, 80 columns.
#
# Key predictors for Industrial AV:
#   - NNN rent (triple-net lease structure dominates this sector)
#   - Vacancy / availability (tight industrial market = value driver)
#   - Cap rates (typically lower than office in recent years)
#   - Net absorption & deliveries (supply-demand balance)
#   - Industrial employment (demand proxy)
# =============================================================================

message("\n--- xx_costar_industrial_to_panel.R ---")

library(data.table)
library(readxl)
library(stringr)
library(here)

source(here("scripts", "ml", "xx_costar_import_utils.R"))

if (!exists("cache_dir")) cache_dir <- here("data", "cache")
if (!exists("scenario"))  scenario  <- "baseline"

# ---- 1. Locate file ---------------------------------------------------------
costar_dir <- here("data", "costar")
ind_file <- list.files(costar_dir,
                       pattern = "industrial.*\\.xlsx$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(ind_file) == 0)
  stop("No CoStar industrial file found in ", costar_dir)
ind_file <- ind_file[length(ind_file)]
message("  Reading: ", basename(ind_file))

# ---- 2. Variable mapping ----------------------------------------------------
var_map_industrial <- c(
  # Vacancy & occupancy
  "Vacancy Rate"                     = "cs_ind_vacancy_rate",
  "Occupancy Rate"                   = "cs_ind_occupancy_rate",
  "Availability Rate"                = "cs_ind_availability_rate",
  "Availability Rate % Direct"       = "cs_ind_avail_rate_direct",
  "Availability Rate % Sublet"       = "cs_ind_avail_rate_sublet",
  # Rents (NNN dominates industrial)
  "NNN Rent Overall"                 = "cs_ind_nnn_rent",
  "NNN Rent Direct"                  = "cs_ind_nnn_rent_direct",
  "All Service Type Rent Overall"    = "cs_ind_all_svc_rent",
  "All Service Type Rent Direct"     = "cs_ind_all_svc_rent_direct",
  "Market Asking Rent/SF"            = "cs_ind_asking_rent_sf",
  "Market Asking Rent Growth"        = "cs_ind_asking_rent_growth",
  "Market Asking Rent Index"         = "cs_ind_asking_rent_index",
  # Cap rates & returns
  "Cap Rate"                         = "cs_ind_cap_rate",
  "Market Cap Rate"                  = "cs_ind_market_cap_rate",
  "Median Cap Rate"                  = "cs_ind_median_cap_rate",
  "Capital Value Index"              = "cs_ind_cap_value_index",
  "NOI Index"                        = "cs_ind_noi_index",
  "Total Return"                     = "cs_ind_total_return",
  "Appreciation Return"              = "cs_ind_appreciation_return",
  "Income Return"                    = "cs_ind_income_return",
  # Pricing
  "Asset Value"                      = "cs_ind_asset_value",
  "Market Sale Price Per SF"         = "cs_ind_sale_price_sf",
  "Market Sale Price Index"          = "cs_ind_sale_price_index",
  "Market Sale Price Growth"         = "cs_ind_sale_price_growth",
  "Median Price/Bldg SF"             = "cs_ind_median_price_sf",
  # Supply & demand
  "Net Absorption SF"                = "cs_ind_net_absorption_sf",
  "Net Delivered SF"                 = "cs_ind_net_delivered_sf",
  "Construction Starts SF"           = "cs_ind_constr_starts_sf",
  "Under Construction SF"            = "cs_ind_under_constr_sf",
  "Inventory SF"                     = "cs_ind_inventory_sf",
  "Demand SF"                        = "cs_ind_demand_sf",
  # Available / vacant space
  "Available SF Total"               = "cs_ind_available_sf",
  "Vacant SF Total"                  = "cs_ind_vacant_sf",
  # Leasing
  "Leasing SF Total"                 = "cs_ind_leasing_sf",
  # Demographics / employment
  "Industrial Employment"            = "cs_ind_industrial_employment",
  "Total Employment"                 = "cs_ind_total_employment",
  "Population"                       = "cs_ind_population",
  "Median Household Income"          = "cs_ind_median_hh_income",
  "Households"                       = "cs_ind_households"
)

flow_cols_industrial <- c(
  "cs_ind_net_absorption_sf",
  "cs_ind_net_delivered_sf",
  "cs_ind_constr_starts_sf",
  "cs_ind_demand_sf",
  "cs_ind_leasing_sf"
)

# ---- 3. Read & annualize ----------------------------------------------------
raw_ind <- read_costar_quarterly(ind_file, var_map = var_map_industrial)
val_cols <- intersect(unname(var_map_industrial), names(raw_ind))

annual_ind <- annualize_quarterly(raw_ind, val_cols,
                                  flow_cols = intersect(flow_cols_industrial, val_cols))

message("  Annualized: ", scales::comma(nrow(annual_ind)), " submarket-year rows")
message("  Year range: ", min(annual_ind$tax_yr), "-", max(annual_ind$tax_yr))

# ---- 4. Add city-wide aggregate & cache -------------------------------------
costar_industrial_wide <- add_city_wide(annual_ind, val_cols)

cache_name <- paste0("costar_industrial_fcst_", min(annual_ind$tax_yr), "_",
                     max(annual_ind$tax_yr), "_", scenario, ".rds")
saveRDS(costar_industrial_wide, file.path(cache_dir, cache_name))
assign("costar_industrial_wide", costar_industrial_wide, envir = .GlobalEnv)
message("  \U1f4be cached: ", cache_name)

# ---- 5. Join to panel -------------------------------------------------------
join_costar_to_panel("panel_tbl_industrial", costar_industrial_wide, "cs_ind_")

rm(raw_ind, annual_ind)
gc(verbose = FALSE)
message("--- xx_costar_industrial_to_panel.R complete ---\n")
