# 01_import_res.R — import residential KCA tables

# RPA accounts
rpa_raw <- read_csv(
  here("data","kca",kca_date_data_extracted,"EXTR_RPAcct_NoName.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(levy_code = as.character(levy_code)) %>%
  filter(levy_code %in% levy_code_list)

# Parcels
parcel_raw <- read_csv(
  here("data","kca",kca_date_data_extracted,"EXTR_Parcel.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(levy_code = as.character(levy_code)) %>%
  filter(levy_code %in% levy_code_list) %>%
  decode_lookup_columns(
    lookup_tbl = lookup,
    cols = c(
      "hbu_as_if_vacant","hbu_as_improved","present_use",
      "water_system","sewer_system","access","topography","street_surface",
      "restrictive_sz_shape","inadequate_parking",
      "mt_rainier","olympics","cascades","territorial","seattle_skyline",
      "puget_sound","lake_washington","lake_sammamish","small_lake_river_creek",
      "other_view","wfnt_location","wfnt_bank","wfnt_poor_quality",
      "wfnt_restricted_access","tideland_shoreland",
      "traffic_noise","contamination","historic_site","current_use_designation"
    ),
    lu_types = c(103,104,102,56,57,55,59,60,90,92,rep(58,10),50,52,51,53,54,95,93,67,16)
  ) %>%
  mutate(
    major     = stringr::str_pad(as.character(major), 6, pad = "0"),
    minor     = stringr::str_pad(as.character(minor), 4, pad = "0"),
    parcel_id = paste0(major, minor)
  )

# Residential buildings
res_raw <- read_csv(
  here("data","kca",kca_date_data_extracted,"EXTR_ResBldg.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    major    = str_pad(as.character(major), 6, pad = "0"),
    minor    = str_pad(as.character(minor), 4, pad = "0"),
    bldg_nbr = as.integer(bldg_nbr),
    zip_code = str_pad(as.character(zip_code), 5, pad = "0"),
    parcel_id = paste0(major, minor)
  ) %>%
  decode_lookup_columns(
    lookup_tbl = lookup,
    cols = c("bldg_grade","fin_basement_grade","heat_system","heat_source","condition"),
    lu_types = c(82,82,108,84,83)
  )

# Crosswalks
residential_crosswalk <- read_excel(here("data","crosswalk.xlsx"), sheet = "residential")
zoning_crosswalk      <- read_excel(here("data","crosswalk.xlsx"), sheet = "zoning")

# RPA parcel_id
rpa_raw <- rpa_raw %>%
  mutate(
    major     = stringr::str_pad(as.character(major), 6, pad = "0"),
    minor     = stringr::str_pad(as.character(minor), 4, pad = "0"),
    parcel_id = paste0(major, minor)
  )

# Expose
assign("res_bldg_raw",   res_raw,    envir = .GlobalEnv)
assign("parcel_res_full", parcel_raw, envir = .GlobalEnv)

message("01_import_res.R loaded — ",
        nrow(parcel_raw), " parcels | ",
        nrow(res_raw),    " bldg rows")
