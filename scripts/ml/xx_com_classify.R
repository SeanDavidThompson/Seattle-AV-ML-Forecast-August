# =============================================================================
# xx_com_classify.R  —  Tiered reclassification of the commercial "other" bucket
# =============================================================================
# Purpose
# -------
# split_com_panel_to_subgroups() assigns a parcel to a commercial subgroup on
# two passes: spec_area_name (crosswalk) then present_use.  Everything that
# matches neither lands in panel_tbl_com_other and is forecast by naive
# carry-forward.
#
# This script shrinks that bucket with a deterministic waterfall that runs
# BEFORE the flat/blended residual treatment:
#
#   Tier 1  present_use          EXTR_Parcel.PresentUse       (LookUp type 102)
#   Tier 2  section use mix      EXTR_CommBldgSection.SectionUse (type 118),
#                                sqft-weighted dominant use
#   Tier 3  predominant use      EXTR_CommBldg.PredominantUse (type 118)
#   Tier 4  free-text keywords   PropertyName / BldgDescr / SectionDescr /
#                                ComplexDescr / permit project name
#   Tier 5  zoning prior         EXTR_Parcel.CurrentZoning (land-only parcels)
#
# Produces in .GlobalEnv:
#   com_class_map    parcel_id | com_subgroup_assigned | class_tier |
#                    class_evidence | is_vacant | is_parking
#   com_class_audit  tier coverage counts
#
# Parcels that stay unmatched keep com_subgroup_assigned = NA and flow to the
# residual growth treatment in xx_com_other_growth.R.  Vacant-land and parking
# parcels are TAGGED rather than assigned — they are legitimately not office /
# retail / industrial and should not be forced into a built-space model.
#
# NOTE ON THE EXISTING CODE LISTS  ------------------------------------------
# The present_use_codes currently in COM_SUBGROUPS are LookUp *type 118*
# (commercial building section use) values, not *type 102* (parcel present
# use) values, and within type 118 they are also mis-assigned.  See
# CLASSIFICATION_FINDINGS.md.  COM_SUBGROUPS_PU below is the corrected type-102
# mapping and is what Tier 1 uses.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(here)
})

message("Running xx_com_classify.R ...")

kca_date <- get("kca_date_data_extracted", envir = .GlobalEnv)
kca_root <- here::here("data", "kca", kca_date)

# ---------------------------------------------------------------------------
# read_kca_cols()
# ---------------------------------------------------------------------------
# KCA extracts vary between vintages: PropertyName is absent from some
# EXTR_Parcel exports, SectionDescr from some EXTR_CommBldgSection exports,
# and so on.  fread(select=) only warns on a missing column, which then blows
# up downstream as "object 'property_name' not found".
#
# `spec` is a named list mapping the name this script wants -> a vector of
# candidate column names in the file (first match wins).  Columns that are
# absent are created as NA so every downstream reference is safe, and the
# misses are reported once so you can see what the extract actually carries.
read_kca_cols <- function(path, spec, label = basename(path)) {
  if (!file.exists(path)) stop("Missing file: ", path)
  hdr <- names(data.table::fread(file = path, nrows = 0L))
  found  <- character(0)
  target <- character(0)
  missing <- character(0)
  for (nm in names(spec)) {
    hit <- intersect(spec[[nm]], hdr)
    if (length(hit)) {
      found  <- c(found,  hit[1])
      target <- c(target, nm)
    } else {
      missing <- c(missing, nm)
    }
  }
  dt <- data.table::fread(file = path, select = found,
                          na.strings = c("", "NA"), encoding = "Latin-1")
  data.table::setDT(dt)
  data.table::setnames(dt, found, target)
  chr <- names(dt)[vapply(dt, is.character, logical(1))]
  for (cc in chr)
    data.table::set(dt, j = cc,
                    value = iconv(dt[[cc]], "UTF-8", "UTF-8", sub = ""))
  for (nm in missing) dt[, (nm) := NA]
  if (length(missing))
    message("  \u2139\ufe0f  ", label, ": no column for ",
            paste(missing, collapse = ", "),
            " \u2014 filled NA (those tiers/sources will not fire)")
  dt[]
}

pad_id <- function(major, minor)
  paste0(str_pad(trimws(as.character(major)), 6, "left", "0"), "-",
         str_pad(trimws(as.character(minor)), 4, "left", "0"))

# =============================================================================
# CODE MAPS
# =============================================================================

# ---- Tier 1: EXTR_Parcel.PresentUse — LookUp type 102 -----------------------
# Verified line by line against EXTR_LookUp.csv (LUType == 102).
COM_SUBGROUPS_PU <- list(
  apt = c(
    10L,   # Congregate Housing
    11L,   # Apartment
    16L,   # Apartment(Mixed Use)
    17L,   # Apartment(Co-op)
    18L,   # Apartment(Subsidized)
    49L,   # Retirement Facility
    56L,   # Residence Hall/Dorm
    57L,   # Group Home
    59L    # Nursing Home  (matches the "Nursinghome" spec_area already in apt)
  ),
  office = c(
    106L,  # Office Building
    118L,  # Office Park
    126L,  # Condominium(Office)
    273L   # Historic Prop(Office)
  ),
  industrial = c(
    138L,  # Mining/Quarry/Ore Processing
    195L,  # Warehouse
    202L,  # High Tech/High Flex
    210L,  # Industrial Park
    216L,  # Service Building
    223L,  # Industrial(Gen Purpose)
    245L,  # Industrial(Heavy)
    246L,  # Industrial(Light)
    252L,  # Mini Warehouse
    261L,  # Terminal(Rail)
    262L,  # Terminal(Marine/Comm Fish)
    263L,  # Terminal(Grain)
    264L,  # Terminal(Auto/Bus/Other)
    271L,  # Terminal(Marine)
    276L,  # Historic Prop(Loft/Warehse)
    344L,  # Terminal (Freight Auto/Rail/Other)
    345L,  # Bus Base/Fleet Maint Fac
    346L   # Rail Freight Terminal
  ),
  retail = c(
    60L, 61L, 62L, 63L, 64L,  # Shopping Ctr: Nghbrhood/Community/Regional/
                              #               Maj Retail/Specialty
    96L,   # Retail(Line/Strip)
    101L,  # Retail Store
    104L,  # Retail(Big Box)
    105L,  # Retail(Discount)
    161L,  # Auto Showroom and Lot
    162L,  # Bank
    163L,  # Car Wash
    167L,  # Conv Store without Gas
    168L,  # Conv Store with Gas
    186L,  # Service Station
    191L,  # Grocery Store
    194L,  # Mini Lube
    274L   # Historic Prop(Retail)
  ),
  hospitality = c(
    51L,   # Hotel/Motel
    58L,   # Resort/Lodge/Retreat
    171L,  # Restaurant(Fast Food)
    183L,  # Restaurant/Lounge
    188L,  # Tavern/Lounge
    275L,  # Historic Prop(Eat/Drink)
    278L,  # Historic Prop(Transient Fac)
    340L,  # Bed & Breakfast
    341L   # Rooming House
  ),
  medical = c(
    55L,   # Rehabilitation Center
    122L,  # Medical/Dental Office
    173L   # Hospital
  )
)

# Parcels that must NEVER be assigned to a built-space subgroup.
# 300/309/316 are the codes the current pass-2 list routes into `office`.
PU_VACANT  <- c(299L, 300L, 301L, 309L, 316L, 323L, 324L, 325L, 326L, 327L,
                328L, 330L, 331L, 332L, 333L, 334L, 335L, 336L, 337L, 339L)
PU_PARKING <- c(159L, 180L, 182L, 277L)

# ---- Tier 2/3: SectionUse & PredominantUse — LookUp type 118 ----------------
# Ranges, not enumerations: type 118 is dense and the 300-599 block is ordered
# by use family.  Individual overrides follow the ranges.
classify_use118 <- function(u) {
  data.table::fcase(
    u %in% c(344L, 345L, 346L, 347L),                     "office",
    u %in% c(341L, 342L, 343L, 313L, 320L),               "medical",
    u %in% c(349L, 350L, 351L),                           "hospitality",
    u %in% c(300L, 352L, 354L, 355L, 356L, 357L),         "apt",
    u %in% c(353L, 318L, 319L, 304L, 303L, 531L, 532L),   "retail",
    u %in% c(386L, 406L, 407L, 408L, 409L, 410L, 528L),   "industrial",
    u >= 330L & u <  348L,                                "office",
    u >= 348L & u <  353L,                                "hospitality",
    u >= 353L & u <  400L,                                "retail",
    u >= 400L & u <  500L,                                "industrial",
    default = NA_character_
  )
}

# ---- Tier 4: free-text keyword patterns ------------------------------------
# Precedence is list order: the first pattern that matches wins.
# Each group closes with `)S?\\b` so plurals match: APARTMENTS, OFFICES,
# RESTAURANTS, WAREHOUSES, STORES all hit their singular term.  Medical and
# hospitality run before office/retail because "MEDICAL OFFICE BUILDING" and
# "HOTEL RETAIL" would otherwise be captured by the broader terms.
COM_KEYWORDS <- list(
  medical = paste0(
    "\\b(MEDICAL|DENTAL|CLINIC|HOSPITAL|SURGERY|SURGICAL|HEALTH ?CENTER|",
    "BIOTECH|BIOMEDICAL|LABORATOR(Y|IES)|LIFE ?SCIENCE|DIAGNOSTIC|",
    "URGENT ?CARE|PHARMAC(Y|EUTICAL)|NURSING|HOSPICE|REHAB)S?\\b"),
  hospitality = paste0(
    "\\b(HOTEL|MOTEL|INN|LODGE|RESORT|HOSTEL|BED ?(AND|&) ?BREAKFAST|",
    "RESTAURANT|CAFE|CAFETERIA|COFFEE|BISTRO|DINER|EATER(Y|IES)|",
    "TAVERN|BAR ?(AND|&) ?GRILL|GRILL|BREWERY|BREWPUB|PUB|LOUNGE|",
    "BANQUET|CATERING|FOOD ?(COURT|HALL|SERVICE)|PIZZ(A|ERIA)|",
    "SUITES|HOSPITALITY)S?\\b"),
  apt = paste0(
    "\\b(APARTMENT|APTS?|MULTI-?FAMILY|MULTIFAMILY|RESIDEN(CE|TIAL|CES)|",
    "DORM(ITORY)?|SENIOR ?(LIVING|HOUSING)|ASSISTED ?LIVING|",
    "CONGREGATE|STUDIOS?|LOFTS?|FLATS?|TOWNHO(ME|USE)S?|",
    "WORKFORCE ?HOUSING|AFFORDABLE ?HOUSING)S?\\b"),
  industrial = paste0(
    "\\b(WAREHOUSE|DISTRIBUTION|FULFIL?LMENT|LOGISTICS|MANUFACTURING|",
    "FACTORY|PLANT|INDUSTRIAL|FLEX|SHOP ?BUILDING|MACHINE ?SHOP|",
    "COLD ?STORAGE|SELF ?STORAGE|MINI ?STORAGE|STORAGE ?(FACILITY|",
    "BUILDING)|TERMINAL|FREIGHT|SHIPYARD|DRY ?DOCK|FOUNDRY|",
    "MAINTENANCE ?(FACILITY|BUILDING)|YARD|DATA ?CENTER)S?\\b"),
  retail = paste0(
    "\\b(RETAIL|STORE|SHOP(S|PING)?|SHOPPING ?(CENTER|CENTRE|CTR|MALL)|",
    "MALL|MARKET|MARKETPLACE|GROCER(Y|S)|SUPERMARKET|PHARMACY|",
    "SHOWROOM|DEALERSHIP|AUTO ?(SALES|CENTER)|SERVICE ?STATION|",
    "GAS ?STATION|CAR ?WASH|BANK|CREDIT ?UNION|SALON|",
    "BOUTIQUE|OUTLET|PLAZA|STOREFRONT)S?\\b"),
  office = paste0(
    "\\b(OFFICE|OFC|TOWER|CORPORATE ?(CENTER|CENTRE|CAMPUS|HQ)|",
    "HEADQUARTERS|BUSINESS ?(CENTER|CENTRE|PARK)|PROFESSIONAL ?(BLDG|",
    "BUILDING|CENTER)|EXECUTIVE ?(SUITES?|CENTER)|",
    "COWORKING|CO-?WORK)S?\\b")
)

# Entity-name and generic noise that must not drive a match on its own.
# Stripped from the text BEFORE the keyword patterns are applied.
COM_KEYWORD_NOISE <- paste0(
  "\\b(LLC|L\\.?L\\.?C|INC|INCORPORATED|CORP(ORATION)?|CO|COMPANY|",
  "LP|LLP|LTD|TRUST|PARTNERS(HIP)?|ASSOCIATES?|ASSOC|PROPERT(Y|IES)|",
  "HOLDINGS?|INVESTMENTS?|VENTURES?|GROUP|ENTERPRISES?|REALTY|",
  "REAL ?ESTATE|MANAGEMENT|MGMT|DEVELOPMENT|DEVELOPERS?|",
  "BUILDING|BLDG|PARCEL|LOT|UNIT|PHASE|PORTFOLIO)\\b")

normalise_text <- function(x) {
  x <- toupper(as.character(x))
  x <- str_replace_all(x, "[^A-Z0-9&]+", " ")
  x <- str_replace_all(x, COM_KEYWORD_NOISE, " ")
  str_squish(x)
}

keyword_classify <- function(txt) {
  out <- rep(NA_character_, length(txt))
  txt <- normalise_text(txt)
  ok  <- !is.na(txt) & nzchar(txt)
  for (key in names(COM_KEYWORDS)) {
    hit <- is.na(out) & ok & str_detect(txt, COM_KEYWORDS[[key]])
    out[hit] <- key
  }
  out
}

# ---- Tier 5: zoning prior (land-only parcels) ------------------------------
# Deliberately coarse.  Seattle zoning strings look like "NC3P-75 (M)",
# "SM-SLU 240/125", "IG1 U/45", "C1-55 (M)", "DOC1 U/450/U".
zoning_classify <- function(z) {
  z <- toupper(str_squish(as.character(z)))
  data.table::fcase(
    is.na(z) | !nzchar(z),                    NA_character_,
    str_detect(z, "^(IG|IB|IC|IH)"),          "industrial",
    str_detect(z, "^(DOC|DMC|DH|DMR)"),       "office",
    str_detect(z, "^(NC|C1|C2|SM|MIO)"),      "retail",
    str_detect(z, "^(LR|MR|HR|RSL|MPC)"),     "apt",
    default = NA_character_
  )
}

# =============================================================================
# BUILD THE CLASSIFICATION MAP
# =============================================================================

build_com_class_map <- function(target_ids = NULL,
                                use_permits = TRUE) {

  # ---- parcel_id format ------------------------------------------------------
  # split_com_panel_to_subgroups() supports both "123456-0010" and "1234560010"
  # panels.  Everything below is built dash-form; the returned map is converted
  # back to whatever the caller uses so the join cannot silently miss.
  .dash <- TRUE
  if (!is.null(target_ids) && length(target_ids)) {
    .dash <- any(grepl("-", utils::head(target_ids, 10L)))
    if (!.dash)
      target_ids <- sub("^([0-9]{6})([0-9]{4})$", "\\1-\\2",
                        as.character(target_ids))
  }

  # ---- Parcel-level source ---------------------------------------------------
  p_path <- file.path(kca_root, "EXTR_Parcel.csv")
  parc <- read_kca_cols(p_path, list(
    major          = c("Major"),
    minor          = c("Minor"),
    property_name  = c("PropertyName", "PropName", "Property Name"),
    present_use    = c("PresentUse", "Present Use"),
    current_zoning = c("CurrentZoning", "Current Zoning", "Zoning"),
    spec_area      = c("SpecArea", "Spec Area"),
    prop_type      = c("PropType", "PropertyType", "Property Type")
  ), "EXTR_Parcel")
  parc[, property_name  := as.character(property_name)]
  parc[, current_zoning := as.character(current_zoning)]
  parc[, parcel_id      := pad_id(major, minor)]
  parc[, present_use   := suppressWarnings(as.integer(present_use))]
  parc[, spec_area     := suppressWarnings(as.integer(spec_area))]
  parc <- unique(parc, by = "parcel_id")

  if (!is.null(target_ids))
    parc <- parc[parcel_id %chin% unique(target_ids)]

  map <- parc[, .(parcel_id, present_use, property_name, current_zoning,
                  spec_area, prop_type)]
  map[, `:=`(com_subgroup_assigned = NA_character_,
             class_tier            = NA_character_,
             class_evidence        = NA_character_)]

  # ---- Flags -----------------------------------------------------------------
  map[, is_vacant  := present_use %in% PU_VACANT]
  map[, is_parking := present_use %in% PU_PARKING]

  # ---- Tier 1: present_use ---------------------------------------------------
  for (key in names(COM_SUBGROUPS_PU)) {
    hit <- is.na(map$com_subgroup_assigned) &
      map$present_use %in% COM_SUBGROUPS_PU[[key]]
    if (any(hit, na.rm = TRUE))
      map[which(hit), `:=`(com_subgroup_assigned = key,
                           class_tier            = "1_present_use",
                           class_evidence        = paste0("PresentUse=",
                                                          present_use))]
  }

  # ---- Tier 2: sqft-weighted dominant section use ----------------------------
  s_path <- file.path(kca_root, "EXTR_CommBldgSection.csv")
  if (file.exists(s_path)) {
    sect <- read_kca_cols(s_path, list(
      major         = c("Major"),
      minor         = c("Minor"),
      section_use   = c("SectionUse", "Section Use"),
      gross_sq_ft   = c("GrossSqFt", "GrossSqFeet", "Gross Square Feet"),
      section_descr = c("SectionDescr", "SectionDescription",
                        "Section Description")
    ), "EXTR_CommBldgSection")
    sect[, section_descr := as.character(section_descr)]
    sect[, parcel_id   := pad_id(major, minor)]
    sect[, section_use := suppressWarnings(as.integer(section_use))]
    sect[, gross_sqft  := suppressWarnings(as.numeric(gross_sq_ft))]
    sect[, sg          := classify_use118(section_use)]

    dom <- sect[!is.na(sg) & is.finite(gross_sqft) & gross_sqft > 0,
                .(sqft = sum(gross_sqft)), by = .(parcel_id, sg)]
    data.table::setorder(dom, parcel_id, -sqft)
    dom_tot <- dom[, .(tot = sum(sqft)), by = parcel_id]
    dom <- unique(dom, by = "parcel_id")[dom_tot, on = "parcel_id"]
    dom[, share := sqft / tot]

    # Require a genuine plurality: a 20%-office / 80%-mixed parcel should not
    # be called office.  0.5 is the cutoff; lower it only with evidence.
    dom <- dom[share >= 0.50, .(parcel_id, sg, share)]

    map[dom, on = "parcel_id",
        `:=`(com_subgroup_assigned =
               fifelse(is.na(com_subgroup_assigned), i.sg,
                       com_subgroup_assigned),
             class_tier =
               fifelse(is.na(class_tier), "2_section_use", class_tier),
             class_evidence =
               fifelse(is.na(class_evidence),
                       paste0("SectionUse share=", round(i.share, 2)),
                       class_evidence))]
    assign("comm_section_use_mix", dom, envir = .GlobalEnv)
  } else {
    message("  \u26a0\ufe0f  EXTR_CommBldgSection.csv not found — Tier 2 skipped")
  }

  # ---- Tier 3: predominant use ----------------------------------------------
  b_path <- file.path(kca_root, "EXTR_CommBldg.csv")
  bldg <- NULL
  if (file.exists(b_path)) {
    bldg <- read_kca_cols(b_path, list(
      major             = c("Major"),
      minor             = c("Minor"),
      predominant_use   = c("PredominantUse", "Predominant Use"),
      bldg_descr        = c("BldgDescr", "BldgDescription",
                            "Building Description"),
      bldg_gross_sq_ft  = c("BldgGrossSqFt", "BldgGrossSqFeet",
                            "Building Gross Square Feet")
    ), "EXTR_CommBldg")
    bldg[, bldg_descr      := as.character(bldg_descr)]
    bldg[, parcel_id       := pad_id(major, minor)]
    bldg[, predominant_use := suppressWarnings(as.integer(predominant_use))]
    bldg[, bldg_gross_sqft := suppressWarnings(as.numeric(bldg_gross_sq_ft))]

    # Largest building on the parcel wins
    data.table::setorder(bldg, parcel_id, -bldg_gross_sqft)
    big <- unique(bldg, by = "parcel_id")[, .(parcel_id, predominant_use)]
    big[, sg := classify_use118(predominant_use)]
    big <- big[!is.na(sg)]

    map[big, on = "parcel_id",
        `:=`(com_subgroup_assigned =
               fifelse(is.na(com_subgroup_assigned), i.sg,
                       com_subgroup_assigned),
             class_tier =
               fifelse(is.na(class_tier), "3_predominant_use", class_tier),
             class_evidence =
               fifelse(is.na(class_evidence),
                       paste0("PredominantUse=", i.predominant_use),
                       class_evidence))]
  } else {
    message("  \u26a0\ufe0f  EXTR_CommBldg.csv not found — Tier 3 skipped")
  }

  # ---- Tier 4: free text -----------------------------------------------------
  # Text sources are stacked and evaluated in reliability order.  Parcel
  # PropertyName is the weakest (often an owner entity), so it runs last.
  txt_parts <- list()

  if (!is.null(bldg))
    txt_parts$bldg <- bldg[!is.na(bldg_descr) & nzchar(bldg_descr),
                           .(parcel_id, src = "BldgDescr", txt = bldg_descr)]

  if (exists("sect", inherits = FALSE))
    txt_parts$sect <- sect[!is.na(section_descr) & nzchar(section_descr),
                           .(parcel_id, src = "SectionDescr",
                             txt = section_descr)]

  a_path <- file.path(kca_root, "EXTR_AptComplex.csv")
  if (file.exists(a_path)) {
    apt <- read_kca_cols(a_path, list(
      major         = c("Major"),
      minor         = c("Minor"),
      complex_descr = c("ComplexDescr", "ComplexDescription",
                        "Complex Description")
    ), "EXTR_AptComplex")
    apt[, complex_descr := as.character(complex_descr)]
    apt[, parcel_id := pad_id(major, minor)]
    txt_parts$apt <- apt[!is.na(complex_descr) & nzchar(complex_descr),
                         .(parcel_id, src = "ComplexDescr",
                           txt = complex_descr)]
  }

  if (isTRUE(use_permits)) {
    pd_path <- file.path(kca_root, "EXTR_PermitDetail.csv")
    ph_path <- file.path(kca_root, "EXTR_Permit.csv")
    if (file.exists(pd_path) && file.exists(ph_path)) {
      # LookUp type 161: 12 = Project Name, 11 = Other Description
      pdet <- read_kca_cols(pd_path, list(
        permit_nbr  = c("PermitNbr", "Permit Nbr"),
        permit_item = c("PermitItem", "Permit Item"),
        item_value  = c("ItemValue", "Item Value")
      ), "EXTR_PermitDetail")
      pdet[, item_value  := as.character(item_value)]
      pdet[, permit_item := suppressWarnings(as.integer(permit_item))]
      pdet <- pdet[permit_item %in% c(11L, 12L) &
                     !is.na(item_value) & nzchar(item_value)]
      phist <- read_kca_cols(ph_path, list(
        major      = c("Major"),
        minor      = c("Minor"),
        permit_nbr = c("PermitNbr", "Permit Nbr")
      ), "EXTR_Permit")
      phist[, parcel_id := pad_id(major, minor)]
      pj <- merge(pdet[, .(permit_nbr, item_value)],
                  unique(phist[, .(permit_nbr, parcel_id)]),
                  by = "permit_nbr", allow.cartesian = TRUE)
      txt_parts$permit <- pj[, .(parcel_id, src = "PermitProject",
                                 txt = item_value)]
    } else {
      message("  \u2139\ufe0f  KCA permit extracts not found — permit text skipped")
    }
  }

  # PropertyName is the weakest source and runs last; it is also the one most
  # often absent, so it is appended rather than gating the whole tier.
  txt_parts$prop_name <- map[!is.na(property_name) & nzchar(property_name),
                             .(parcel_id, src = "PropertyName",
                               txt = property_name)]
  txt_parts <- Filter(function(z) !is.null(z) && nrow(z) > 0, txt_parts)

  if (length(txt_parts) > 0) {
    txt <- rbindlist(txt_parts, use.names = TRUE, fill = TRUE)
    message("  Tier 4 text sources: ",
            paste(names(txt_parts), collapse = ", "), " (",
            format(nrow(txt), big.mark = ","), " strings)")

    txt[, sg := keyword_classify(txt)]
    txt <- txt[!is.na(sg)]

    if (nrow(txt) > 0) {
      # One parcel can hit several patterns across sources.  Keep the modal
      # label; ties break on source reliability (order of src_rank).
      txt[, src_rank := match(src, c("SectionDescr", "BldgDescr",
                                     "ComplexDescr", "PermitProject",
                                     "PropertyName"))]
      votes <- txt[, .(n = .N, best_rank = min(src_rank)),
                   by = .(parcel_id, sg)]
      data.table::setorder(votes, parcel_id, -n, best_rank)
      votes <- unique(votes, by = "parcel_id")

      map[votes, on = "parcel_id",
          `:=`(com_subgroup_assigned =
                 fifelse(is.na(com_subgroup_assigned), i.sg,
                         com_subgroup_assigned),
               class_tier =
                 fifelse(is.na(class_tier), "4_keyword", class_tier),
               class_evidence =
                 fifelse(is.na(class_evidence),
                         paste0("keyword n=", i.n), class_evidence))]
      assign("com_keyword_hits", txt, envir = .GlobalEnv)
    }
  } else {
    message("  \u26a0\ufe0f  Tier 4 skipped \u2014 no free-text columns present ",
            "in this extract")
  }

  # ---- Tier 5: zoning prior, land-only parcels only --------------------------
  map[, zone_sg := zoning_classify(current_zoning)]
  z_ok <- is.na(map$com_subgroup_assigned) & !is.na(map$zone_sg) &
    map$is_vacant
  if (any(z_ok, na.rm = TRUE))
    map[which(z_ok), `:=`(com_subgroup_assigned = zone_sg,
                          class_tier            = "5_zoning",
                          class_evidence        = paste0("Zoning=",
                                                         current_zoning))]
  map[, zone_sg := NULL]

  # ---- Guardrails ------------------------------------------------------------
  # Vacant land and standalone parking never enter a built-space subgroup via
  # tiers 1-4.  (Tier 5 assigns vacant land on purpose — that is the only
  # place it is allowed, and it is reported separately below.)
  bad <- map$is_vacant & !is.na(map$com_subgroup_assigned) &
    map$class_tier != "5_zoning"
  if (any(bad, na.rm = TRUE)) {
    message("  \u26a0\ufe0f  ", sum(bad, na.rm = TRUE),
            " vacant parcels matched a built-space tier — reverting to NA")
    map[which(bad), `:=`(com_subgroup_assigned = NA_character_,
                         class_tier = NA_character_,
                         class_evidence = NA_character_)]
  }
  map[is_parking == TRUE,
      `:=`(com_subgroup_assigned = NA_character_,
           class_tier = "parking_hold", class_evidence = "PresentUse parking")]

  if (!.dash) map[, parcel_id := gsub("-", "", parcel_id, fixed = TRUE)]

  map
}

# =============================================================================
# VALIDATION — score the classifier on parcels that are already labelled
# =============================================================================
# Runs the same waterfall on parcels whose subgroup is known from spec_area,
# then compares.  This is the honest test of Tier 4 before it touches the
# forecast.  Returns a confusion matrix and per-tier accuracy.
validate_com_class <- function(known_map) {
  stopifnot(all(c("parcel_id", "com_subgroup") %in% names(known_map)))
  km  <- data.table::as.data.table(known_map)[!is.na(com_subgroup)]
  pred <- build_com_class_map(target_ids = km$parcel_id)
  cmp  <- merge(km[, .(parcel_id, truth = com_subgroup)],
                pred[, .(parcel_id, pred = com_subgroup_assigned, class_tier)],
                by = "parcel_id")
  cmp[, correct := !is.na(pred) & pred == truth]

  message("\n  --- classifier validation on ", nrow(cmp), " labelled parcels ---")
  message("  overall accuracy (of those predicted): ",
          round(100 * mean(cmp[!is.na(pred)]$correct), 1), "%")
  message("  coverage (predicted at all): ",
          round(100 * mean(!is.na(cmp$pred)), 1), "%")
  print(cmp[!is.na(pred), .(n = .N, acc = round(mean(correct), 3)),
            by = class_tier][order(class_tier)])
  print(table(truth = cmp$truth, pred = cmp$pred, useNA = "ifany"))
  assign("com_class_validation", cmp, envir = .GlobalEnv)
  invisible(cmp)
}

# =============================================================================
# RUN
# =============================================================================
# Restrict to the parcels currently in com_other when that panel exists,
# otherwise classify every commercial parcel (standalone QA mode).
# main_ml.R publishes `panel_tbl_com_other_ids` during pass 3, before
# panel_tbl_com_other itself is built; fall back to the panel when this script
# is run standalone for QA.
.target <- NULL
if (exists("panel_tbl_com_other_ids", envir = .GlobalEnv)) {
  .target <- unique(as.character(get("panel_tbl_com_other_ids",
                                     envir = .GlobalEnv)))
} else if (exists("panel_tbl_com_other", envir = .GlobalEnv)) {
  .target <- unique(as.character(
    get("panel_tbl_com_other", envir = .GlobalEnv)$parcel_id))
}
if (!is.null(.target))
  message("  classifying ", format(length(.target), big.mark = ","),
          " unassigned commercial parcels")

com_class_map <- build_com_class_map(target_ids = .target)

com_class_audit <- com_class_map[
  , .(parcels = .N),
  by = .(class_tier = fifelse(is.na(class_tier), "unmatched", class_tier),
         com_subgroup_assigned)][order(class_tier, -parcels)]

message("\n  --- com_other reclassification ---")
print(com_class_audit)
message("  assigned: ",
        format(sum(!is.na(com_class_map$com_subgroup_assigned)), big.mark = ","),
        " of ", format(nrow(com_class_map), big.mark = ","),
        " (", round(100 * mean(!is.na(com_class_map$com_subgroup_assigned)), 1),
        "%)")
message("  vacant flagged: ", sum(com_class_map$is_vacant, na.rm = TRUE),
        " | parking flagged: ", sum(com_class_map$is_parking, na.rm = TRUE))

assign("com_class_map",   com_class_map,   envir = .GlobalEnv)
assign("com_class_audit", com_class_audit, envir = .GlobalEnv)
assign("COM_SUBGROUPS_PU", COM_SUBGROUPS_PU, envir = .GlobalEnv)
assign("classify_use118", classify_use118, envir = .GlobalEnv)
assign("validate_com_class", validate_com_class, envir = .GlobalEnv)

message("xx_com_classify.R loaded.")
