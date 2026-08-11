# scripts/ml/xx_llc_ownership.R --------------------------------------------
# Create an LLC ownership flag from the KCA Real Property Account file.
#
# The public download (EXTR_RPAcct_NoName.csv) strips taxpayer names.
# This script requires the full version (EXTR_RPAcct.csv) which includes
# TaxpayerName. Download from the KCA data portal with authorized access.
#
# Output: Adds `is_llc` (integer 0/1) to the parcel-level data frame.
#         This is NOT a model feature — it's a stratification variable
#         for downstream equity analysis (LLC as rental property proxy).
#
# Usage in main_ml.R: source after 01_import step, before panel assembly.
#   The flag gets carried through the pipeline as a parcel attribute.
# -------------------------------------------------------------------------

library(data.table)
library(here)
library(stringi)

# ---- Configuration -------------------------------------------------------

# Path to the FULL RPAcct file (with taxpayer names)
# Adjust to match your KCA extract folder structure
rpacct_with_names <- here("data", "kca", kca_date_data_extracted, "EXTR_RPAcct.csv")

# Cache path
llc_cache_path <- here("data", "cache", "llc_flag_lookup.rds")

# ---- Build LLC Lookup ----------------------------------------------------

build_llc_lookup <- function(rpacct_path = rpacct_with_names,
                             cache_path  = llc_cache_path,
                             use_cache   = TRUE) {
  
  # Check cache first
  if (use_cache && file.exists(cache_path)) {
    message("  [LLC] Loading cached LLC lookup: ", basename(cache_path))
    return(readRDS(cache_path))
  }
  
  # Verify the full RPAcct file exists (not the NoName version)
  if (!file.exists(rpacct_path)) {
    # Check if only the NoName version exists
    noname_path <- sub("EXTR_RPAcct\\.csv$", "EXTR_RPAcct_NoName.csv", rpacct_path)
    if (file.exists(noname_path)) {
      warning(
        "[LLC] Only EXTR_RPAcct_NoName.csv found (taxpayer names stripped).\n",
        "  To build LLC flags, download EXTR_RPAcct.csv from KCA with authorized access.\n",
        "  Returning NULL — is_llc will not be available."
      )
    } else {
      warning("[LLC] RPAcct file not found at: ", rpacct_path, "\n  Returning NULL.")
    }
    return(NULL)
  }
  
  message("  [LLC] Reading EXTR_RPAcct.csv for taxpayer names ...")
  
  # Read only the columns we need (Major, Minor, TaxpayerName)
  # KCA column names vary slightly across extracts; handle common variants
  acct_raw <- fread(
    rpacct_path,
    select = c("Major", "Minor", "TaxpayerName"),
    colClasses = c(Major = "character", Minor = "character", TaxpayerName = "character")
  )
  
  # If column names differ, try alternatives
  if (!"TaxpayerName" %in% names(acct_raw)) {
    alt_names <- c("Taxpayer_Name", "taxpayer_name", "TAXPAYERNAME", "taxpayername")
    matched <- intersect(alt_names, names(acct_raw))
    if (length(matched) == 1) {
      setnames(acct_raw, matched, "TaxpayerName")
    } else {
      warning("[LLC] Could not find TaxpayerName column. Available: ",
              paste(names(acct_raw), collapse = ", "))
      return(NULL)
    }
  }
  
  message("  [LLC] Raw accounts loaded: ", format(nrow(acct_raw), big.mark = ","), " rows")
  
  # Build parcel_id to match pipeline format (zero-padded Major + Minor)
  acct_raw[, parcel_id := paste0(
    stri_pad_left(Major, 6, "0"),
    stri_pad_left(Minor, 4, "0")
  )]
  
  # ---- String detection for LLC ------------------------------------------
  # Cast to upper for consistent matching
  acct_raw[, taxpayer_upper := stri_trans_toupper(TaxpayerName)]
  
  # Primary pattern: "LLC" as a standalone token or at end of string
  # Also catch common variants: "L.L.C.", "L L C", "LIMITED LIABILITY"
  acct_raw[, is_llc := as.integer(
    stri_detect_regex(taxpayer_upper, "\\bLLC\\b") |
    stri_detect_regex(taxpayer_upper, "\\bL\\.?L\\.?C\\.?\\b") |
    stri_detect_regex(taxpayer_upper, "\\bLIMITED\\s+LIABILITY\\s+CO") |
    # Also flag other entity types commonly used for rental holdings
    # These are separate flags so you can analyze them independently
    FALSE
  )]
  
  # Optional: broader "entity owner" flag (LP, LLP, Inc, Corp, Trust)
  # Useful if you want to separate individual owners from all entity types
  acct_raw[, is_entity := as.integer(
    is_llc == 1L |
    stri_detect_regex(taxpayer_upper, "\\bINC\\.?\\b") |
    stri_detect_regex(taxpayer_upper, "\\bCORP\\.?\\b") |
    stri_detect_regex(taxpayer_upper, "\\bLLP\\b") |
    stri_detect_regex(taxpayer_upper, "\\b(LIVING\\s+)?TRUST\\b") |
    stri_detect_regex(taxpayer_upper, "\\bLP\\b") |
    stri_detect_regex(taxpayer_upper, "\\bLTD\\b")
  )]
  
  # Deduplicate to one row per parcel_id
  # (RPAcct can have multiple records per parcel for tax year history;
  #  take the most recent / keep any LLC flag)
  llc_lookup <- acct_raw[, .(
    is_llc    = max(is_llc, na.rm = TRUE),
    is_entity = max(is_entity, na.rm = TRUE)
  ), by = parcel_id]
  
  # Summary stats
  n_total  <- nrow(llc_lookup)
  n_llc    <- sum(llc_lookup$is_llc)
  n_entity <- sum(llc_lookup$is_entity)
  pct_llc  <- round(100 * n_llc / n_total, 1)
  
  message("  [LLC] Lookup built: ", format(n_total, big.mark = ","), " parcels")
  message("  [LLC] LLC-owned: ", format(n_llc, big.mark = ","),
          " (", pct_llc, "%)")
  message("  [LLC] Any entity: ", format(n_entity, big.mark = ","),
          " (", round(100 * n_entity / n_total, 1), "%)")
  
  # Cache
  saveRDS(llc_lookup, cache_path)
  message("  [LLC] Cached to: ", basename(cache_path))
  
  return(llc_lookup)
}


# ---- Join to Parcel Frame ------------------------------------------------

#' Add LLC flag to a parcel-level data frame
#' 
#' @param parcel_df  Data frame with `parcel_id` column (parcel_res_full, 
#'                   parcel_comm_full, or parcel_condo_full)
#' @param llc_lookup Output of build_llc_lookup()
#' @return           parcel_df with is_llc and is_entity columns added
join_llc_flag <- function(parcel_df, llc_lookup) {
  if (is.null(llc_lookup)) {
    message("  [LLC] No lookup available — adding is_llc = NA")
    parcel_df[, is_llc := NA_integer_]
    parcel_df[, is_entity := NA_integer_]
    return(parcel_df)
  }
  
  # Drop existing columns if re-running
  for (col in c("is_llc", "is_entity")) {
    if (col %in% names(parcel_df)) parcel_df[, (col) := NULL]
  }
  
  parcel_df <- merge(parcel_df, llc_lookup, by = "parcel_id", all.x = TRUE)
  
  # Parcels not found in RPAcct get 0 (not NA) — they're not LLC-owned
  parcel_df[is.na(is_llc), is_llc := 0L]
  parcel_df[is.na(is_entity), is_entity := 0L]
  
  matched <- sum(parcel_df$is_llc, na.rm = TRUE)
  message("  [LLC] Joined to ", deparse(substitute(parcel_df)), ": ",
          format(matched, big.mark = ","), " LLC parcels of ",
          format(nrow(parcel_df), big.mark = ","))
  
  return(parcel_df)
}


# ---- Aggregate LLC Share for Panel Output --------------------------------

#' Calculate LLC ownership share summaries
#' 
#' For use with the full retrofitted + forecasted panel to report
#' % of parcels (and % of AV) owned by LLCs, by property type,
#' geography, or time.
#' 
#' @param panel_df  Full panel data frame with is_llc, total_av, tax_year
#' @param by_vars   Character vector of grouping variables
#' @return          Summary table with LLC counts, shares, and AV shares
llc_share_summary <- function(panel_df, by_vars = "tax_year") {
  
  if (!"is_llc" %in% names(panel_df)) {
    stop("is_llc column not found in panel. Run join_llc_flag() first.")
  }
  
  summary_dt <- panel_df[, .(
    n_parcels     = .N,
    n_llc         = sum(is_llc, na.rm = TRUE),
    pct_llc       = round(100 * mean(is_llc, na.rm = TRUE), 2),
    total_av      = sum(total_av, na.rm = TRUE),
    llc_av        = sum(total_av * is_llc, na.rm = TRUE),
    pct_av_llc    = round(100 * sum(total_av * is_llc, na.rm = TRUE) /
                            sum(total_av, na.rm = TRUE), 2)
  ), by = by_vars]
  
  setorderv(summary_dt, by_vars)
  return(summary_dt)
}


# ---- Run -----------------------------------------------------------------
# Called when sourced by main_ml.R

message("xx_llc_ownership.R: building LLC ownership lookup ...")
llc_lookup <- build_llc_lookup()

# Join to whichever parcel frames exist in the global env
if (exists("parcel_res_full", envir = .GlobalEnv)) {
  parcel_res_full <- join_llc_flag(
    get("parcel_res_full", envir = .GlobalEnv), llc_lookup
  )
  assign("parcel_res_full", parcel_res_full, envir = .GlobalEnv)
}

if (exists("parcel_comm_full", envir = .GlobalEnv)) {
  parcel_comm_full <- join_llc_flag(
    get("parcel_comm_full", envir = .GlobalEnv), llc_lookup
  )
  assign("parcel_comm_full", parcel_comm_full, envir = .GlobalEnv)
}

if (exists("parcel_condo_full", envir = .GlobalEnv)) {
  parcel_condo_full <- join_llc_flag(
    get("parcel_condo_full", envir = .GlobalEnv), llc_lookup
  )
  assign("parcel_condo_full", parcel_condo_full, envir = .GlobalEnv)
}

message("xx_llc_ownership.R loaded.")
