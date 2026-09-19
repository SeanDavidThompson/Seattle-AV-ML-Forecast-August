# =============================================================================
# apt_grouping_check.R — apartment grouping reconciliation.  READ-ONLY.
# =============================================================================
# Compares KCA's own apartment specialty membership (EXTR_Parcel.SpecArea ==
# "100") against what the commercial panel carries as spec_area == 100 at
# tax_yr 2026, over the Seattle levy-code population used by 01_import_res.R.
#
# Writes NOTHING: no files, no cache, no .GlobalEnv assignment.  Everything
# lives inside local(); paths are read with the same here::here() roots the
# pipeline uses, so it resolves onto the share exactly as a normal run does.
#
# Run from the project root:
#   source(here::here("apt_grouping_check.R"))          # or wherever you put it
# Optionally set first (otherwise the defaults below apply):
#   kca_date_data_extracted <- "2026-07-28"
#   scenario                <- "baseline"
#   area_reports_year       <- 2026
# =============================================================================

local({

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

kca_date <- get0("kca_date_data_extracted", envir = .GlobalEnv,
                 ifnotfound = "2026-07-28")
scenario <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
rpt_yr   <- as.integer(get0("area_reports_year", envir = .GlobalEnv,
                            ifnotfound = 2026L))
tax_yr_k <- 2026L
spec_apt <- 100L

# Same constant as 00_init.R / 01_import_res.R
levy_code_list <- c("0010","0011","0013","0014","0016","0025","0030","0032")

kca_root  <- here::here("data", "kca", kca_date)
cache_dir <- here::here("data", "cache")

hdr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
fmt <- function(x) formatC(x, big.mark = ",", format = "d")
bn  <- function(x) sprintf("$%.3fB", sum(x, na.rm = TRUE) / 1e9)

# Case/punctuation-insensitive column picker: KCA header spellings drift
# between extracts (SpecArea / Spec Area, ApprLandVal / AppraisedLandVal).
pick <- function(nms, cands) {
  norm <- tolower(gsub("[^a-z0-9]", "", tolower(nms)))
  for (c0 in cands) {
    i <- match(gsub("[^a-z0-9]", "", tolower(c0)), norm)
    if (!is.na(i)) return(nms[i])
  }
  NA_character_
}
# Zero-pad to width n.  formatC(flag = "0") pads CHARACTER input with spaces,
# so coerce to integer first; this accepts "10", "0010" and 10 alike, matching
# stringr::str_pad(..., pad = "0") as used in 01_import_res.R.
pad <- function(x, n) {
  x <- trimws(as.character(x))
  i <- suppressWarnings(as.integer(x))
  out <- formatC(i, width = n, flag = "0", format = "d")
  out[is.na(i)] <- NA_character_
  out
}
int <- function(x) suppressWarnings(as.integer(trimws(as.character(x))))
# "$ (1,889,223)" -> -1889223; plain and scientific notation pass through.
num <- function(x) {
  x   <- trimws(as.character(x))
  neg <- grepl("\\(", x) | grepl("^-", x)
  v   <- suppressWarnings(as.numeric(gsub("[^0-9.eE]", "", x)))
  data.table::fifelse(is.na(v), NA_real_, data.table::fifelse(neg, -v, v))
}

# -----------------------------------------------------------------------------
# 1. EXTR_Parcel — Seattle levy codes only (mirrors 01_import_res.R)
# -----------------------------------------------------------------------------
hdr("1. EXTR_Parcel: Seattle levy-code population")

pp <- file.path(kca_root, "EXTR_Parcel.csv")
stopifnot(file.exists(pp))

p_nms <- names(data.table::fread(pp, nrows = 0L))
c_maj <- pick(p_nms, "Major");       c_min <- pick(p_nms, "Minor")
c_lvy <- pick(p_nms, c("LevyCode", "Levy Code"))
c_pu  <- pick(p_nms, c("PresentUse", "Present Use"))
c_sa  <- pick(p_nms, c("SpecArea", "Spec Area"))
c_ssa <- pick(p_nms, c("SpecSubArea", "Spec Sub Area"))
c_pt  <- pick(p_nms, c("PropType", "Prop Type"))
stopifnot(!is.na(c_maj), !is.na(c_min), !is.na(c_lvy), !is.na(c_sa))

p_sel <- c(Major = c_maj, Minor = c_min, LevyCode = c_lvy, PresentUse = c_pu,
           SpecArea = c_sa, SpecSubArea = c_ssa, PropType = c_pt)
p_sel <- p_sel[!is.na(p_sel)]
xp <- data.table::fread(pp, select = unname(p_sel),
                        colClasses = "character", showProgress = FALSE)
data.table::setnames(xp, unname(p_sel), names(p_sel))
for (miss in setdiff(c("PresentUse", "SpecSubArea", "PropType"), names(xp)))
  xp[, (miss) := NA_character_]

xp[, levy_code := pad(LevyCode, 4)]
sea <- xp[levy_code %chin% levy_code_list]
sea[, parcel_id := paste0(pad(Major, 6), pad(Minor, 4))]
sea[, `:=`(kca_spec_area = int(SpecArea),
           kca_spec_sub  = int(SpecSubArea),
           present_use   = int(PresentUse))]
sea <- unique(sea, by = "parcel_id")

cat(sprintf("EXTR_Parcel rows: %s | Seattle levy codes: %s parcels\n",
            fmt(nrow(xp)), fmt(nrow(sea))))
cat(sprintf("KCA SpecArea == %d (Apartments): %s parcels\n",
            spec_apt, fmt(sea[kca_spec_area == spec_apt, .N])))

kca_ids <- sea[kca_spec_area == spec_apt, parcel_id]

# -----------------------------------------------------------------------------
# 2. Certified TY2026 AV from ValueHistory (panel-independent)
# -----------------------------------------------------------------------------
# Using ValueHistory rather than either panel keeps the AV measure identical
# across all three cells of the agreement table — a KCA-only parcel that never
# entered the com panel still gets the same definition of TY2026 AV.
# Filters mirror xx_av_history.R: levy code, tax_status "T", no tax_val_reason,
# latest change_date within parcel x tax_yr.
hdr("2. Certified TY2026 AV (EXTR_ValueHistory_V.csv)")

vh_path <- file.path(kca_root, "value_history", "EXTR_ValueHistory_V.csv")
av26 <- NULL
if (file.exists(vh_path)) {
  v_nms <- names(data.table::fread(vh_path, nrows = 0L))
  v_sel <- c(maj = pick(v_nms, "Major"),       min = pick(v_nms, "Minor"),
             yr  = pick(v_nms, c("TaxYr", "Tax Yr", "TaxYear")),
             lvy = pick(v_nms, c("LevyCode", "Levy Code")),
             st  = pick(v_nms, c("TaxStatus", "Tax Status")),
             rsn = pick(v_nms, c("TaxValReason", "Tax Val Reason")),
             lnd = pick(v_nms, c("ApprLandVal", "AppraisedLandVal")),
             imp = pick(v_nms, c("ApprImpsVal", "AppraisedImpsVal", "ApprImprVal")),
             chg = pick(v_nms, c("ChangeDate", "Change Date")))
  v_sel <- v_sel[!is.na(v_sel)]
  vh <- data.table::fread(vh_path, select = unname(v_sel),
                          colClasses = "character", showProgress = FALSE)
  data.table::setnames(vh, unname(v_sel), names(v_sel))
  vh[, levy_code := pad(lvy, 4)]
  vh <- vh[levy_code %chin% levy_code_list & int(yr) == tax_yr_k]
  if ("st"  %in% names(vh)) vh <- vh[trimws(st) == "T"]
  if ("rsn" %in% names(vh)) vh <- vh[is.na(rsn) | trimws(rsn) == ""]
  vh[, parcel_id := paste0(pad(maj, 6), pad(min, 4))]
  if ("chg" %in% names(vh)) {
    data.table::setorder(vh, parcel_id, chg)
    vh <- vh[, .SD[.N], by = parcel_id]
  } else {
    vh <- unique(vh, by = "parcel_id")
  }
  vh[, land := num(lnd)]
  vh[, imps := num(imp)]
  av26 <- vh[, .(parcel_id,
                 av = data.table::fifelse(is.na(land), 0, land) +
                      data.table::fifelse(is.na(imps), 0, imps))]
  cat(sprintf("TY%d ValueHistory rows (Seattle, T, no val reason): %s parcels | %s\n",
              tax_yr_k, fmt(nrow(av26)), bn(av26$av)))
} else {
  cat("ValueHistory not found — AV columns will fall back to the com panel.\n")
}

# -----------------------------------------------------------------------------
# 3. Commercial panel at tax_yr 2026
# -----------------------------------------------------------------------------
hdr(sprintf("3. Commercial panel, tax_yr %d", tax_yr_k))

com_path <- file.path(cache_dir,
  sprintf("panel_tbl_2006_2031_forecasted_%s_com.rds", scenario))
if (!file.exists(com_path))
  com_path <- file.path(cache_dir,
    sprintf("panel_tbl_2006_2031_inputs_%s_com.rds", scenario))
stopifnot(file.exists(com_path))
cat("panel: ", basename(com_path), "\n", sep = "")

com <- as.data.table(readRDS(com_path))
com <- com[as.integer(tax_yr) == tax_yr_k]
com[, pid := gsub("-", "", as.character(parcel_id))]

sa_col <- pick(names(com), c("spec_area", "SpecArea", "spec_area_code"))
if (is.na(sa_col)) {
  stop("The com panel carries no spec_area column — nothing to compare. ",
       "Re-run 06_forecast_av_2026_2031_sequential_comm.R, which joins it.")
}
com[, panel_spec_area := int(get(sa_col))]

sg_col <- pick(names(com), c("com_subgroup", "subgroup", "sub_group"))
sn_col <- pick(names(com), c("spec_area_name", "spec_name"))

av_col <- pick(names(com), "total_assessed")
com[, panel_av := if (!is.na(av_col) && any(!is.na(com[[av_col]])))
      as.numeric(get(av_col))
    else data.table::fifelse(is.na(appr_land_val), 0, as.numeric(appr_land_val)) +
         data.table::fifelse(is.na(appr_imps_val), 0, as.numeric(appr_imps_val))]

keep <- c("pid", "panel_spec_area", "panel_av",
          if (!is.na(sg_col)) sg_col, if (!is.na(sn_col)) sn_col)
com <- unique(com[, ..keep], by = "pid")
if (!is.na(sg_col)) data.table::setnames(com, sg_col, "com_subgroup")
if (!is.na(sn_col)) data.table::setnames(com, sn_col, "spec_area_name")

cat(sprintf("com panel TY%d: %s parcels | spec_area == %d: %s parcels\n",
            tax_yr_k, fmt(nrow(com)), spec_apt,
            fmt(com[panel_spec_area == spec_apt, .N])))

# The com panel is a subset of the levy-code population; restrict the panel
# side to Seattle so the two sides are drawn from the same universe.
com_sea <- com[pid %chin% sea$parcel_id]
cat(sprintf("  of which inside the Seattle levy-code population: %s parcels ",
            fmt(nrow(com_sea))))
cat(sprintf("(spec_area == %d: %s)\n", spec_apt,
            fmt(com_sea[panel_spec_area == spec_apt, .N])))

panel_ids <- com_sea[panel_spec_area == spec_apt, pid]

# -----------------------------------------------------------------------------
# (a) Agreement table
# -----------------------------------------------------------------------------
hdr("(a) Agreement: KCA SpecArea 100  vs  panel spec_area 100, TY2026")

cmp <- sea[, .(parcel_id, present_use, kca_spec_area, kca_spec_sub, PropType)]
cmp[, kca_apt   := parcel_id %chin% kca_ids]
cmp[, panel_apt := parcel_id %chin% panel_ids]
cmp[, in_com    := parcel_id %chin% com_sea$pid]
cmp[com_sea, on = c(parcel_id = "pid"),
    `:=`(panel_spec_area = i.panel_spec_area, panel_av = i.panel_av)]
if ("com_subgroup" %in% names(com_sea))
  cmp[com_sea, on = c(parcel_id = "pid"), com_subgroup := i.com_subgroup]

# Guarantee the optional panel columns exist so the summaries below never
# have to branch on their presence (a scalar NA in a `by` list is a length
# mismatch, not a missing value).
if (!("com_subgroup" %in% names(cmp))) cmp[, com_subgroup := NA_character_]
if (!("panel_spec_area" %in% names(cmp))) cmp[, panel_spec_area := NA_integer_]
if (!("panel_av" %in% names(cmp))) cmp[, panel_av := NA_real_]

if (!is.null(av26)) cmp[av26, on = "parcel_id", av := i.av]
if (!("av" %in% names(cmp))) cmp[, av := NA_real_]
cmp[is.na(av), av := panel_av]          # fall back where ValueHistory is silent

cmp <- cmp[kca_apt | panel_apt]
cmp[, bucket := data.table::fifelse(kca_apt & panel_apt, "both",
                data.table::fifelse(kca_apt, "KCA-only", "panel-only"))]

agree <- cmp[, .(parcels = .N,
                 ty2026_av = sum(av, na.rm = TRUE),
                 av_missing = sum(is.na(av)),
                 in_com_panel = sum(in_com)),
             by = bucket][order(factor(bucket, c("both", "KCA-only", "panel-only")))]
agree[, ty2026_av_B := round(ty2026_av / 1e9, 3)][, ty2026_av := NULL]
print(agree)
cat(sprintf("\nunion: %s parcels | %s\n", fmt(nrow(cmp)), bn(cmp$av)))

# -----------------------------------------------------------------------------
# (b) What the disagreeing parcels are
# -----------------------------------------------------------------------------
hdr("(b) Disagreement composition")

lu_path <- file.path(kca_root, "EXTR_LookUp.csv")
pu_desc <- NULL
if (file.exists(lu_path)) {
  lu <- data.table::fread(lu_path, colClasses = "character", showProgress = FALSE)
  lt <- pick(names(lu), "LUType"); li <- pick(names(lu), "LUItem")
  ld <- pick(names(lu), "LUDescription")
  if (!any(is.na(c(lt, li, ld))))
    pu_desc <- unique(lu[int(get(lt)) == 102L,
                         .(present_use = int(get(li)), pu_desc = get(ld))],
                      by = "present_use")
}
label_pu <- function(dt) {
  if (!is.null(pu_desc)) dt[pu_desc, on = "present_use", pu_desc := i.pu_desc]
  dt
}

for (b in c("panel-only", "KCA-only")) {
  s <- cmp[bucket == b]
  cat(sprintf("\n--- %s: %s parcels | %s ---\n", b, fmt(nrow(s)), bn(s$av)))
  if (!nrow(s)) next

  top <- s[, .(parcels = .N, av_B = round(sum(av, na.rm = TRUE) / 1e9, 3)),
           by = present_use][order(-parcels)]
  label_pu(top)
  cat("top PresentUse:\n"); print(utils::head(top, 12))

  if (b == "KCA-only") {
    # KCA calls it apartments; what did the pipeline put it in instead?
    alt <- s[, .(parcels = .N, av_B = round(sum(av, na.rm = TRUE) / 1e9, 3)),
             by = .(panel_spec_area = data.table::fifelse(
                      in_com, as.character(panel_spec_area), "(not in com panel)"),
                    com_subgroup = as.character(com_subgroup))][order(-parcels)]
    cat("spec_area the pipeline assigned instead:\n"); print(utils::head(alt, 12))
  } else {
    # Panel calls it apartments; what does KCA carry?
    alt <- s[, .(parcels = .N, av_B = round(sum(av, na.rm = TRUE) / 1e9, 3)),
             by = .(kca_spec_area = data.table::fifelse(
                      is.na(kca_spec_area), "(none)", as.character(kca_spec_area)),
                    PropType)][order(-parcels)]
    cat("KCA SpecArea carried instead:\n"); print(utils::head(alt, 12))
  }
}

# -----------------------------------------------------------------------------
# (c) Agreed apartments by SpecSubArea, mapped to report regions
# -----------------------------------------------------------------------------
hdr("(c) Agreed apartments by SpecSubArea -> report regions R1/R2/R3")

both <- cmp[bucket == "both"]
by_ssa <- both[, .(parcels = .N, av_B = round(sum(av, na.rm = TRUE) / 1e9, 3)),
               by = .(nbhd = kca_spec_sub)][order(nbhd)]
cat(sprintf("both-call-apartments: %s parcels across %s SpecSubAreas\n",
            fmt(nrow(both)), fmt(by_ssa[!is.na(nbhd), .N])))

ara_path <- file.path(cache_dir, sprintf("area_report_actuals_%d.rds", rpt_yr))
regions <- NULL
if (file.exists(ara_path)) {
  ara <- as.data.table(readRDS(ara_path))
  rk <- pick(names(ara), c("report_kind"))
  sa <- pick(names(ara), c("spec_area"))
  rg <- pick(names(ara), c("spec_region"))
  nb <- pick(names(ara), c("nbhds"))
  an <- pick(names(ara), c("area_name"))
  pc <- pick(names(ara), c("pct_change"))
  if (!any(is.na(c(rk, sa, rg, nb)))) {
    regions <- ara[grepl("region", get(rk)) & int(get(sa)) == spec_apt &
                     !is.na(get(nb)) & nzchar(get(nb)),
                   .(spec_region = int(get(rg)),
                     area_name   = if (!is.na(an)) as.character(get(an)) else NA_character_,
                     pct_change  = if (!is.na(pc)) as.numeric(get(pc)) else NA_real_,
                     nbhds       = gsub("[[:space:]]", "", as.character(get(nb))))]
    regions <- unique(regions, by = "spec_region")
  }
} else {
  cat("area_report_actuals_", rpt_yr, ".rds not in cache — ",
      "run with use_area_actuals = TRUE once first.\n", sep = "")
}

if (!is.null(regions) && nrow(regions)) {
  map <- regions[, .(nbhd = int(unlist(strsplit(nbhds, ",", fixed = TRUE)))),
                 by = .(spec_region, area_name, pct_change)]
  cat("\nreport regions:\n")
  print(regions[, .(spec_region, area_name,
                    pct_change = round(100 * pct_change, 2),
                    n_nbhds = lengths(strsplit(nbhds, ",", fixed = TRUE)))][order(spec_region)])

  by_ssa[map, on = "nbhd", `:=`(spec_region = i.spec_region, area_name = i.area_name)]

  cat("\nparcels per region:\n")
  print(by_ssa[, .(nbhds = .N, parcels = sum(parcels), av_B = round(sum(av_B), 3)),
               by = .(spec_region = data.table::fifelse(
                        is.na(spec_region), -1L, spec_region), area_name)][order(spec_region)])

  unmapped <- by_ssa[is.na(spec_region) & !is.na(nbhd)]
  if (nrow(unmapped)) {
    cat(sprintf("\n%s SpecSubAreas (%s parcels) are in no R1/R2/R3 list:\n",
                fmt(nrow(unmapped)), fmt(sum(unmapped$parcels))))
    print(unmapped[order(-parcels)][1:min(20, .N)])
  }
} else {
  by_ssa[, `:=`(spec_region = NA_integer_, area_name = NA_character_)]
}

# Per-neighborhood project counts: the parser validates the Project Inventory
# table against the "R n Total:" footers but keeps only the neighborhood LIST
# (reg$nbhds), so the per-neighborhood counts have to be re-read from the PDF.
rpt_dir <- here::here("data", "kca", "area_reports", as.character(rpt_yr))
inv <- NULL
if (dir.exists(rpt_dir) && requireNamespace("pdftools", quietly = TRUE)) {
  inv_tot <- NULL
  pdfs <- list.files(rpt_dir, pattern = "\\.pdf$", full.names = TRUE,
                     ignore.case = TRUE)
  for (f in pdfs) {
    lines <- tryCatch(
      unlist(strsplit(paste(pdftools::pdf_text(f), collapse = "\n"), "\n")),
      error = function(e) character(0))
    if (!length(lines)) next
    head_txt <- paste(utils::head(lines, 200), collapse = " ")
    if (!grepl("Apartment", head_txt, ignore.case = TRUE)) next
    i0 <- grep("Project\\s+Inventory", lines, ignore.case = TRUE)
    if (!length(i0)) next
    hdr_i <- i0[1] + which(grepl("\\bR1\\b.*\\bR2\\b",
                                 lines[(i0[1] + 1):min(i0[1] + 5, length(lines))]))[1]
    if (is.na(hdr_i)) next
    end_i <- hdr_i + which(grepl("Total\\s*:", lines[(hdr_i + 1):length(lines)]))[1]
    if (is.na(end_i)) end_i <- min(hdr_i + 60, length(lines))
    # Only the (neighborhood, name, projects) triples are read here.  The
    # region each neighborhood belongs to comes from area_report_actuals$nbhds,
    # NOT from the column geometry: .spec_regions() infers the region from the
    # x-position of each match relative to the R2..R9 header offsets, which is
    # correct for the published layout but misattributes neighborhoods the
    # moment the columns shift.  The nbhds lists are already the parsed,
    # footer-validated answer, so they are the safer key.
    trip <- "(\\d{1,3})\\s+([A-Za-z][A-Za-z /.&'-]*?)\\s+(\\d[\\d,]*)(?=\\s|$)"
    rows <- list()
    for (i in seq(hdr_i + 1, end_i - 1)) {
      mm <- gregexpr(trip, lines[i], perl = TRUE)[[1]]
      if (mm[1] < 0) next
      for (q in seq_along(mm)) {
        seg <- substr(lines[i], mm[q], mm[q] + attr(mm, "match.length")[q] - 1)
        g   <- regmatches(seg, regexec(trip, seg, perl = TRUE))[[1]]
        rows[[length(rows) + 1]] <- data.table(
          nbhd      = as.integer(g[2]),
          nbhd_name = trimws(g[3]),
          projects  = as.integer(gsub(",", "", g[4])))
      }
    }
    # "R 1 Total: 4,652" footers, to reconcile the parse against the report
    ft <- regmatches(lines[end_i],
                     gregexpr("R\\s*(\\d)\\s*Total:\\s*([\\d,]+)",
                              lines[end_i], perl = TRUE))[[1]]
    if (length(ft))
      inv_tot <- data.table(
        spec_region    = as.integer(sub("R\\s*(\\d).*", "\\1", ft)),
        report_total   = as.integer(gsub(",", "", sub(".*Total:\\s*", "", ft))))
    if (length(rows)) {
      inv <- unique(rbindlist(rows), by = "nbhd")
      cat("\ninventory from: ", basename(f), "\n", sep = "")
    }
    break
  }
}

if (!is.null(inv) && nrow(inv)) {
  # region comes from the report's own nbhds lists, not from column geometry
  if (exists("map", inherits = FALSE) && !is.null(map))
    inv[map, on = "nbhd", spec_region := i.spec_region]
  side <- merge(inv, by_ssa[, .(nbhd, parcels, av_B)], by = "nbhd", all = TRUE)
  side[is.na(parcels), parcels := 0L]
  side[is.na(av_B), av_B := 0]
  side[, per_project := round(parcels / projects, 3)]
  data.table::setorder(side, spec_region, nbhd, na.last = TRUE)
  cat("\nparcels per neighborhood vs report project counts.\n",
      "NOTE: Specialty 100 is a COUNTYWIDE report, so its project counts cover\n",
      "all of King County while these parcels are Seattle levy codes only —\n",
      "parcels-per-project is well under 1 wherever a region reaches outside\n",
      "Seattle, and is a coverage ratio, not an error.\n", sep = "")
  print(side[, .(spec_region, nbhd, nbhd_name, report_projects = projects,
                 agreed_parcels = parcels, parcels_per_project = per_project,
                 av_B)], nrows = 200)

  reg_tot <- side[, .(nbhds = .N, report_projects = sum(projects, na.rm = TRUE),
                      agreed_parcels = sum(parcels),
                      av_B = round(sum(av_B, na.rm = TRUE), 3)),
                  by = spec_region][order(spec_region, na.last = TRUE)]
  if (!is.null(inv_tot)) {
    reg_tot[inv_tot, on = "spec_region", report_total := i.report_total]
    reg_tot[, footer_ok := report_projects == report_total]
  }
  cat("\nregion totals:\n"); print(reg_tot)
  if (!is.null(inv_tot) && any(!reg_tot$footer_ok %in% TRUE))
    cat("  NOTE: a region's parsed inventory disagrees with its \"R n Total:\"",
        " footer — treat that region's per-neighborhood counts as suspect.\n", sep = "")

  orphan <- side[is.na(spec_region) & !is.na(projects)]
  if (nrow(orphan))
    cat(sprintf("\n%s inventory neighborhoods map to no region in nbhds: %s\n",
                fmt(nrow(orphan)), paste(orphan$nbhd, collapse = ", ")))
  cat(sprintf("\ntotal: %s report projects vs %s agreed parcels (%.2f parcels/project)\n",
              fmt(sum(side$projects, na.rm = TRUE)), fmt(sum(side$parcels)),
              sum(side$parcels) / sum(side$projects, na.rm = TRUE)))
} else {
  cat("\n(no Project Inventory table read — pdftools missing, the Apartments PDF\n",
      " is not in ", rpt_dir, ", or its inventory layout changed.)\n", sep = "")
  cat("\nparcels per SpecSubArea:\n")
  print(by_ssa[order(-parcels)], nrows = 200)
}

# -----------------------------------------------------------------------------
# (d) 2-4 unit properties — excluded from the apartment specialty
# -----------------------------------------------------------------------------
hdr("(d) 2-4 unit properties (the apartment specialty excludes these)")

# Pipeline definition (02_transfrm.R): max NbrLivingUnits over a parcel's
# ResBldg rows, in [2, 4].
rb_path <- file.path(kca_root, "EXTR_ResBldg.csv")
small_mf <- NULL
if (file.exists(rb_path)) {
  r_nms <- names(data.table::fread(rb_path, nrows = 0L))
  r_sel <- c(maj = pick(r_nms, "Major"), min = pick(r_nms, "Minor"),
             lu  = pick(r_nms, c("NbrLivingUnits", "Nbr Living Units")))
  if (!any(is.na(r_sel))) {
    rb <- data.table::fread(rb_path, select = unname(r_sel),
                            colClasses = "character", showProgress = FALSE)
    data.table::setnames(rb, unname(r_sel), names(r_sel))
    rb[, parcel_id := paste0(pad(maj, 6), pad(min, 4))]
    rb[, units := suppressWarnings(as.numeric(lu))]
    small_mf <- rb[, .(max_living_units = suppressWarnings(max(units, na.rm = TRUE))),
                   by = parcel_id]
    small_mf <- small_mf[is.finite(max_living_units)]
    small_mf[, is_small_mf := as.integer(max_living_units >= 2 & max_living_units <= 4)]
  }
}

if (!is.null(small_mf)) {
  cmp[small_mf, on = "parcel_id",
      `:=`(max_living_units = i.max_living_units, is_small_mf = i.is_small_mf)]
  cmp[is.na(is_small_mf), is_small_mf := 0L]
  out <- cmp[, .(parcels = .N,
                 small_mf_2_4 = sum(is_small_mf == 1L),
                 pct = round(100 * mean(is_small_mf == 1L), 1),
                 small_mf_av_B = round(sum(av[is_small_mf == 1L], na.rm = TRUE) / 1e9, 3)),
             by = bucket][order(factor(bucket, c("both", "KCA-only", "panel-only")))]
  cat("by ResBldg NbrLivingUnits in [2,4] (the pipeline's is_small_mf):\n")
  print(out)
  cat("\nunit-count distribution among the 2-4 unit rows:\n")
  print(cmp[is_small_mf == 1L, .N, by = .(bucket, max_living_units)][
    order(bucket, max_living_units)])
} else {
  cat("EXTR_ResBldg.csv / NbrLivingUnits unavailable - the unit-count answer\n",
      "cannot be given from ResBldg; see the PresentUse cross-check below if the\n",
      "LookUp table is present.\n", sep = "")
}

# Cross-check off PresentUse (Duplex / Triplex / 4-Plex), read from the
# LookUp rather than hardcoded so a code-system change shows up here.
if (!is.null(pu_desc)) {
  small_codes <- pu_desc[grepl("duplex|triplex|4[ -]?plex|four[ -]?plex", pu_desc,
                               ignore.case = TRUE), present_use]
  if (length(small_codes)) {
    cat("\ncross-check on PresentUse ",
        paste(sort(small_codes), collapse = ", "), ":\n", sep = "")
    print(cmp[, .(parcels = .N,
                  small_mf_pu = sum(present_use %in% small_codes)), by = bucket][
      order(factor(bucket, c("both", "KCA-only", "panel-only")))])
  }
}

cat("\nRead-only check complete — nothing written.\n")

})
