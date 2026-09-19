# =============================================================================
# com_unmatched_levy_audit.R   — READ ONLY, writes nothing
# -----------------------------------------------------------------------------
# For the TY2026 parcels in the commercial panel that are NOT in the Seattle
# levy-code population of EXTR_Parcel:
#   (a) how many are condo Majors (EXTR_CondoComplex / EXTR_CondoUnit)
#   (b) for the remainder, levy code + district from whatever source carries
#       one, with parcel counts and TY2026 AV by levy code
#
# Run from the repo root (or anywhere `here::here()` resolves to it) on a
# machine that has data/kca/<kca_date>/ and data/cache/ populated.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(here); library(stringr); library(scales)
})

kca_date       <- "2026-07-28"                 # CFG$kca_date_data_extracted
scenario       <- "baseline"
levy_code_list <- c("0010","0011","0013","0014","0016","0025","0030","0032")
kca_root       <- here::here("data", "kca", kca_date)
cache_dir      <- here::here("data", "cache")

pad4  <- function(x) str_pad(trimws(as.character(x)), 4, "left", "0")
pad6  <- function(x) str_pad(trimws(as.character(x)), 6, "left", "0")
nodash<- function(x) gsub("-", "", as.character(x), fixed = TRUE)
bil   <- function(x) sprintf("%.3f", sum(x, na.rm = TRUE) / 1e9)

# ---- 1. Commercial panel, TY2026 -------------------------------------------
panel_path <- file.path(cache_dir,
  paste0("panel_tbl_2006_2031_forecasted_", scenario, "_com.rds"))
stopifnot(file.exists(panel_path))
com <- as.data.table(readRDS(panel_path))[tax_yr == 2026]
com[, pid := nodash(parcel_id)]
com[, major := substr(pid, 1, 6)]

# TY2026 AV: certified actual where present, model prediction otherwise
pick <- function(dt, nm) {
  if (nm %in% names(dt)) as.numeric(dt[[nm]]) else rep(NA_real_, nrow(dt))
}
# NA-safe sum: NA only when BOTH sides are NA (a land-only parcel keeps its land)
addna <- function(a, b) {
  fifelse(is.na(a) & is.na(b), NA_real_,
          fifelse(is.na(a), 0, a) + fifelse(is.na(b), 0, b))
}
com[, av_ty2026 := fcoalesce(
  pick(com, "total_assessed"),
  addna(pick(com, "appr_land_val"), pick(com, "appr_imps_val")),
  pick(com, "pred_total_assessed"),
  addna(pick(com, "pred_appr_land_val"), pick(com, "pred_appr_imps_val"))
)]
com_ty <- unique(com[, .(pid, major, av_ty2026)], by = "pid")
cat(sprintf("com panel TY2026: %s parcels | $%sB\n",
            comma(nrow(com_ty)), bil(com_ty$av_ty2026)))

# ---- 2. Seattle levy-code population in EXTR_Parcel -------------------------
parcel <- fread(file.path(kca_root, "EXTR_Parcel.csv"), encoding = "Latin-1")
setnames(parcel, names(parcel), tolower(names(parcel)))
parcel[, pid := paste0(pad6(major), pad4(minor))]
parcel[, levy_code := pad4(levycode)]
seattle_pids <- parcel[levy_code %chin% levy_code_list, unique(pid)]
cat(sprintf("EXTR_Parcel: %s rows | Seattle levy codes: %s parcels\n",
            comma(nrow(parcel)), comma(length(seattle_pids))))

unm <- com_ty[!pid %chin% seattle_pids]
cat(sprintf("\nTY2026 com parcels NOT in Seattle levy-code population: %s | $%sB\n",
            comma(nrow(unm)), bil(unm$av_ty2026)))

# =============================================================================
# (a) Condo Majors — legitimately absent from EXTR_Parcel
# =============================================================================
read_if <- function(f) {
  p <- file.path(kca_root, f)
  if (!file.exists(p)) { cat("  (missing: ", f, ")\n", sep = ""); return(NULL) }
  d <- fread(p, encoding = "Latin-1"); setnames(d, names(d), tolower(names(d))); d
}
cplx <- read_if("EXTR_CondoComplex.csv")
unit <- read_if("EXTR_CondoUnit.csv")

cplx_major <- if (!is.null(cplx)) unique(pad6(cplx$major)) else character(0)
unit_major <- if (!is.null(unit)) unique(pad6(unit$major)) else character(0)
unit_pid   <- if (!is.null(unit) && "minor" %in% names(unit))
                unique(paste0(pad6(unit$major), pad4(unit$minor))) else character(0)

unm[, in_cplx_major := major %chin% cplx_major]
unm[, in_unit_major := major %chin% unit_major]
unm[, in_unit_pid   := pid   %chin% unit_pid]
unm[, is_condo      := in_cplx_major | in_unit_major]

cat("\n--- (a) condo coverage of the unmatched set ---\n")
print(unm[, .(parcels = .N, ty2026_av_B = as.numeric(bil(av_ty2026))),
          by = .(in_cplx_major, in_unit_major)][order(-parcels)])
cat(sprintf("\ncondo Major (either table): %s parcels | $%sB\n",
            comma(sum(unm$is_condo)), bil(unm[is_condo == TRUE]$av_ty2026)))
cat(sprintf("exact CondoUnit parcel_id match: %s\n", comma(sum(unm$in_unit_pid))))
cat(sprintf("NOT condo (carried to part b): %s parcels | $%sB\n",
            comma(sum(!unm$is_condo)), bil(unm[is_condo == FALSE]$av_ty2026)))

# =============================================================================
# (b) Levy code + district for the non-condo remainder
# =============================================================================
rest <- unm[is_condo == FALSE]

# Source 1: EXTR_Parcel itself, countywide (no levy filter) — the expected hit
# for anything that is simply outside the Seattle levy codes.
src_parcel <- unique(parcel[, .(pid, levy_code)], by = "pid")

# Source 2: ValueHistory (unfiltered), TY2026 row per parcel
vh_path <- file.path(kca_root, "value_history", "EXTR_ValueHistory_V.csv")
src_vh <- NULL
if (file.exists(vh_path)) {
  vh <- fread(vh_path, encoding = "Latin-1")
  setnames(vh, names(vh), tolower(names(vh)))
  vh[, pid := paste0(pad6(major), pad4(minor))]
  vh[, levy_code := pad4(levycode)]
  setorderv(vh, c("pid", "taxyr", "changedate"))
  src_vh <- unique(vh[taxyr == 2026, .(pid, levy_code)], by = "pid", fromLast = TRUE)
}

# Source 3: RPAcct
rp_path <- file.path(kca_root, "EXTR_RPAcct_NoName.csv")
src_rp <- NULL
if (file.exists(rp_path)) {
  rp <- fread(rp_path, encoding = "Latin-1")
  setnames(rp, names(rp), tolower(names(rp)))
  rp[, pid := paste0(pad6(major), pad4(minor))]
  rp[, levy_code := pad4(levycode)]
  src_rp <- unique(rp[, .(pid, levy_code)], by = "pid")
}

# NB: use match(), not a data.table join on a bare `pid` symbol — inside
# src[...] the name `pid` resolves to src's own column, not the caller's.
lk <- function(src, ids) {
  if (is.null(src)) return(rep(NA_character_, length(ids)))
  src$levy_code[match(ids, src$pid)]
}
rest[, levy_parcel := lk(src_parcel, pid)]
rest[, levy_vh     := lk(src_vh,     pid)]
rest[, levy_rp     := lk(src_rp,     pid)]
rest[, levy_code   := fcoalesce(levy_parcel, levy_vh, levy_rp)]
rest[, levy_source := fcase(!is.na(levy_parcel), "EXTR_Parcel",
                            !is.na(levy_vh),     "ValueHistory",
                            !is.na(levy_rp),     "RPAcct",
                            default = "none")]

# District: whatever column any source carries
dist_col <- function(dt) if (is.null(dt)) NULL else
  grep("district", names(dt), value = TRUE, ignore.case = TRUE)[1]
for (nm in c("parcel", "vh", "rp")) {
  dt <- switch(nm, parcel = parcel, vh = if (exists("vh")) vh else NULL,
               rp = if (exists("rp")) rp else NULL)
  dc <- dist_col(dt)
  if (!is.null(dc) && !is.na(dc)) {
    map <- unique(dt[, .(pid, d = as.character(get(dc)))], by = "pid")
    rest[, (paste0("district_", nm)) := map$d[match(rest$pid, map$pid)]]
    cat(sprintf("district column found in %s: %s\n", nm, dc))
  }
}
dcols <- grep("^district_", names(rest), value = TRUE)
if (length(dcols)) rest[, district := do.call(fcoalesce, lapply(.SD, as.character)),
                        .SDcols = dcols] else rest[, district := NA_character_]

cat("\n--- (b) levy-code source coverage ---\n")
print(rest[, .(parcels = .N), by = levy_source][order(-parcels)])

cat("\n--- (b) parcel counts and TY2026 AV by levy code ---\n")
by_levy <- rest[, .(parcels = .N,
                    ty2026_av_B = round(sum(av_ty2026, na.rm = TRUE) / 1e9, 4),
                    av_missing  = sum(is.na(av_ty2026)),
                    districts   = paste(sort(unique(na.omit(district))), collapse = "; ")),
                by = .(levy_code)][order(-parcels)]
print(by_levy, nrows = 100)
cat(sprintf("\ntotal: %s parcels | $%sB | %s with no levy code anywhere\n",
            comma(nrow(rest)), bil(rest$av_ty2026),
            comma(sum(is.na(rest$levy_code)))))

if (length(dcols)) {
  cat("\n--- (b) by district ---\n")
  print(rest[, .(parcels = .N,
                 ty2026_av_B = round(sum(av_ty2026, na.rm = TRUE) / 1e9, 4)),
             by = district][order(-parcels)], nrows = 100)
}

cat("\nRead-only check complete — nothing written.\n")
