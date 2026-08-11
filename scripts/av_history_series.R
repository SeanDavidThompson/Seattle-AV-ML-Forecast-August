# =============================================================================
# av_history_series.R  (v3)
# Full certified-basis AV series: history 2001-2026 + forecast 2027-2031,
# by segment, in one chart with av_forecast_facets.R aesthetics (no dots).
#
# Layers
# ------
#   2001-2021  chained: MATCHED-PARCEL YoY growth from av_history_cln
#              (parcels present in both adjacent years only -- immune to
#              ValueHistory coverage gaps, e.g. ~330 large office parcels
#              missing 2016-2017 rows), chained backward off the 2022
#              certified anchor per group.  PP backcast at its 2022-2026
#              CAGR (flagged estimated).
#   2022-2026  certified decomposition: cert_real x hist_appraised shares
#              (res/com/condo) + exact PP; commercial subgroups allocated
#              within the certified commercial base by ValueHistory shares.
#   2027-2031  forecast, imported (never recomputed here):
#     PREFERRED  data/wrangled/facets_series.csv exported from
#                av_forecast_facets.R -- cols tax_yr, group, av
#                [, av_pes, av_opt]; groups: total, res, condo, com_apt,
#                com_office, com_industrial, com_retail, com_hospitality,
#                com_medical.  History rows (2022-2026), if present, also
#                override the internal anchors so the overlap is EXACT.
#     FALLBACK   latest data/wrangled/av_certified_total_summary_*.csv
#                (from av_reconcile_certified.R; NC-inclusive Total with
#                baseline/pessimistic/optimistic) -- Total panel only;
#                other panels then end at 2026.
#
# Conventions: tax_yr = payable year (TY2026 = assessed 1/1/2025);
# regular-levy basis; certified history totals embed each year's NC, and
# the summary-CSV Total forecast carries the NC layer forward -- so the
# Total panel is "With NC + PP" throughout, matching the facets chart.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(here)
  library(readxl)
  library(ggplot2)
  library(scales)
})

message("Running av_history_series.R (v3) ...")

# ------------------------------------------------------------------- CFG ----
CFG_HIST <- list(
  kca_date  = get0("kca_date_data_extracted", envir = .GlobalEnv,
                   ifnotfound = "2026-07-28"),
  cache_rds = here::here("data", "cache", "av_history_cln.rds"),
  xwalk_xlsx = here::here("data", "crosswalk.xlsx"),
  wrangled_dir = here::here("data", "wrangled"),
  facets_series_csv = here::here("data", "wrangled", "facets_series.csv"),
  make_plot = TRUE
)

FORECAST_YRS <- 2027:2031

# ------------------------------------------- certified anchors (= facets) ----
exempt_2023 <- 1172001173
share_2023  <- exempt_2023 / (307774003816 + exempt_2023)

certified_anchor_tbl <- data.table(
  tax_yr     = 2022:2026,
  cert_total = c(276293453116 * (1 - share_2023),
                 308874491598 - exempt_2023,
                 299443737414,
                 297696494175,
                 306466960900),
  pp_av      = c(7490457363, 8542526282, 8947018132, 9073883825, 9330922938)
)
certified_anchor_tbl[, cert_real := cert_total - pp_av]

hist_appraised <- data.table(
  tax_yr = rep(2022:2026, each = 3),
  track  = rep(c("res", "com", "condo"), 5),
  appr_b = c(136.44,  99.12, 28.88,
             159.58, 104.96, 30.76,
             147.84, 105.44, 32.85,
             158.89,  94.53, 30.94,
             165.53,  94.79, 32.25)
)
hist_appraised[, share := appr_b / sum(appr_b), by = tax_yr]

track_anchor <- hist_appraised[certified_anchor_tbl, on = "tax_yr"][
  , .(tax_yr, track, anchor_av = share * cert_real)]

# ---------------------------------- commercial subgroup maps (= main_ml.R) ----
COM_SPEC_AREA_MAP <- list(
  apt         = c("Apartment", "Nursinghome"),
  office      = c("MajorOffice"),
  industrial  = c("Industrial", "Warehouse"),
  retail      = c("MajorRetail"),
  hospitality = c("Hotel", "Restaurants"),
  medical     = c("Biotech")
)
COM_PU_MAP <- list(
  apt         = c(10L, 11L, 16L, 17L, 18L, 49L, 56L, 57L, 59L),
  office      = c(106L, 118L, 126L, 273L),
  industrial  = c(138L, 195L, 202L, 210L, 216L, 223L, 245L, 246L, 252L,
                  168L, 186L,
                  261L, 262L, 263L, 264L, 271L, 276L, 344L, 345L, 346L),
  retail      = c(60L, 61L, 62L, 63L, 64L, 96L, 101L, 104L, 105L,
                  161L, 162L, 163L, 167L, 191L, 194L, 274L),
  hospitality = c(51L, 58L, 171L, 183L, 188L, 275L, 278L, 340L, 341L),
  medical     = c(55L, 122L, 173L)
)
COM_PU_EXCLUDE <- c(299L, 300L, 301L, 309L, 316L,
                    323L, 324L, 325L, 326L, 327L, 328L,
                    330L, 331L, 332L, 333L, 334L, 335L, 336L, 337L,
                    339L, 159L, 180L, 182L, 277L)

# =============================================================================
# 1. VALUES
# =============================================================================
if (exists("av_history_cln", envir = .GlobalEnv)) {
  av_dt <- as.data.table(get("av_history_cln", envir = .GlobalEnv))
} else if (file.exists(CFG_HIST$cache_rds)) {
  av_dt <- as.data.table(readRDS(CFG_HIST$cache_rds))
  message("  av_history_cln loaded from cache")
} else {
  stop("av_history_cln unavailable -- run run_main_ml(replicate = TRUE) once.")
}

av_dt <- av_dt[, .(
  parcel_id = gsub("-", "", parcel_id),
  tax_yr    = as.integer(tax_yr),
  total_assessed = fifelse(
    is.na(appr_land_val) & is.na(appr_imps_val), NA_real_,
    fifelse(is.na(appr_land_val), appr_imps_val,
            fifelse(is.na(appr_imps_val), appr_land_val,
                    appr_land_val + appr_imps_val)))
)][!is.na(total_assessed) & total_assessed > 0]

# =============================================================================
# 2. CLASSIFICATION
# =============================================================================
kca_root <- here::here("data", "kca", CFG_HIST$kca_date)

# ---- condo membership: EXTR_CondoUnit (units are NOT in EXTR_Parcel) -------
condo_path <- file.path(kca_root, "EXTR_CondoUnit.csv")
if (!file.exists(condo_path)) stop("Missing: ", condo_path)
condo_ids <- fread(file = condo_path, select = c("Major", "Minor"),
                   na.strings = c("", "NA", " "), encoding = "Latin-1")
condo_ids <- unique(condo_ids[, .(
  parcel_id = paste0(str_pad(trimws(as.character(Major)), 6, "left", "0"),
                     str_pad(trimws(as.character(Minor)), 4, "left", "0")))])

# ---- EXTR_Parcel minimal read ----------------------------------------------
parcel_path <- file.path(kca_root, "EXTR_Parcel.csv")
if (!file.exists(parcel_path)) stop("Missing: ", parcel_path)
hdr <- names(fread(file = parcel_path, nrows = 0L))
sel <- intersect(c("Major", "Minor", "PropType", "PresentUse", "SpecArea"), hdr)
p_lkp <- fread(file = parcel_path, select = sel,
               na.strings = c("", "NA", " "), encoding = "Latin-1")
p_lkp <- p_lkp[, .(
  parcel_id   = paste0(str_pad(trimws(as.character(Major)), 6, "left", "0"),
                       str_pad(trimws(as.character(Minor)), 4, "left", "0")),
  prop_type   = if ("PropType" %in% sel)
                  toupper(trimws(as.character(PropType))) else NA_character_,
  present_use = suppressWarnings(as.integer(PresentUse)),
  spec_area   = suppressWarnings(as.integer(SpecArea))
)]
p_lkp <- unique(p_lkp, by = "parcel_id")

spec_name_dt <- tryCatch({
  xw <- as.data.table(readxl::read_excel(CFG_HIST$xwalk_xlsx,
                                         sheet = "commercial2"))
  unique(xw[, .(spec_area = as.integer(spec_area),
                spec_area_name = as.character(name))], by = "spec_area")
}, error = function(e) {
  warning("crosswalk 'commercial2' unavailable -- present_use pass only")
  data.table(spec_area = integer(), spec_area_name = character())
})
p_lkp <- spec_name_dt[p_lkp, on = "spec_area"]

spec_lookup <- rbindlist(lapply(names(COM_SPEC_AREA_MAP), \(k)
  data.table(spec_area_name = COM_SPEC_AREA_MAP[[k]], sub = k)))
pu_lookup   <- rbindlist(lapply(names(COM_PU_MAP), \(k)
  data.table(present_use = COM_PU_MAP[[k]], sub = k)))
p_lkp[spec_lookup, sub_spec := i.sub, on = "spec_area_name"]
p_lkp[pu_lookup,   sub_pu   := i.sub, on = "present_use"]

p_lkp[, group := fcase(
  prop_type == "R", "residential",
  prop_type == "K", "condo",
  prop_type == "C" & !is.na(sub_spec), paste0("com_", sub_spec),
  prop_type == "C" & present_use %in% COM_PU_EXCLUDE, "com_other",
  prop_type == "C" & !is.na(sub_pu),   paste0("com_", sub_pu),
  prop_type == "C", "com_other",
  default = "real_other"
)]

av_dt[p_lkp, group := i.group, on = "parcel_id"]
av_dt[condo_ids, group := "condo", on = "parcel_id"]
av_dt[is.na(group), group := "real_unclassified"]

message("  unclassified share 2022+: ",
        sprintf("%.1f%%", 100 * av_dt[tax_yr >= 2022,
                                      mean(group == "real_unclassified")]))

# =============================================================================
# 3. RAW AGGREGATES + GROWTH CHAINS
# =============================================================================
agg <- av_dt[, .(n_parcels = uniqueN(parcel_id), av_raw = sum(total_assessed)),
             by = .(tax_yr, group)]
setorder(agg, group, tax_yr)
agg[, gyy_raw := av_raw / shift(av_raw) - 1, by = group]

com_sub_groups <- c("com_apt", "com_office", "com_industrial", "com_retail",
                    "com_hospitality", "com_medical", "com_other")

agg[, track := fcase(group == "residential", "res",
                     group == "condo", "condo",
                     group %in% com_sub_groups, "com",
                     default = "drop")]

# ---- MATCHED-PARCEL growth (same convention as the forecast chains) --------
# Aggregate YoY confounds value growth with parcels entering/leaving the
# history: e.g. ~330 large office parcels are missing their 2016-2017
# ValueHistory rows, cratering the aggregate sum ($13.7B -> $5.2B) and then
# snapping back.  Matched growth compares only parcels present in BOTH
# adjacent years, so coverage gaps drop out of numerator and denominator.
setkey(av_dt, parcel_id, tax_yr)
av_dt[, `:=`(av_lag = shift(total_assessed, 1L),
             yr_lag = shift(tax_yr, 1L)), by = parcel_id]

mtch <- av_dt[yr_lag == tax_yr - 1L]

gyy_grp <- mtch[, .(num = sum(total_assessed), den = sum(av_lag),
                    n_matched = .N), by = .(group, tax_yr)]
gyy_grp[, gyy := num / den - 1]

gyy_trk <- mtch[group %in% c("residential", "condo", com_sub_groups)]
gyy_trk[, track := fcase(group == "residential", "res",
                         group == "condo", "condo",
                         default = "com")]
gyy_trk <- gyy_trk[, .(num = sum(total_assessed), den = sum(av_lag)),
                   by = .(track, tax_yr)]
gyy_trk[, gyy := num / den - 1]

# chains consume `gyy` from agg / trk -- attach matched rates there
agg[gyy_grp, `:=`(gyy = i.gyy, n_matched = i.n_matched),
    on = .(group, tax_yr)]
trk <- gyy_trk[, .(track, tax_yr, gyy)]

# flag residual coverage swings for QA (matched chain is robust to them,
# but they mark years where the RAW sums are not publishable)
cov <- agg[group %in% c("residential", "condo", com_sub_groups)]
cov[, n_chg := n_parcels / shift(n_parcels) - 1, by = group]
jumpy <- cov[abs(n_chg) > 0.10 & tax_yr <= 2021,
             .(group, tax_yr, n_chg = round(n_chg, 2))]
if (nrow(jumpy)) {
  message("  Coverage swings >10% (handled by matched chain; raw sums ",
          "unreliable in these group-years):")
  print(jumpy)
}

# =============================================================================
# 4. HISTORY: ANCHOR 2022-2026 + CHAIN BACK 2001-2021
# =============================================================================
chain_back <- function(anchor_2022, growth_dt) {
  yrs <- sort(unique(growth_dt$tax_yr))
  yrs <- yrs[yrs <= 2022]
  out <- data.table(tax_yr = yrs, av = NA_real_)
  out[tax_yr == 2022, av := anchor_2022]
  for (y in rev(yrs[yrs < 2022])) {
    g <- growth_dt[tax_yr == y + 1, gyy]
    nxt <- out[tax_yr == y + 1, av]
    out[tax_yr == y, av := if (length(g) == 1 && !is.na(g)) nxt / (1 + g)
                           else NA_real_]
  }
  out
}

# ---- optional facets export: exact overlap + forecast source ---------------
facets_series <- NULL
if (file.exists(CFG_HIST$facets_series_csv)) {
  facets_series <- fread(CFG_HIST$facets_series_csv)
  stopifnot(all(c("tax_yr", "group", "av") %in% names(facets_series)))
  for (cc in c("av_pes", "av_opt"))
    if (!cc %in% names(facets_series)) facets_series[, (cc) := NA_real_]
  message("  facets_series.csv found: anchors + forecast taken from it")
}

anchor_for <- function(key, internal_anchor) {
  # facets history rows (2022-2026) override the internal anchors when given
  if (!is.null(facets_series) &&
      nrow(facets_series[group == key & tax_yr %in% 2022:2026]) == 5L) {
    facets_series[group == key & tax_yr %in% 2022:2026, .(tax_yr, av)]
  } else internal_anchor
}

series_list <- list()

for (tr in c("res", "com", "condo")) {
  anch <- anchor_for(tr, track_anchor[track == tr,
                                      .(tax_yr, av = anchor_av)])
  back <- chain_back(anch[tax_yr == 2022, av], trk[track == tr])
  series_list[[tr]] <- rbind(
    back[tax_yr < 2022, .(tax_yr, group = tr, av, basis = "chained")],
    anch[, .(tax_yr, group = tr, av, basis = "certified")])
}

com_anchor <- track_anchor[track == "com", .(tax_yr, com_av = anchor_av)]
sub_share <- agg[group %in% com_sub_groups & tax_yr >= 2022,
                 .(tax_yr, group, av_raw)]
sub_share[, share := av_raw / sum(av_raw), by = tax_yr]

for (sg in com_sub_groups) {
  anch <- anchor_for(sg, sub_share[group == sg][com_anchor, on = "tax_yr"][
    , .(tax_yr, av = share * com_av)])
  back <- chain_back(anch[tax_yr == 2022, av],
                     agg[group == sg, .(tax_yr, gyy)])
  series_list[[sg]] <- rbind(
    back[tax_yr < 2022, .(tax_yr, group = sg, av, basis = "chained")],
    anch[, .(tax_yr, group = sg, av, basis = "certified")])
}

pp_cagr <- (certified_anchor_tbl[tax_yr == 2026, pp_av] /
            certified_anchor_tbl[tax_yr == 2022, pp_av])^(1 / 4) - 1
pp_back <- data.table(tax_yr = 2001:2021)
pp_back[, av := certified_anchor_tbl[tax_yr == 2022, pp_av] /
          (1 + pp_cagr)^(2022 - tax_yr)]
series_list[["pp"]] <- rbind(
  pp_back[, .(tax_yr, group = "pp", av, basis = "cagr_backcast")],
  certified_anchor_tbl[, .(tax_yr, group = "pp", av = pp_av,
                           basis = "certified")])

hist_long <- rbindlist(series_list, use.names = TRUE)

tot <- hist_long[group %in% c("res", "com", "condo", "pp"),
                 .(av = sum(av)), by = tax_yr]
tot[, `:=`(group = "total",
           basis = fifelse(tax_yr >= 2022, "certified", "chained"))]
hist_long <- rbind(hist_long, tot, use.names = TRUE)

tie <- tot[certified_anchor_tbl, on = "tax_yr", nomatch = NULL]
stopifnot(all(abs(tie$av - tie$cert_total) < 1000))
message("  Tie check passed: total == certified regular levy 2022-2026")

hist_long[, `:=`(av_pes = NA_real_, av_opt = NA_real_)]

# =============================================================================
# 5. FORECAST 2027-2031 (imported)
# =============================================================================
fcst_long <- NULL

if (!is.null(facets_series)) {
  fcst_long <- facets_series[tax_yr %in% FORECAST_YRS,
                             .(tax_yr, group, av, av_pes, av_opt,
                               basis = "forecast")]
} else {
  # Fallback: Total only, from the reconcile script's NC-inclusive summary
  cand <- list.files(CFG_HIST$wrangled_dir,
                     pattern = "^av_certified_total_summary_.*\\.csv$",
                     full.names = TRUE)
  if (length(cand)) {
    ts_csv <- cand[order(cand)][length(cand)]           # latest stamp
    ts <- fread(ts_csv)
    stopifnot(all(c("tax_yr", "baseline") %in% names(ts)))
    fcst_long <- ts[tax_yr %in% FORECAST_YRS,
                    .(tax_yr, group = "total", av = baseline,
                      av_pes = if ("pessimistic" %in% names(ts))
                        pessimistic else NA_real_,
                      av_opt = if ("optimistic" %in% names(ts))
                        optimistic else NA_real_,
                      basis = "forecast")]
    message("  Total forecast from: ", basename(ts_csv),
            " (other panels end at 2026 -- export facets_series.csv from",
            " av_forecast_facets.R for all panels)")
  } else {
    message("  No forecast source found -- history only.",
            " Export facets_series.csv or run av_reconcile_certified.R.")
  }
}

full_long <- rbind(hist_long, fcst_long, use.names = TRUE, fill = TRUE)
setorder(full_long, group, tax_yr)
full_long[, yoy := av / shift(av) - 1, by = group]

# =============================================================================
# 6. EXPORT
# =============================================================================
dir.create(CFG_HIST$wrangled_dir, showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.Date(), "%Y-%m-%d")

fwrite(full_long,
       file.path(CFG_HIST$wrangled_dir,
                 paste0("av_history_forecast_long_", stamp, ".csv")))
fwrite(dcast(full_long, tax_yr ~ group, value.var = "av"),
       file.path(CFG_HIST$wrangled_dir,
                 paste0("av_history_forecast_wide_", stamp, ".csv")))
fwrite(agg[, .(tax_yr, group, n_parcels, n_matched, av_raw,
               gyy_raw, gyy_matched = gyy)],
       file.path(CFG_HIST$wrangled_dir,
                 paste0("av_history_raw_detail_", stamp, ".csv")))
message("  Wrote history+forecast long/wide + raw detail CSVs")

# =============================================================================
# 7. CHART -- facets aesthetics; solid history, dashed forecast, no dots
# =============================================================================
if (isTRUE(CFG_HIST$make_plot)) {

  panel_map <- c(total = "Total AV (With NC + PP)", res = "Residential",
                 com_apt = "Apartment", com_office = "Major Office",
                 com_industrial = "Industrial", com_retail = "Retail",
                 com_hospitality = "Hospitality", com_medical = "Medical",
                 condo = "Condo")

  pal <- c("Total AV (With NC + PP)" = "#404040", "Residential" = "#2E75B6",
           "Apartment" = "#C00000",  "Major Office" = "#843C0C",
           "Industrial" = "#7030A0", "Retail" = "#BF8F00",
           "Hospitality" = "#0F8E8E", "Medical" = "#D81B60",
           "Condo" = "#548235")

  theme_av <- theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
      axis.title.x = element_blank(),
      strip.text = element_text(face = "bold")
    )

  pd <- full_long[group %in% names(panel_map)]
  pd[, series  := factor(panel_map[group], levels = unname(panel_map))]
  pd[, segment := fifelse(tax_yr <= 2026, "Historical", "Forecast")]

  # Bridge the 2026 point into the forecast segment so the lines connect
  bridge <- copy(pd[tax_yr == 2026])
  bridge[, segment := "Forecast"]
  has_fcst <- unique(pd[segment == "Forecast", group])
  pd <- rbind(pd, bridge[group %in% has_fcst], use.names = TRUE)

  p <- ggplot(pd, aes(tax_yr, av / 1e9, color = series)) +
    geom_ribbon(data = pd[segment == "Forecast" & !is.na(av_pes)],
                aes(ymin = av_pes / 1e9, ymax = av_opt / 1e9, fill = series),
                alpha = 0.15, color = NA) +
    geom_line(aes(linetype = segment), linewidth = 1.0) +
    geom_vline(xintercept = 2026.5, linetype = "dotted", color = "gray60",
               linewidth = 0.4) +
    facet_wrap(~series, scales = "free_y", ncol = 3) +
    scale_x_continuous(breaks = seq(2002, 2030, 4)) +
    scale_y_continuous(labels = dollar_format(suffix = "B")) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_linetype_manual(values = c("Historical" = "solid",
                                     "Forecast" = "dashed"), guide = "none") +
    labs(title = "Seattle Assessed Value by Segment: History and Forecast (Certified Basis)",
         subtitle = paste0("2022-2026 certified decomposition; 2001-2021 ",
                           "chained on matched-parcel growth; 2027-2031 ",
                           "forecast (shaded = scenario range)"),
         caption = paste0("Regular-levy basis; certified totals embed each ",
                          "year's new construction and the Total forecast ",
                          "carries the NC layer forward. Pre-2022 PP ",
                          "estimated at 2022-2026 CAGR; subgroup splits use ",
                          "current-extract classification.\n",
                          "Source: KC Assessor, CoStar, S&P Global, OERF"),
         y = "Assessed Value ($B)") +
    theme_av

  ggsave(file.path(CFG_HIST$wrangled_dir,
                   paste0("av_history_forecast_facets_", stamp, ".png")),
         p, width = 14, height = 9, dpi = 150)
  message("  Wrote av_history_forecast_facets_", stamp, ".png")
}

message("av_history_series.R (v3) done.")
