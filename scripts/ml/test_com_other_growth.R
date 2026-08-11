suppressPackageStartupMessages({library(data.table); library(zoo)})
suppressPackageStartupMessages(library(here))
O <- Sys.getenv("ML_DIR", here::here("scripts", "ml"))
ok <- function(l,c) cat(ifelse(isTRUE(c),"PASS","**FAIL**"),"-",l,"\n")

exprs <- parse(file.path(O,"xx_com_other_growth.R"))
want <- c("com_subgroup_growth","com_other_rate_by_year","forecast_com_other")
for (e in exprs) if (is.call(e) && as.character(e[[1]]) %in% c("<-","=") &&
    is.name(e[[2]]) && as.character(e[[2]]) %in% want) eval(e)
ok("growth fns extracted", all(sapply(want, exists)))

## synthetic subgroup growth: office flat, retail +5%, industrial +2%
sg <- rbindlist(list(
  data.table(subgroup="office",     tax_yr=2027:2028, av=c(13.0,13.0)*1e9,
             av_lag=c(13.0,13.0)*1e9, dlog=c(0,0)),
  data.table(subgroup="retail",     tax_yr=2027:2028, av=c(10.5,11.0)*1e9,
             av_lag=c(10.0,10.5)*1e9, dlog=log(c(10.5/10,11/10.5))),
  data.table(subgroup="industrial", tax_yr=2027:2028, av=c(26.0,26.5)*1e9,
             av_lag=c(25.5,26.0)*1e9, dlog=log(c(26/25.5,26.5/26)))))

r_w <- com_other_rate_by_year(sg, "com_weighted")
manual <- weighted.mean(sg[tax_yr==2027]$dlog, sg[tax_yr==2027]$av_lag)
ok("com_weighted equals manual weighted.mean",
   isTRUE(all.equal(r_w[tax_yr==2027]$dlog_other, manual)))
ok("com_weighted is between min and max subgroup rate",
   r_w[tax_yr==2027]$dlog_other > min(sg[tax_yr==2027]$dlog) &&
   r_w[tax_yr==2027]$dlog_other < max(sg[tax_yr==2027]$dlog))

r_b <- com_other_rate_by_year(sg, "blend",
        weights=c(office=.3, retail=.3, industrial=.1))
manual_b <- sum(sg[tax_yr==2027]$dlog * c(.3,.3,.1)) / .7
ok("blend renormalises missing subgroups",
   isTRUE(all.equal(r_b[tax_yr==2027]$dlog_other, manual_b)))

r_p <- com_other_rate_by_year(sg, "proxy", proxy="retail")
ok("proxy equals that subgroup's rate",
   isTRUE(all.equal(r_p[tax_yr==2027]$dlog_other, sg[subgroup=="retail" & tax_yr==2027]$dlog)))
ok("flat is zero", all(com_other_rate_by_year(sg,"flat")$dlog_other == 0))
ok("bad method errors", inherits(try(com_other_rate_by_year(sg,"nope"), silent=TRUE),"try-error"))

sg_hot <- copy(sg); sg_hot[, dlog := 0.9]
ok("cap applied", all(abs(com_other_rate_by_year(sg_hot,"com_weighted",cap=.15)$dlog_other) <= .15))
ok("cap flagged in basis", all(grepl("capped", com_other_rate_by_year(sg_hot,"com_weighted",cap=.15)$basis)))

## ---- forecast_com_other ---------------------------------------------------
yrs <- 2024:2031; FS <- 2027L
mk <- function(id, land, imps, area) data.table(
  parcel_id=id, tax_yr=yrs, area=area, spec_area=0L,
  appr_land_val=ifelse(yrs<FS, land, NA_real_),
  appr_imps_val=if (is.na(imps)) NA_real_ else ifelse(yrs<FS, imps, NA_real_))
co <- rbindlist(list(mk("A",1e6,4e6,30L), mk("B",2e6,NA,30L), mk("C",5e5,5e5,77L)))

assign("area_report_actuals", data.table(
  area=c("30"), assessment_yr=2026L, prop_type="com", report_kind="geo",
  basis="population", pct_change=-0.02), envir=.GlobalEnv)

out <- forecast_com_other(co, FS, 2031L, r_w, use_kca_actuals=TRUE)

a27 <- out[parcel_id=="A" & tax_yr==2027]
ok("geo actual anchors 2027 for area 30", a27$co_rate_source=="geo_report")
ok("2027 land = 2026 land * exp(-0.0202)",
   isTRUE(all.equal(a27$pred_appr_land_val, 1e6*exp(log1p(-0.02)))))
ok("total = land + imps", isTRUE(all.equal(a27$pred_total_assessed,
   a27$pred_appr_land_val + a27$pred_appr_imps_val)))

a28 <- out[parcel_id=="A" & tax_yr==2028]
ok("2028 compounds off 2027, not off 2026",
   isTRUE(all.equal(a28$pred_appr_land_val,
                    1e6*exp(log1p(-0.02) + r_w[tax_yr==2028]$dlog_other))))
ok("2028 uses the modelled rate", a28$co_rate_source=="value_weighted_com")

b <- out[parcel_id=="B" & tax_yr==2028]
ok("land-only parcel keeps NA improvement", is.na(b$pred_appr_imps_val))
ok("land-only total = land only", isTRUE(all.equal(b$pred_total_assessed, b$pred_appr_land_val)))

c27 <- out[parcel_id=="C" & tax_yr==2027]
ok("area with no published rate falls to model", c27$co_rate_source=="value_weighted_com")

ok("historical years untouched",
   isTRUE(all.equal(out[tax_yr==2026 & parcel_id=="A"]$pred_appr_land_val, 1e6)))
ok("no NA totals in forecast years",
   out[tax_yr>=FS, sum(is.na(pred_total_assessed))]==0)
ok("all forecast rows have a rate source",
   out[tax_yr>=FS, sum(is.na(co_rate_source))]==0)
ok("scratch cols removed",
   !any(c(".obs_land",".dlog",".cum",".is_specialty") %in% names(out)))

## specialty parcel must not take the geo rate under nonspecialty scope
co2 <- copy(co); co2[parcel_id=="A", spec_area := 280L]
out2 <- forecast_com_other(co2, FS, 2031L, r_w, use_kca_actuals=TRUE,
                           geo_scope="nonspecialty")
ok("specialty parcel skips geo rate",
   out2[parcel_id=="A" & tax_yr==2027]$co_rate_source=="value_weighted_com")
out3 <- forecast_com_other(co2, FS, 2031L, r_w, use_kca_actuals=TRUE, geo_scope="all")
ok("geo_scope='all' lets it through",
   out3[parcel_id=="A" & tax_yr==2027]$co_rate_source=="geo_report")

## flat method must reproduce the old carry-forward exactly
outf <- forecast_com_other(co, FS, 2031L, com_other_rate_by_year(sg,"flat"),
                           use_kca_actuals=FALSE)
ok("flat reproduces carry-forward",
   all(outf[tax_yr>=FS & parcel_id=="A"]$pred_total_assessed == 5e6))
