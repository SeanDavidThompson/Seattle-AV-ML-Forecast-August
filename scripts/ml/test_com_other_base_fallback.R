suppressPackageStartupMessages({library(data.table); library(zoo); library(stringr)})
suppressPackageStartupMessages(library(here))
O <- Sys.getenv("ML_DIR", here::here("scripts", "ml"))
ok <- function(l,c) cat(ifelse(isTRUE(c),"PASS","**FAIL**"),"-",l,"\n")
for (e in parse(file.path(O,"xx_com_other_growth.R")))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-","=") && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("com_other_rate_by_year","forecast_com_other")) eval(e)

yrs <- 2024:2031; FS <- 2027L
rate <- data.table(tax_yr=2027:2031, dlog_other=log(1.02), basis="value_weighted_com")

## Sean's exact failure: no appr_* columns at all, only total_assessed
co <- rbindlist(lapply(c("A","B"), function(id) data.table(
  parcel_id=id, tax_yr=yrs, spec_area=0L,
  total_assessed=ifelse(yrs<FS, 1e6, NA_real_))))
out <- forecast_com_other(co, FS, 2031L, rate, use_kca_actuals=FALSE)
ok("total-only panel no longer yields $0",
   out[tax_yr>=FS, sum(pred_total_assessed, na.rm=TRUE)] > 0)
ok("2027 = base * 1.02", isTRUE(all.equal(
   out[parcel_id=="A" & tax_yr==2027]$pred_total_assessed, 1e6*1.02)))
ok("2031 compounds 5 years", isTRUE(all.equal(
   out[parcel_id=="A" & tax_yr==2031]$pred_total_assessed, 1e6*1.02^5)))
ok("historical untouched", isTRUE(all.equal(
   out[parcel_id=="A" & tax_yr==2026]$pred_total_assessed, 1e6)))

## log_total_assessed only
co2 <- copy(co); co2[, log_total_assessed := log(total_assessed)][, total_assessed := NULL]
out2 <- forecast_com_other(co2, FS, 2031L, rate, use_kca_actuals=FALSE)
ok("log_total_assessed fallback works",
   isTRUE(all.equal(out2[parcel_id=="A" & tax_yr==2027]$pred_total_assessed, 1e6*1.02)))

## split still preferred when present
co3 <- copy(co); co3[, appr_land_val := ifelse(tax_yr<FS, 4e5, NA_real_)]
co3[, appr_imps_val := ifelse(tax_yr<FS, 6e5, NA_real_)]
out3 <- forecast_com_other(co3, FS, 2031L, rate, use_kca_actuals=FALSE)
r <- out3[parcel_id=="A" & tax_yr==2027]
ok("split path still populates land+imps",
   !is.na(r$pred_appr_land_val) && !is.na(r$pred_appr_imps_val))
ok("split total = land + imps", isTRUE(all.equal(
   r$pred_total_assessed, r$pred_appr_land_val + r$pred_appr_imps_val)))

## genuinely unusable -> warns, still no crash
co4 <- copy(co); co4[, total_assessed := NA_real_]
w <- NULL
out4 <- withCallingHandlers(
  forecast_com_other(co4, FS, 2031L, rate, use_kca_actuals=FALSE),
  warning=function(x){ w <<- conditionMessage(x); invokeRestart("muffleWarning") })
ok("empty base warns loudly", !is.null(w) && grepl("\\$0", w))
