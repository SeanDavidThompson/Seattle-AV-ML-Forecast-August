suppressPackageStartupMessages({library(data.table); library(zoo); library(stringr)})
suppressPackageStartupMessages(library(here))
O <- Sys.getenv("ML_DIR", here::here("scripts", "ml"))
ok <- function(lbl, cond) cat(ifelse(isTRUE(cond),"PASS","**FAIL**"), "-", lbl, "\n")

## ---- 1. COM_SUBGROUPS integrity (from patched main_ml.R) -------------------
src <- readLines(file.path(O,"main_ml.R"), warn=FALSE)
i0 <- grep("^COM_SUBGROUPS <- list\\($", src)
i1 <- grep("^COM_SUBGROUP_KEYS <- names\\(COM_SUBGROUPS\\)$", src)
eval(parse(text = paste(src[i0:i1], collapse="\n")))

all_pu <- unlist(lapply(COM_SUBGROUPS, `[[`, "present_use_codes"))
ok("6 subgroups defined", length(COM_SUBGROUPS)==6)
ok("no present_use code in two subgroups", !any(duplicated(all_pu)))
ok("no present_use code also in COM_PU_EXCLUDE", length(intersect(all_pu, COM_PU_EXCLUDE))==0)
ok("300/309 (vacant) no longer in office", !any(c(300L,309L) %in% COM_SUBGROUPS$office$present_use_codes))
ok("300/309 are excluded", all(c(300L,309L) %in% COM_PU_EXCLUDE))
ok("restaurants (171,183) in hospitality", all(c(171L,183L) %in% COM_SUBGROUPS$hospitality$present_use_codes))
ok("office building 106 in office", 106L %in% COM_SUBGROUPS$office$present_use_codes)

## every code must exist in LookUp type 102
lk <- fread(Sys.getenv("LOOKUP_CSV", here::here("data", "kca", "EXTR_LookUp.csv")))
pu102 <- as.integer(trimws(lk[trimws(LUType)=="102", LUItem]))
bad <- setdiff(all_pu, pu102)
ok(paste0("all ", length(all_pu), " codes exist in LookUp type 102"), length(bad)==0)
if(length(bad)) print(bad)

## ---- 2. classifier helpers -------------------------------------------------
exprs <- parse(file.path(O,"xx_com_classify.R"))
want <- c("classify_use118","COM_KEYWORDS","COM_KEYWORD_NOISE",
          "normalise_text","keyword_classify","zoning_classify",
          "COM_SUBGROUPS_PU","PU_VACANT","PU_PARKING")
for (e in exprs) {
  if (is.call(e) && length(e) >= 3 &&
      as.character(e[[1]]) %in% c("<-","=") &&
      is.name(e[[2]]) && as.character(e[[2]]) %in% want) eval(e)
}
ok("helpers extracted", all(sapply(want, exists)))

ok("use118 344 -> office",      classify_use118(344L)=="office")
ok("use118 300 -> apt",         classify_use118(300L)=="apt")
ok("use118 341 -> medical",     classify_use118(341L)=="medical")
ok("use118 349/350 -> hospitality", all(classify_use118(c(349L,350L))=="hospitality"))
ok("use118 353 -> retail",      classify_use118(353L)=="retail")
ok("use118 406/407 -> industrial", all(classify_use118(c(406L,407L))=="industrial"))
ok("use118 NA safe",            is.na(classify_use118(NA_integer_)))
ok("use118 out-of-range NA",    is.na(classify_use118(999L)))

kw <- keyword_classify(c("SEATTLE MEDICAL OFFICE BUILDING","HILTON HOTEL & SUITES",
                         "ACME PROPERTIES LLC","BALLARD SELF STORAGE",
                         "QFC GROCERY STORE","THE TOWER OFFICE","RIVERVIEW APARTMENTS",
                         NA, "", "SMITH ASSOCIATES INC"))
ok("medical beats office in text",  kw[1]=="medical")
ok("hotel -> hospitality",          kw[2]=="hospitality")
ok("pure entity name -> no match",  is.na(kw[3]))
ok("self storage -> industrial",    kw[4]=="industrial")
ok("grocery -> retail",             kw[5]=="retail")
ok("tower/office -> office",        kw[6]=="office")
ok("apartments -> apt",             kw[7]=="apt")
ok("NA/empty safe",                 all(is.na(kw[8:9])))
ok("ASSOCIATES INC -> no match",    is.na(kw[10]))

pl <- keyword_classify(c("RIVERVIEW APARTMENTS","DOWNTOWN OFFICES",
                         "TWO RESTAURANTS","INDUSTRIAL WAREHOUSES",
                         "AIRPORT HOTELS","RETAIL STORES","MEDICAL CLINICS"))
ok("plurals: APARTMENTS/OFFICES/RESTAURANTS/WAREHOUSES/HOTELS/STORES/CLINICS",
   identical(pl, c("apt","office","hospitality","industrial","hospitality",
                   "retail","medical")))

ok("zoning IG1 -> industrial", zoning_classify("IG1 U/45")=="industrial")
ok("zoning DOC1 -> office",    zoning_classify("DOC1 U/450/U")=="office")
ok("zoning NC3P -> retail",    zoning_classify("NC3P-75 (M)")=="retail")
ok("zoning LR3 -> apt",        zoning_classify("LR3 (M)")=="apt")
ok("zoning NA safe",           is.na(zoning_classify(NA)))
