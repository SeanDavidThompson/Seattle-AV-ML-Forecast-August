# area_report_import.R -----------------------------------------------------
# Scrape actual assessed-value growth rates from KCA area revalue reports.
#
# Inputs:  PDF area reports placed in
#            here("data", "kca", "area_reports", <area_reports_year>)
#          Two report templates are recognised automatically:
#            (1) Residential revalue reports (e.g. "Area 1 - West Shoreline ...
#                Residential Revalue for 2026 Assessment Roll").  Actuals come
#                from the Executive Summary "Improved Valuation Change Summary"
#                table.  Both the "Sales" and "Pop" rows are captured; values
#                are MEAN AV per parcel.  The population ("Pop") rows are the
#                actual roll growth for the area.
#            (2) Commercial district reports (North/Central/South "Geographic
#                Areas Report ... Commercial Revalue").  Actuals come from the
#                per-Geo-Area "Change in Total Assessed Value" / "Population -
#                Parcel Summary Data" table; values are TOTAL AV.
#            (3) Commercial specialty reports (Major Office 280, Apartments 100,
#                etc.).  Two headline layouts are recognised: "CHANGE IN TOTAL
#                ASSESSED VALUE" and the "Population Value Summary" table
#                (Land / Improvements / Total).  Where the report has a
#                "Percent Change - Total Values" table by region and a
#                "Project Inventory - Regions and Neighborhoods" table (the
#                Apartments report), one row per region is added with the
#                neighborhoods that region covers.
#            (4) Residential condominium reports (Specialty 700, e.g. "Capitol
#                Hill / Areas: 35, 40, 65, 70, AND 85 / Residential Condominium
#                Revalue for 2026 Assessment Roll").  Actuals come from the
#                Executive Summary "Population - Improved Parcel Summary Data"
#                table (MEAN AV per living unit; land, imps and total), plus the
#                Sales rows.  One row per report; `nbhds` lists the condo
#                neighborhoods it covers.
#                These reports also contain the word "Specialty", so they used
#                to be routed to the commercial specialty parser, which found no
#                table and dropped them with a warning.
#
# Globals expected in .GlobalEnv (set by run_main_ml()):
#   area_reports_year   assessment year of the reports (e.g. 2026)
#   cache_dir, output_dir
#
# Output:
#   area_report_actuals (tibble) assigned to .GlobalEnv, cached to
#   <cache_dir>/area_report_actuals_<year>.rds and written to
#   <output_dir>/area_report_actuals_<year>.csv
#
# Columns:
#   assessment_yr  int    e.g. 2026 (values are as of 1/1 of this year)
#   prop_type      chr    "res" | "com" | "condo" (which report template it came from)
#   report_kind    chr    "geo" | "specialty" | "specialty_submarket" | "condo"
#   area           int    KCA appraisal area / geo area code (NA for condo/specialty)
#   spec_area      int    specialty area (700 for condo reports)
#   spec_sub       int    specialty submarket
#   spec_region    int    specialty region (Apartments: 1 Central/North, 2 South, 3 East)
#   basis          chr    "population" (full roll) | "sales" (ratio sample,
#                         res and condo reports)
#   av_prev        num    prior-year AV  (res/condo = mean per parcel; com = total)
#   av_curr        num    current-year AV (same measure as av_prev)
#   delta          num    av_curr - av_prev
#   pct_change     num    reported % change as a decimal (e.g. -0.047)
#   pct_change_chk num    delta / av_prev, recomputed for validation
#   land_prev, land_curr, imps_prev, imps_curr   num  component AV (condo, specialty)
#   pct_land, pct_imps    num  reported component % change as a decimal (condo, specialty)
#   n_parcels      int    improved parcels in the population (condo only)
#   nbhds          chr    comma-separated neighborhoods covered (condo reports and
#                         specialty regions)
#   area_name      chr    report area / region name, e.g. "Capitol Hill", "South"
#   report_id      chr    PDF file stem, e.g. "700_01"
#   district       chr    first title line of the report
#   source_file    chr    PDF file name
# ---------------------------------------------------------------------------

if (!requireNamespace("pdftools", quietly = TRUE))
  stop("area_report_import.R requires the {pdftools} package.\n",
       "Install with: install.packages(\"pdftools\")")

area_reports_year <- if (exists("area_reports_year", envir = .GlobalEnv))
  get("area_reports_year", envir = .GlobalEnv) else 2026L
cache_dir  <- get("cache_dir",  envir = .GlobalEnv)
output_dir <- get("output_dir", envir = .GlobalEnv)

area_reports_dir <- here::here("data", "kca", "area_reports",
                               as.character(area_reports_year))

# ---- Helpers --------------------------------------------------------------

# "$1,143,400" / "-$57,400" / "$ -15,497,275" -> numeric
# (strip everything but digits/dot; sign detected separately since the minus
#  can precede or follow the dollar sign in these reports)
.num <- function(x) {
  # Specialty reports render negatives in parentheses: "$ (1,889,223,800)"
  neg  <- grepl("-", x) | grepl("\\(", x)
  sign <- ifelse(neg, -1, 1)
  sign * as.numeric(gsub("[^0-9.]", "", x))
}

# Extract the numbered lines of a pdf as a character vector
.pdf_lines <- function(path) {
  txt <- pdftools::pdf_text(path)
  unlist(strsplit(paste(txt, collapse = "\n"), "\n"))
}

.report_title <- function(lines) {
  non_empty <- trimws(lines[nzchar(trimws(lines))])
  if (length(non_empty) == 0) return(NA_character_)
  non_empty[1]
}

# ---- Parser: residential revalue reports ----------------------------------
# Table rows look like (layout-preserved text):
#   Area 1 Sales  $1,200,800  $1,143,400  -$57,400  -4.8%  $1,255,900  91.5% ...
#   Area 1 Pop    $1,145,200  $1,091,700  -$53,500  -4.7%
parse_res_report <- function(lines, src) {
  pat <- paste0(
    "^\\s*Area\\s+(\\d+)\\s+(Sales|Pop)\\s+",
    "(-?\\$?\\s?[\\d,]+)\\s+",      # prev value
    "(-?\\$?\\s?[\\d,]+)\\s+",      # curr value
    "([-+]?\\$?\\s?[-+]?[\\d,]+)\\s+",  # $ change (sign can precede or follow $)
    "([-+]?[\\d.]+)\\s?%"               # % change
  )
  m <- regmatches(lines, regexec(pat, lines, perl = TRUE))
  m <- m[lengths(m) == 7]
  if (length(m) == 0) return(NULL)

  purrr::map_dfr(m, function(g) {
    tibble::tibble(
      prop_type   = "res",
      report_kind = "geo",
      spec_area   = NA_integer_,
      spec_sub    = NA_integer_,
      area        = as.integer(g[2]),
      basis      = ifelse(g[3] == "Pop", "population", "sales"),
      av_prev    = .num(g[4]),
      av_curr    = .num(g[5]),
      delta      = .num(g[6]),
      pct_change = .num(g[7]) / 100,
      source_file = src
    )
  })
}

# ---- Parser: commercial district reports ----------------------------------
# The per-area table follows a header line:
#   Geo Area   2025 Total Value   2026 Total Value   $ Change   % Change
# with rows:
#   10   $3,962,976,475   $3,947,479,200   -$15,497,275   -0.39%
# and terminates at the "Total" row.
parse_com_report <- function(lines, src) {
  hdr_idx <- grep("Geo\\s*Area\\s+\\d{4}\\s+Total Value", lines)
  if (length(hdr_idx) == 0) return(NULL)

  row_pat <- paste0(
    "^\\s*(\\d{1,3})\\s+",
    "(-?\\$\\s?[\\d,]+)\\s+",
    "(-?\\$\\s?[\\d,]+)\\s+",
    "(-?\\$\\s?-?[\\d,]+|\\$\\s?-[\\d,]+)\\s+",
    "(-?[\\d.]+)\\s?%"
  )

  out <- list()
  for (h in hdr_idx) {
    i <- h + 1
    while (i <= length(lines)) {
      ln <- lines[i]
      if (grepl("^\\s*Total", ln)) break                 # summary row -> stop
      g <- regmatches(ln, regexec(row_pat, ln, perl = TRUE))[[1]]
      if (length(g) == 6) {
        out[[length(out) + 1]] <- tibble::tibble(
          prop_type   = "com",
          report_kind = "geo",
          spec_area   = NA_integer_,
          spec_sub    = NA_integer_,
          area        = as.integer(g[2]),
          basis      = "population",
          av_prev    = .num(g[3]),
          av_curr    = .num(g[4]),
          delta      = .num(g[5]),
          pct_change = .num(g[6]) / 100,
          source_file = src
        )
      } else if (!grepl("^\\s*$", ln)) {
        break                                            # non-table line -> stop
      }
      i <- i + 1
    }
  }
  if (length(out) == 0) return(NULL)
  dplyr::bind_rows(out)
}

# ---- Specialty helpers -------------------------------------------------------
# "Population Value Summary" table (Apartments and similar):
#                   Land             Improvements            Total
#   2025 Value   $25,448,216,024   $74,488,754,374   $99,936,970,398
#   2026 Value   $23,909,798,440   $70,282,266,551   $94,192,064,991
#   Difference  ($1,538,417,584)  ($4,206,487,823)  ($5,744,905,407)
#    % Change        -6.05%           -5.65%            -5.75%
.spec_value_summary <- function(lines, window = 14) {
  money <- "(\\(?-?\\$?\\s?\\(?-?[\\d,]{4,}\\)?)"
  pct   <- "(\\(?[-+]?[\\d.]+\\)?)\\s?%"
  hdr <- grep("Population\\s*.?\\s*Value\\s+Summary", lines, ignore.case = TRUE, perl = TRUE)
  for (h in hdr) {
    win  <- lines[seq(h + 1, min(h + window, length(lines)))]
    r3   <- regmatches(win, regexec(paste0("^\\s*(\\d{4})\\s+Value\\s+", money, "\\s+",
                                           money, "\\s+", money), win, perl = TRUE))
    r3   <- r3[lengths(r3) == 5]
    if (length(r3) < 2) next
    yrs  <- as.integer(vapply(r3, `[`, "", 2))
    prev <- .num(r3[[which.min(yrs)]][3:5])
    curr <- .num(r3[[which.max(yrs)]][3:5])
    pr   <- regmatches(win, regexec(paste0("^\\s*%\\s*Change\\s+", pct, "\\s+", pct, "\\s+", pct),
                                    win, perl = TRUE))
    pr   <- pr[lengths(pr) == 4]
    pc   <- if (length(pr)) .num(pr[[1]][2:4]) / 100 else curr / prev - 1
    return(list(prev = prev, curr = curr, pct = pc))
  }
  NULL
}

# "Percent Change - Total Values" by region, plus the region -> neighborhood
# inventory.  Returns NULL when the report has neither.
.spec_regions <- function(lines) {
  j <- grep("Percent\\s+Change\\s*.?\\s*Total\\s+Values", lines, ignore.case = TRUE, perl = TRUE)
  if (!length(j)) return(NULL)
  reg <- list()
  for (i in seq(j[1] + 1, min(j[1] + 10, length(lines)))) {
    g <- regmatches(lines[i], regexec("^\\s*([A-Za-z][A-Za-z /&-]*?)\\s+(\\(?[-+]?[\\d.]+\\)?)\\s?%\\s*$",
                                      lines[i], perl = TRUE))[[1]]
    if (length(g) == 3 && !grepl("^Region", g[2], ignore.case = TRUE))
      reg[[length(reg) + 1]] <- data.frame(region = trimws(g[2]), pct = .num(g[3]) / 100)
  }
  if (!length(reg)) return(NULL)
  reg <- do.call(rbind, reg)
  county <- reg$pct[grepl("^County", reg$region, ignore.case = TRUE)]
  reg <- reg[!grepl("^County", reg$region, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(reg)) return(list(regions = NULL, county = county))

  # Region codes: "divided ... into three regions: Central (1), South (2), East (3)"
  txt  <- paste(lines, collapse = " ")
  defs <- regmatches(txt, gregexpr("([A-Z][a-z]+)\\s*\\((\\d)\\)", txt, perl = TRUE))[[1]]
  code_of <- setNames(as.integer(sub(".*\\((\\d)\\)", "\\1", defs)),
                      tolower(sub("\\s*\\(.*", "", defs)))
  reg$code <- NA_integer_
  for (k in seq_len(nrow(reg))) {
    hit <- names(code_of)[vapply(names(code_of), function(nm)
      grepl(nm, tolower(reg$region[k]), fixed = TRUE), logical(1))]
    if (length(hit)) reg$code[k] <- code_of[[hit[1]]]
  }
  reg$code[is.na(reg$code)] <- seq_len(nrow(reg))[is.na(reg$code)]   # fall back to order

  # Neighborhood inventory: columns headed R1 / R2 / R3
  reg$nbhds <- NA_character_
  inv <- grep("Project\\s+Inventory", lines, ignore.case = TRUE)
  if (length(inv)) {
    hdr_i <- inv[1] + which(grepl("\\bR1\\b.*\\bR2\\b", lines[(inv[1] + 1):min(inv[1] + 5, length(lines))]))[1]
    if (!is.na(hdr_i)) {
      hdr   <- lines[hdr_i]
      cuts  <- vapply(paste0("\\bR", 2:9, "\\b"), function(pt) regexpr(pt, hdr, perl = TRUE)[1], integer(1))
      cuts  <- cuts[cuts > 0] - 5
      end_i <- hdr_i + which(grepl("Total\\s*:", lines[(hdr_i + 1):length(lines)]))[1]
      if (is.na(end_i)) end_i <- min(hdr_i + 60, length(lines))
      trip  <- "(\\d{1,3})\\s+([A-Za-z][A-Za-z /.&'-]*?)\\s+(\\d[\\d,]*)(?=\\s|$)"
      nb <- list()
      for (i in seq(hdr_i + 1, end_i - 1)) {
        ln <- lines[i]
        mm <- gregexpr(trip, ln, perl = TRUE)[[1]]
        if (mm[1] < 0) next
        for (q in seq_along(mm)) {
          seg   <- substr(ln, mm[q], mm[q] + attr(mm, "match.length")[q] - 1)
          piece <- regmatches(seg, regexec(trip, seg, perl = TRUE))[[1]]
          nb[[length(nb) + 1]] <- data.frame(code = 1L + sum(mm[q] >= cuts),
                                             nbhd = as.integer(piece[2]),
                                             projects = as.integer(gsub(",", "", piece[4])))
        }
      }
      if (length(nb)) {
        nb <- do.call(rbind, nb)
        # Check against the "R 1 Total: 4,652" footers
        tot <- regmatches(lines[end_i], gregexpr("R\\s*(\\d)\\s*Total:\\s*([\\d,]+)", lines[end_i], perl = TRUE))[[1]]
        for (t in tot) {
          rc <- as.integer(sub("R\\s*(\\d).*", "\\1", t))
          want <- as.integer(gsub(",", "", sub(".*Total:\\s*", "", t)))
          got  <- sum(nb$projects[nb$code == rc])
          if (!identical(got, want))
            warning(sprintf("Region %d inventory parsed %d projects, report total is %d", rc, got, want),
                    call. = FALSE)
        }
        for (k in seq_len(nrow(reg)))
          reg$nbhds[k] <- paste(sort(nb$nbhd[nb$code == reg$code[k]]), collapse = ",")
      }
    }
  }
  list(regions = reg, county = county)
}

# ---- Parser: commercial SPECIALTY reports ---------------------------------
# e.g. "Major Office Buildings / Area: 280 / Commercial Revalue for 2026
# Assessment Roll".  These cover a countywide population (Major Office 280,
# Major Retail 250, Warehouses 500, Hotels 160, ...) valued by a specialty
# appraiser, and are the correct growth source for parcels carrying that
# spec_area.  The geographic district reports EXCLUDE these parcels.
#
# Captured:
#   (a) the specialty-wide total from "CHANGE IN TOTAL ASSESSED VALUE"
#   (b) per-submarket rows from the "Specialty Area Breakdown" table, so a
#       Seattle-only rate can be built instead of the countywide headline
parse_spec_report <- function(lines, src) {
  head_txt <- paste(lines[1:min(120, length(lines))], collapse = " ")

  sa <- regmatches(head_txt,
                   regexec("(?:Specialty(?:\\s+Area)?|Area:)\\s*(\\d{2,3})",
                           head_txt, perl = TRUE))[[1]]
  if (length(sa) < 2) {
    sa2 <- regmatches(src, regexec("(\\d{2,3})", src))[[1]]
    if (length(sa2) < 2) return(NULL)
    spec_no <- as.integer(sa2[2])
  } else {
    spec_no <- as.integer(sa[2])
  }

  out <- list()

  # (a) specialty-wide total -------------------------------------------------
  tot_hdr <- grep("CHANGE IN TOTAL ASSESSED VALUE", lines, ignore.case = TRUE)
  tot_pat <- paste0(
    "^\\s*(\\$\\s*[\\d,]+)\\s+",
    "(\\$\\s*[\\d,]+)\\s+",
    "(\\$?\\s*\\(?-?[\\d,]+\\)?)\\s+",
    "(-?[\\d.]+)\\s?%"
  )
  for (h in tot_hdr) {
    for (i in seq(h + 1, min(h + 6, length(lines)))) {
      g <- regmatches(lines[i], regexec(tot_pat, lines[i], perl = TRUE))[[1]]
      if (length(g) == 5) {
        out[[length(out) + 1]] <- tibble::tibble(
          prop_type   = "com",
          report_kind = "specialty",
          spec_area   = spec_no,
          spec_sub    = NA_integer_,
          area        = NA_integer_,
          basis       = "population",
          av_prev     = .num(g[2]),
          av_curr     = .num(g[3]),
          delta       = .num(g[4]),
          pct_change  = .num(g[5]) / 100,
          source_file = src
        )
        break
      }
    }
    if (length(out) > 0) break
  }

  # (a2) "Population Value Summary" headline (Apartments layout)
  if (length(out) == 0) {
    vs <- .spec_value_summary(lines)
    if (!is.null(vs)) {
      out[[length(out) + 1]] <- tibble::tibble(
        prop_type   = "com",
        report_kind = "specialty",
        spec_area   = spec_no,
        spec_sub    = NA_integer_,
        area        = NA_integer_,
        basis       = "population",
        av_prev     = vs$prev[3],
        av_curr     = vs$curr[3],
        delta       = vs$curr[3] - vs$prev[3],
        pct_change  = vs$pct[3],
        land_prev   = vs$prev[1],
        land_curr   = vs$curr[1],
        imps_prev   = vs$prev[2],
        imps_curr   = vs$curr[2],
        pct_land    = vs$pct[1],
        pct_imps    = vs$pct[2],
        source_file = src
      )
    }
  }

  # (a3) regional rates and their neighborhoods
  rg <- .spec_regions(lines)
  if (!is.null(rg)) {
    if (length(out) == 0 && length(rg$county)) {
      out[[length(out) + 1]] <- tibble::tibble(
        prop_type = "com", report_kind = "specialty", spec_area = spec_no,
        spec_sub = NA_integer_, area = NA_integer_, basis = "population",
        av_prev = NA_real_, av_curr = NA_real_, delta = NA_real_,
        pct_change = rg$county[1], source_file = src)
    }
    if (!is.null(rg$regions)) {
      for (k in seq_len(nrow(rg$regions))) {
        out[[length(out) + 1]] <- tibble::tibble(
          prop_type   = "com",
          report_kind = "specialty_region",
          spec_area   = spec_no,
          spec_sub    = NA_integer_,
          spec_region = as.integer(rg$regions$code[k]),
          area        = NA_integer_,
          basis       = "population",
          av_prev     = NA_real_,
          av_curr     = NA_real_,
          delta       = NA_real_,
          pct_change  = rg$regions$pct[k],
          nbhds       = rg$regions$nbhds[k],
          area_name   = rg$regions$region[k],
          source_file = src
        )
      }
    }
  }

  # (b) per-submarket rows:
  #   "280-120 Central Business District 69 $ 7,457,718,200 $ 108,082,872 -8.66%"
  sub_pat <- paste0(
    "^\\s*(\\d{2,3})[-\\s]+(\\d{3})\\s+",
    ".*?\\$\\s*([\\d,]+)\\s+",
    "\\$\\s*[\\d,]+\\s+",
    "(-?[\\d.]+)\\s?%"
  )
  m <- regmatches(lines, regexec(sub_pat, lines, perl = TRUE))
  m <- m[lengths(m) == 5]
  for (g in m) {
    if (as.integer(g[2]) != spec_no) next
    out[[length(out) + 1]] <- tibble::tibble(
      prop_type   = "com",
      report_kind = "specialty_submarket",
      spec_area   = spec_no,
      spec_sub    = as.integer(g[3]),
      area        = NA_integer_,
      basis       = "population",
      av_prev     = NA_real_,
      av_curr     = .num(g[4]),
      delta       = NA_real_,
      pct_change  = .num(g[5]) / 100,
      source_file = src
    )
  }

  if (length(out) == 0) return(NULL)
  # The breakdown table is reprinted on the per-region pages; keep one row per
  # submarket so the duplicate check downstream stays meaningful.
  res <- dplyr::bind_rows(out)
  if (!"spec_region" %in% names(res)) res$spec_region <- NA_integer_
  dplyr::distinct(res, report_kind, spec_area, spec_sub, spec_region, .keep_all = TRUE)
}

# ---- Parser: residential CONDOMINIUM reports (Specialty 700) -------------
# Executive Summary layout (pdftools text):
#   Neighborhoods:                   35, 40, 65, 70, AND 85.
#   ...Sales - Improved Valuation Change Summary
#     2025 Value       $165,700      $414,900      $580,600     $599,000   98.3%   6.86%
#     2026 Value       $160,300      $391,400      $551,700     $599,000   92.7%   5.72%
#      %Change         -3.3%          -5.7%         -5.0%        -5.6%    -16.62%
#   Population - Improved Parcel Summary Data:
#     2025 Value        $175,500      $365,100        $540,600
#     2026 Value        $170,000      $339,600        $509,600
#   Percent Change         -3.1%         -7.0%           -5.7%
#   Number of improved Parcels in the Population: 8,015
parse_condo_report <- function(lines, src) {
  money <- "(-?\\$\\s?-?[\\d,]+)"
  pct   <- "([-+]?[\\d.]+)\\s?%"
  row3  <- paste0("^\\s*(\\d{4})\\s+Value\\s+", money, "\\s+", money, "\\s+", money)
  label <- "^\\s*(Neighbou?rhoods?|Areas?)\\s*:"

  # Neighborhood list (prefer the Executive Summary line over the cover line)
  nb_idx <- grep(label, lines, ignore.case = TRUE, perl = TRUE)
  nb_idx <- nb_idx[order(!grepl("^\\s*Neighbou?rhood", lines[nb_idx], ignore.case = TRUE))]
  nbhds <- integer(0)
  for (i in nb_idx) {
    txt <- sub(label, "", lines[i], ignore.case = TRUE, perl = TRUE)
    j <- i + 1                       # lists can wrap onto the next line
    while (j <= length(lines) && nzchar(trimws(lines[j])) &&
           grepl("^[\\s\\d,.&]*(and)?[\\s\\d,.&]*$", lines[j], ignore.case = TRUE, perl = TRUE)) {
      txt <- paste(txt, lines[j]); j <- j + 1
    }
    v <- suppressWarnings(as.integer(regmatches(txt, gregexpr("\\b\\d{1,3}\\b", txt, perl = TRUE))[[1]]))
    if (length(v)) { nbhds <- sort(unique(v)); break }
  }
  if (!length(nbhds)) {
    warning("Condo report without a neighborhood list: ", src, call. = FALSE)
    return(NULL)
  }

  spec <- regmatches(paste(lines, collapse = " "),
                     regexec("Specialty\\s+(\\d{3})\\s*:\\s*Residential Condominium",
                             paste(lines, collapse = " "), perl = TRUE))[[1]]
  spec_no <- if (length(spec) == 2) as.integer(spec[2]) else 700L

  # Pull the two value rows and the % row that follow a header line
  grab_table <- function(hdr_pat, pct_pat, window = 12) {
    h <- grep(hdr_pat, lines, ignore.case = TRUE, perl = TRUE)
    if (!length(h)) return(NULL)
    h <- h[1]
    win <- lines[seq(h + 1, min(h + window, length(lines)))]
    vals <- regmatches(win, regexec(row3, win, perl = TRUE))
    vals <- vals[lengths(vals) == 5]
    if (length(vals) < 2) return(NULL)
    yrs <- as.integer(vapply(vals, `[`, "", 2))
    prev <- vals[[which.min(yrs)]]
    curr <- vals[[which.max(yrs)]]
    p <- regmatches(win, regexec(pct_pat, win, perl = TRUE))
    p <- p[lengths(p) == 4]
    p <- if (length(p)) .num(p[[1]][2:4]) / 100 else rep(NA_real_, 3)
    list(prev = .num(prev[3:5]), curr = .num(curr[3:5]), pct = p)
  }
  pop <- grab_table("Population\\s*.\\s*Improved Parcel Summary",
                    paste0("^\\s*Percent\\s+Change\\s+", pct, "\\s+", pct, "\\s+", pct))
  sal <- grab_table("Sales\\s*.\\s*Improved Valuation Change Summary",
                    paste0("^\\s*%\\s*Change\\s+", pct, "\\s+", pct, "\\s+", pct))
  if (is.null(pop)) {
    warning("Condo report without a Population summary table: ", src, call. = FALSE)
    return(NULL)
  }

  n_txt <- regmatches(lines, regexec("Number of improved Parcels in the Population:\\s*([\\d,]+)",
                                     lines, ignore.case = TRUE, perl = TRUE))
  n_txt <- n_txt[lengths(n_txt) == 2]
  n_par <- if (length(n_txt)) as.integer(gsub(",", "", n_txt[[1]][2])) else NA_integer_

  mk <- function(tb, basis, n) {
    tibble::tibble(
      prop_type   = "condo",
      report_kind = "condo",
      spec_area   = spec_no,
      spec_sub    = NA_integer_,
      area        = NA_integer_,
      basis       = basis,
      av_prev     = tb$prev[3],
      av_curr     = tb$curr[3],
      delta       = tb$curr[3] - tb$prev[3],
      pct_change  = tb$pct[3],
      land_prev   = tb$prev[1],
      land_curr   = tb$curr[1],
      imps_prev   = tb$prev[2],
      imps_curr   = tb$curr[2],
      pct_land    = tb$pct[1],
      pct_imps    = tb$pct[2],
      n_parcels   = n,
      nbhds       = paste(nbhds, collapse = ","),
      area_name   = .report_title(lines),
      source_file = src
    )
  }
  out <- mk(pop, "population", n_par)
  if (!is.null(sal)) out <- dplyr::bind_rows(out, mk(sal, "sales", NA_integer_))
  # Reported % rounds to 0.1pp; fall back to the recomputed rate if it is missing
  out |>
    dplyr::mutate(
      pct_change = dplyr::coalesce(pct_change, av_curr / av_prev - 1),
      pct_land   = dplyr::coalesce(pct_land, land_curr / land_prev - 1),
      pct_imps   = dplyr::coalesce(pct_imps, imps_curr / imps_prev - 1)
    )
}

# ---- Main -----------------------------------------------------------------

message("\n--- area_report_import.R (assessment year ", area_reports_year, ") ---")

if (!dir.exists(area_reports_dir)) {
  warning("Area report directory not found: ", area_reports_dir,
          "\nNo actuals imported — area_report_actuals will not be created.",
          call. = FALSE)
} else {

  pdf_files <- list.files(area_reports_dir, pattern = "\\.pdf$",
                          full.names = TRUE, ignore.case = TRUE)
  if (length(pdf_files) == 0)
    warning("No PDFs found in ", area_reports_dir, call. = FALSE)

  parsed <- purrr::map_dfr(pdf_files, function(f) {
    src   <- basename(f)
    lines <- tryCatch(.pdf_lines(f), error = function(e) {
      warning("Could not read ", src, ": ", conditionMessage(e), call. = FALSE)
      character(0)
    })
    if (length(lines) == 0) return(NULL)

    head_txt <- paste(lines[1:min(200, length(lines))], collapse = " ")
    is_res  <- grepl("Residential Revalue", head_txt, ignore.case = TRUE)
    is_com  <- grepl("Commercial Revalue",  head_txt, ignore.case = TRUE)
    # Condo reports carry "Specialty 700" and the word "specialty" in the
    # cover letter, so they must be tested before the commercial specialty check.
    is_condo <- grepl("Condominium\\s+Revalue", head_txt, ignore.case = TRUE, perl = TRUE) ||
                any(grepl("Specialty\\s*700\\s*:", lines, perl = TRUE))
    # Specialty reports also say "Commercial Revalue", so test for them first.
    is_spec <- grepl("Specialty", head_txt, ignore.case = TRUE) &&
               !grepl("Geographic Areas Report", head_txt, ignore.case = TRUE)

    route <- if (is_condo) "condo" else if (is_spec) "specialty" else if (is_res) "res"
             else if (is_com) "com" else "unknown"

    res <- if (is_condo) parse_condo_report(lines, src)
           else if (is_spec) parse_spec_report(lines, src)
           else if (is_res) parse_res_report(lines, src)
           else if (is_com) parse_com_report(lines, src)
           else {
             # Unknown template: try each parser
             out <- parse_res_report(lines, src)
             if (is.null(out)) out <- parse_com_report(lines, src)
             if (is.null(out)) out <- parse_spec_report(lines, src)
             out
           }

    if (is.null(res) || nrow(res) == 0) {
      warning("No area growth table recognised in: ", src, " (routed as ", route, ")",
              call. = FALSE)
      return(NULL)
    }

    res$district  <- .report_title(lines)
    res$report_id <- tools::file_path_sans_ext(src)
    pop <- res[res$basis == "population", ]
    what <- if (route == "condo") {
      sprintf("%s, nbhds %s, %+.1f%% total (land %+.1f%%, imps %+.1f%%), %s units",
              pop$area_name[1], pop$nbhds[1], 100 * pop$pct_change[1],
              100 * pop$pct_land[1], 100 * pop$pct_imps[1],
              format(pop$n_parcels[1], big.mark = ","))
    } else if (route == "specialty") {
      hl  <- res[res$report_kind == "specialty", ]
      rg  <- res[res$report_kind == "specialty_region", ]
      paste0("spec ", paste(unique(res$spec_area), collapse = ","),
             if (nrow(hl)) sprintf(", %+.2f%% total", 100 * hl$pct_change[1]) else "",
             if (nrow(hl) && "pct_land" %in% names(hl) && !is.na(hl$pct_land[1]))
               sprintf(" (land %+.2f%%, imps %+.2f%%)", 100 * hl$pct_land[1], 100 * hl$pct_imps[1]) else "",
             if (nrow(rg)) paste0("; regions: ", paste(sprintf("R%d %s %+.2f%% [%d nbhds]",
               rg$spec_region, rg$area_name, 100 * rg$pct_change,
               lengths(strsplit(dplyr::coalesce(rg$nbhds, ""), ","))), collapse = ", ")) else "",
             "; ", nrow(res), " rows")
    } else {
      paste0(dplyr::n_distinct(res$area), " areas")
    }
    message("  \u2705 ", src, " [", route, "]: ", what)
    res
  })

  if (nrow(parsed) > 0) {

    area_report_actuals <- parsed |>
      dplyr::mutate(
        assessment_yr  = as.integer(area_reports_year),
        pct_change_chk = delta / av_prev
      ) |>
      dplyr::select(assessment_yr, prop_type, report_kind,
                    area, spec_area, spec_sub, basis,
                    av_prev, av_curr, delta, pct_change, pct_change_chk,
                    dplyr::any_of(c("spec_region",
                                    "land_prev", "land_curr", "imps_prev", "imps_curr",
                                    "pct_land", "pct_imps", "n_parcels",
                                    "nbhds", "area_name")),
                    report_id, district, source_file) |>
      dplyr::arrange(prop_type, report_kind, area, spec_area, spec_sub,
                     dplyr::across(dplyr::any_of("spec_region")), report_id, basis)

    # ---- Validation -------------------------------------------------------
    # Reported vs recomputed % change (res reports round means to $100s,
    # so allow modest tolerance)
    bad_pct <- area_report_actuals |>
      dplyr::filter(!is.na(pct_change_chk),
                    abs(pct_change - pct_change_chk) > 0.005)
    if (nrow(bad_pct) > 0)
      warning("Reported %-change deviates >0.5pp from recomputed value for ",
              nrow(bad_pct), " row(s) — check parsing:\n",
              paste0("  area ", bad_pct$area, " (", bad_pct$source_file, ")",
                     collapse = "\n"),
              call. = FALSE)

    # Condo reports: components should add up, and no neighborhood should
    # appear in two reports
    if ("nbhds" %in% names(area_report_actuals)) {
      condo <- area_report_actuals |>
        dplyr::filter(prop_type == "condo")
      if (nrow(condo) > 0) {
        bad_sum <- condo |>
          dplyr::filter(abs(land_prev + imps_prev - av_prev) > 500 |
                        abs(land_curr + imps_curr - av_curr) > 500)
        if (nrow(bad_sum) > 0)
          warning("Condo land + imps != total for: ",
                  paste(unique(bad_sum$report_id), collapse = ", "), call. = FALSE)
        nb_all <- unlist(strsplit(condo$nbhds[condo$basis == "population"], ","))
        nb_dup <- unique(nb_all[duplicated(nb_all)])
        if (length(nb_dup) > 0)
          warning("Condo neighborhoods listed in more than one report: ",
                  paste(nb_dup, collapse = ", "), call. = FALSE)
      }
    }

    dupes <- area_report_actuals |>
      dplyr::count(prop_type, report_kind, area, spec_area, spec_sub,
                   dplyr::across(dplyr::any_of(c("spec_region", "nbhds"))), basis) |>
      dplyr::filter(n > 1)
    if (nrow(dupes) > 0)
      warning("Duplicate entries across reports (same kind/area/basis): ",
              paste0(dupes$report_kind, "-",
                     dplyr::coalesce(dupes$area, dupes$spec_area),
                     collapse = ", "),
              call. = FALSE)

    # Which specialty populations do we have rates for this cycle?
    spec_have <- sort(unique(stats::na.omit(
      area_report_actuals$spec_area[
        area_report_actuals$report_kind == "specialty"])))
    if (length(spec_have))
      message("  specialty rates parsed for areas: ",
              paste(spec_have, collapse = ", "))
    else
      message("  NOTE: no specialty reports found - specialty parcels will ",
              "follow `specialty_actuals_policy`, NOT the geographic rate")

    # ---- Persist ----------------------------------------------------------
    assign("area_report_actuals", area_report_actuals, envir = .GlobalEnv)

    rds_path <- file.path(cache_dir,
                          paste0("area_report_actuals_", area_reports_year, ".rds"))
    saveRDS(area_report_actuals, rds_path)
    message("  \U1f4be cached: ", basename(rds_path))

    csv_path <- file.path(output_dir,
                          paste0("area_report_actuals_", area_reports_year, ".csv"))
    safe_write_csv(area_report_actuals, csv_path)
    message("  \U1f4be csv: ", basename(csv_path))

    # ---- Summary ----------------------------------------------------------
    smry <- area_report_actuals |>
      dplyr::filter(basis == "population") |>
      dplyr::mutate(cell = dplyr::case_when(
        prop_type == "condo" ~ report_id,
        !is.na(area)         ~ as.character(area),
        TRUE                 ~ paste(report_kind, spec_area, spec_sub,
                                     if ("spec_region" %in% names(area_report_actuals))
                                       spec_region else NA))) |>
      dplyr::group_by(prop_type) |>
      dplyr::summarise(
        n_areas  = dplyr::n_distinct(cell),
        min_pct  = min(pct_change),
        med_pct  = median(pct_change),
        max_pct  = max(pct_change),
        .groups  = "drop"
      )
    message("  Actual ", area_reports_year - 1, "\u2192", area_reports_year,
            " AV growth (population basis):")
    for (i in seq_len(nrow(smry)))
      message(sprintf("    %-5s %2d cells | min %+.1f%% | median %+.1f%% | max %+.1f%%",
                      smry$prop_type[i], smry$n_areas[i],
                      100 * smry$min_pct[i], 100 * smry$med_pct[i],
                      100 * smry$max_pct[i]))
  }
}

message("area_report_import.R complete.")
