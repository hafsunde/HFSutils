#############################################################
#                                                           #
#    PREPARE PROJECT-SPECIFIC NPR ICD-10 EXTRACTIONS       #
#                                                           #
#############################################################
#
# README / HOW TO USE
# -------------------
# Purpose:
#   This script extracts specialist-care ICD-10 registrations from NPR
#   source files (multiple registry streams), and summarizes:
#     1) Total number of registrations per (index_id, code)
#     2) Date of first observed registration per (index_id, code)
#
# Optional extension:
#   - Include ICD-10 registrations from KUHR files, since some specialist
#     ICD-10 records may also be present there.
#
# Output:
#   - CSV file with columns:
#       index_id, code, n_registrations, first_registration_date
#
# IMPORTANT:
#   - Edit USER CONFIGURATION before running, especially:
#       npr_data_dir, valid_ids, target_codes
#
# ASSUMPTIONS:
#   - NPR visit files include: kobl_nokkel, aar, lopenr_k2_, innmnd
#   - NPR code files include:  kobl_nokkel, kodeverdi
#   - Most (not all) code files include kodenavn; when present, only ICD-10
#     rows are kept.
#   - If no explicit event date exists, first date is approximated as
#     YYYY-MM-01 from the month of entry into care.
#
#############################################################

library(data.table)


# Helper functions to convert input IDs to integer core IDs used throughout the script. (It is faster)
normalize_id <- function(id_vec, prefix = "k2_") {
  if (!is.character(id_vec) || !startsWith(id_vec[1], prefix)) {message("Unknown id-structure detected, returned unchanged"); return(id_vec)}
  suppressWarnings(as.integer(substring(trimws(id_vec), nchar(prefix) + 1L)))
}

revert_id <- function(id_vec, prefix = "k2_", width = 7L) {
  if (is.character(id_vec) && startsWith(id_vec[1], prefix)) {message("Original id-structure detected, returned unchanged"); return(id_vec)}
  id_int <- suppressWarnings(as.integer(id_vec))   # Coerce to integer (in case numeric or character was supplied)
  if (any(is.na(id_int) & !is.na(id_vec))) { stop("Some ids could not be coerced to integer, something is wrong.") }
  paste0(prefix, sprintf(paste0("%0", width, "d"), id_int)) # Pad to 7 digits and prepend "k2_"
}

# x is a character vector like "YYYY-MM"
ym_to_IDate_fast <- function(x) {
  x <- trimws(x)
  y <- as.integer(substr(x, 1L, 4L))
  m <- as.integer(substr(x, 6L, 7L))
  as.IDate(ISOdate(y, m, 1L))
}

#############################################################
#                    USER CONFIGURATION                     #
#############################################################

# Directory containing NPR csv files (e.g., k2_-somhoved.csv, somkoder.csv, ...).
npr_data_dir <- "../../../New_MFR_NPR"

# Optional KUHR root directory (used only if include_kuhr_icd10 = TRUE).
kuhr_data_dir <- "../../../original_data"
include_kuhr_icd10 <- TRUE

# Output location.
output_dir <- NULL
output_file_name <- "asthma_allergy_NPR.csv"

# ICD-10 codes of interest. If code_level = 3, values are compared using first
# 3 chars (e.g., "F32", "S06"). If code_level = NULL, full code strings are used.
target_codes <- c("J45", "J30")
code_level <- 3L

# Eligible IDs. Supports both numeric IDs and "k2_"-prefixed IDs.
valid_years <- readRDS("valid_years.rds")
valid_ids <- unique(valid_years$w19_1011_lnr_k2_)

# Optional age restrictions (age approximated as event_year - birth_year).
min_age <- 7
max_age <- 9

# Optional birth-year lookup file for age filtering. Will be ignored if age restrictions are not set
# Must contain one row per person and include ID + birth year columns.
# Supported ID columns: index_id, lopenr_k2_, w19_1011_lnr_k2_
# Supported birth-year columns: foedselsaar, birth_year, FODT
birth_year_lookup_file <- "../../../NewSSB/k2_-w19_1011_xfaste_oppl_nodate.csv"

# Observation-year restrictions.
min_observation_year <- 2008L
max_observation_year <- 2024L

# NPR registry streams to include. Change here if you only want to use some sub-registers
#   name: label for logs
#   visit_file: visit-level file
#   code_file: code-level file
#   code_system_column/value: used only if that column exists in code file
npr_sources <- list(
  list(name = "avtsom", visit_file = "k2_-avtsomhoved.csv", code_file = "avtsomkoder.csv", code_system_column = "kodenavn", code_system_value = "ICD-10"),
  list(name = "avtphv", visit_file = "k2_-avtphvhoved.csv", code_file = "avtphvkoder.csv", code_system_column = "kodenavn", code_system_value = "ICD-10"),
  list(name = "som",    visit_file = "k2_-somhoved.csv",    code_file = "somkoder.csv",    code_system_column = "kodenavn", code_system_value = "ICD-10"),
  list(name = "phbu",   visit_file = "k2_-phbuhoved.csv",   code_file = "phbukoder.csv",   code_system_column = "kodenavn", code_system_value = "ICD-10"),
  list(name = "phv",    visit_file = "k2_-phvhoved.csv",    code_file = "phvkoder.csv",    code_system_column = "kodenavn", code_system_value = "ICD-10"),
  list(name = "reh",    visit_file = "k2_-rehhoved.csv",    code_file = "rehkoder.csv",    code_system_column = "kodenavn", code_system_value = "ICD-10"),
  list(name = "tsb",    visit_file = "k2_-tsbhoved.csv",    code_file = "tsbkoder.csv",    code_system_column = "kodenavn", code_system_value = "ICD-10")
)
#  avtsom  =  Avtalespesialist, somatisk
#  avtphv  =  Avtalespesialist, psykisk helsevern voksen
#  som     =  Somatisk
#  phbu    =  psykisk helsevern barn og unge
#  phv     =  psykisk helsevern voksen
#  reh     =  Rehabilitering
#  tsb     =  Rusbehandling
#############################################################
#                       VALIDATION                          #
#############################################################

if (!dir.exists(path.expand(npr_data_dir))) {
  stop("`npr_data_dir` does not exist: ", npr_data_dir)
}
if (include_kuhr_icd10 && !dir.exists(path.expand(kuhr_data_dir))) {
  stop("`kuhr_data_dir` does not exist: ", kuhr_data_dir)
}
if (!is.null(output_dir) && !dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
if (!is.character(target_codes) || length(target_codes) == 0L) {
  stop("`target_codes` must be a non-empty character vector.")
}
if (length(valid_ids) == 0L) {
  stop("`valid_ids` is empty. Provide eligible index IDs for the project.")
}
if (!is.null(min_age)) min_age <- as.integer(min_age)
if (!is.null(max_age)) max_age <- as.integer(max_age)
if (!is.null(min_age) && !is.null(max_age) && min_age > max_age) {
  stop("`min_age` cannot be greater than `max_age`.")
}
if ((!is.null(min_age) || !is.null(max_age)) && is.null(birth_year_lookup_file)) {
  stop("Age filtering requested, but `birth_year_lookup_file` is NULL.")
}
if (min_observation_year > max_observation_year) {
  stop("`min_observation_year` cannot be greater than `max_observation_year`.")
}
if (!is.null(code_level) && (!is.numeric(code_level) || code_level < 1L)) {
  stop("`code_level` must be NULL or a positive integer.")
}


normalize_code <- function(code_vec, code_level = NULL) {
  code_vec <- toupper(trimws(as.character(code_vec)))
  if (!is.null(code_level)) {
    code_vec <- ifelse(nchar(code_vec) >= code_level, substr(code_vec, 1L, code_level), code_vec)
  }
  code_vec
}

valid_ids <- unique(normalize_id(valid_ids))
valid_ids <- valid_ids[!is.na(valid_ids)]
if (length(valid_ids) == 0L) {
  stop("No valid numeric IDs found in `valid_ids` after parsing.")
}

target_codes <- unique(normalize_code(target_codes, code_level = code_level))
npr_data_dir <- normalizePath(path.expand(npr_data_dir), winslash = "/", mustWork = TRUE)
if (include_kuhr_icd10) {
  kuhr_data_dir <- normalizePath(path.expand(kuhr_data_dir), winslash = "/", mustWork = TRUE)
}

birth_year_lookup <- NULL
if (!is.null(birth_year_lookup_file) & (!is.null(min_age) | !is.null(max_age))) {
  if (!file.exists(birth_year_lookup_file)) {
    stop("`birth_year_lookup_file` does not exist: ", birth_year_lookup_file)
  }
  birth_year_lookup <- if (grepl("\\.rds$", birth_year_lookup_file, ignore.case = TRUE)) {
    as.data.table(readRDS(birth_year_lookup_file))
  } else {
    fread(birth_year_lookup_file, showProgress = TRUE)
  }
  
  id_col <- intersect(c("index_id", "lopenr_k2_", "w19_1011_lnr_k2_"), names(birth_year_lookup))[1]
  by_col <- intersect(c("foedselsaar","birth_year", "FODT"), names(birth_year_lookup))[1]
  if (is.na(id_col) || is.na(by_col)) {
    stop("`birth_year_lookup_file` must include an ID column (index_id/lopenr_k2_/w19_1011_lnr_k2_) and a birth-year column (birth_year/FODT).")
  }
  
  birth_year_lookup <- birth_year_lookup[
    , .(
      index_id = normalize_id(get(id_col)),
      birth_year = as.integer(substr(as.character(get(by_col)), 1L, 4L))
    )
  ]
  birth_year_lookup <- unique(birth_year_lookup[!is.na(index_id) & !is.na(birth_year)])
  birth_year_lookup <- birth_year_lookup[index_id %in% valid_ids]
}

#############################################################
#                    NPR PROCESSING                         #
#############################################################

collect_npr_source <- function(source_cfg) {
  visit_path <- file.path(npr_data_dir, source_cfg$visit_file)
  code_path <- file.path(npr_data_dir, source_cfg$code_file)
  
  if (!file.exists(visit_path)) {
    stop("Missing visit file for source '", source_cfg$name, "': ", visit_path)
  }
  if (!file.exists(code_path)) {
    stop("Missing code file for source '", source_cfg$name, "': ", code_path)
  }
  
  message("Processing NPR source: ", source_cfg$name)
  

# Import code data --------------------------------------------------------
  codes <- fread(code_path, na.strings = "", showProgress = TRUE)
  required_cols <- c("kobl_nokkel", "kodeverdi")
  if (!all(required_cols %in% names(codes))) {
    stop("Code file missing required columns ", paste(required_cols, collapse = ", "), ": ", code_path)
  }
  
  if (!is.null(source_cfg$code_system_column) && source_cfg$code_system_column %in% names(codes)) {
    codes <- codes[get(source_cfg$code_system_column) == source_cfg$code_system_value]
  }
  
  codes <- codes[!is.na(kodeverdi) & kodeverdi != "" & kodeverdi != "ZZZ", .(kobl_nokkel, kodeverdi)]
  if (nrow(codes) == 0L) {
    return(data.table(index_id = integer(0), code = character(0), event_date = as.IDate(character(0))))
  }
  codes[,kodeverdi := normalize_code(kodeverdi, code_level = code_level)]
  codes <- codes[kodeverdi %in% target_codes]
  
  if (nrow(codes) == 0L) {
    return(data.table(index_id = integer(0), code = character(0), event_date = as.IDate(character(0))))
  }
  
  codes[,kobl_nokkel := normalize_id(kobl_nokkel,paste0(toupper(source_cfg$name),"_"))]

# Import visit data -------------------------------------------------------
  visits <- fread(
    visit_path,
    select = c("kobl_nokkel", "aar", "innmnd", "lopenr_k2_"),
    na.strings = "",
    showProgress = TRUE
  )
  setnames(visits, c("lopenr_k2_", "aar", "innmnd"), c("index_id", "event_year", "event_date"))
  
  visits[, index_id := normalize_id(index_id)]
  
  visits <- visits[
    !is.na(index_id) &
      index_id %in% valid_ids &
      !is.na(event_year) &
      event_year >= min_observation_year &
      event_year <= max_observation_year,
    .(kobl_nokkel, index_id, event_year, event_date)
  ]
  
  if (nrow(visits) == 0L) {
    return(data.table(index_id = integer(0), code = character(0), event_date = as.IDate(character(0))))
  }
  
  # Remove visits not related to target codes
  visits[,kobl_nokkel := normalize_id(kobl_nokkel,paste0(toupper(source_cfg$name),"_"))]
  visits <- visits[kobl_nokkel %in% codes$kobl_nokkel]
  
  # Process dates
  visits[, event_date := ym_to_IDate_fast(event_date)]
  
  if (!is.null(birth_year_lookup) & (!is.null(min_age) | !is.null(max_age))) {
    visits <- birth_year_lookup[visits, on = "index_id", nomatch = 0L]
    visits <- visits[!is.na(birth_year)]
    if (!is.null(min_age)) visits <- visits[event_year - birth_year >= min_age]
    if (!is.null(max_age)) visits <- visits[event_year - birth_year <= max_age]
  }
  
  if (nrow(visits) == 0L) {
    return(data.table(index_id = integer(0), code = character(0), event_date = as.IDate(character(0))))
  }

# Combine visits and codes ------------------------------------------------
  npr_events <- codes[visits, on = "kobl_nokkel", nomatch = 0L][
    , .(index_id, code = kodeverdi, event_date)
  ]
  
  npr_events
}

npr_events_all <- rbindlist(lapply(npr_sources, collect_npr_source), use.names = TRUE, fill = TRUE)

#############################################################
#              OPTIONAL KUHR ICD-10 PROCESSING             #
#############################################################

collect_kuhr_icd10 <- function() {
  file_sets <- list(
    list(dir = kuhr_data_dir, years = 2006:2019, pattern = "6208\\.dsv$"),
    list(dir = file.path(kuhr_data_dir, "KUHR2020-2024"), years = 2020:2024, pattern = "3967\\.dsv$")
  )
  
  out <- list()
  out_i <- 0L
  
  for (cfg in file_sets) {
    if (!dir.exists(cfg$dir)) next
    files <- sort(list.files(cfg$dir, pattern = cfg$pattern, full.names = TRUE))
    if (length(files) == 0L) next
    
    n_take <- min(length(files), length(cfg$years))
    dt_files <- data.table(year = cfg$years[seq_len(n_take)], file = files[seq_len(n_take)])
    dt_files <- dt_files[year >= min_observation_year & year <= max_observation_year]
    
    for (j in seq_len(nrow(dt_files))) {
      message("Processing KUHR ICD-10 year ", dt_files$year[j])
      x <- fread(
        dt_files$file[j],
        select = c("LOPENR_k2_", "DIAGNOSER", "DIAGNOSE_KODEVERK","FODT", "DATO"),
        showProgress = TRUE
      )
      x <- x[DIAGNOSE_KODEVERK == "ICD-10"]
      setnames(x, c("LOPENR_k2_", "FODT", "DATO"), c("index_id", "birth_year", "event_date"))
      if (!is.null(min_age)) x <- x[dt_files[j,year] - birth_year >= min_age]
      if (!is.null(max_age)) x <- x[dt_files[j,year] - birth_year <= max_age]
      x[, index_id := normalize_id(index_id)]
      x <- x[index_id %in% valid_ids, .(index_id, DIAGNOSER, event_date)]
      if (nrow(x) == 0L) next
      
      y <- x[, .(code = unlist(strsplit(DIAGNOSER, ",\\s*"))), by = .(index_id, event_date)]
      y[, code := normalize_code(code, code_level = code_level)]
      y <- y[code %in% target_codes]
      if (nrow(y) == 0L) next
      
      out_i <- out_i + 1L
      out[[out_i]] <- y[, .(index_id, code, event_date)]
    }
  }
  
  if (length(out) == 0L) {
    return(data.table(index_id = integer(0), code = character(0), event_date = as.IDate(character(0))))
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

kuhr_events_all <- if (include_kuhr_icd10) collect_kuhr_icd10() else {
  data.table(index_id = integer(0), code = character(0), event_date = as.IDate(character(0)))
}

#############################################################
#                       SUMMARIZE                           #
#############################################################

all_events <- rbindlist(list(npr_events_all, kuhr_events_all), use.names = TRUE, fill = TRUE)

all_events <- if (nrow(all_events) == 0L) {
  all_events <- data.table(
    index_id = character(0),
    code = character(0),
    n_registrations = integer(0),
    first_registration_date = as.IDate(character(0))
  )
} else {
  all_events <- all_events[
    , .(
      n_registrations = .N,
      first_registration_date = min(event_date, na.rm = TRUE)
    ),
    by = .(index_id, code)
  ][order(index_id, code)]
  all_events[,index_id := revert_id(index_id)]
}

output_path <- ifelse(is.null(output_dir),output_file_name, file.path(output_dir, output_file_name))
fwrite(result, output_path)
message("Saved ", nrow(result), " rows to ", output_path)
