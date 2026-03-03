#############################################################
#                                                           #
#   PREPARE PROJECT-SPECIFIC KUHR DIAGNOSIS EXTRACTIONS    #
#                                                           #
#############################################################
#
# README / HOW TO USE
# -------------------
# Purpose:
#   This script extracts KUHR diagnosis registrations for a user-defined set
#   of ICPC-2 codes and eligible individuals, then summarizes:
#     1) Total number of registrations per (index_id, code)
#     2) Date of first observed registration per (index_id, code)
#
# Typical use cases:
#   - Diagnosed vs non-diagnosed analyses (binary case status per code).
#   - Burden/count analyses based on diagnosis frequency.
#   - Time-to-event / survival analyses that need first diagnosis date.
#
# Inputs expected in KUHR files:
#   - LOPENR_k2_         : person index ID
#   - DIAGNOSER          : diagnosis code(s), potentially comma-separated
#   - DIAGNOSE_KODEVERK  : code system (must be "ICPC-2")
#   - FODT               : birth year or date-like birth field
#   - DATO               : registration date
#
# Output:
#   - CSV file in 01_data/ (default) with columns:
#       index_id, code, n_registrations, first_registration_date
#   - The output only includes observed (index_id, code) pairs.
#   - If an ID is in `valid_ids` but absent from the output for a code,
#     that indicates non-diagnosed / no observed registration for that code.
#
# IMPORTANT:
#   - You MUST edit the USER CONFIGURATION section before running.
#   - In particular, set `original_data_dir` and `valid_ids`.
#
# KNOWN ASSUMPTIONS / POTENTIAL MAINTENANCE RISKS:
#   - File discovery still assumes legacy and recent files can be identified
#     using current suffix patterns ("6208.dsv" and "3967.dsv"). If KUHR
#     naming changes, update the file inventory section.
#   - Legacy year mapping assumes the sorted legacy files align with years
#     2006:2019 in order.
#   - Ages are approximated as (event_year - birth_year), not exact birthday
#     age on event date.
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

#############################################################
#                    USER CONFIGURATION                     #
#############################################################

# Path to the folder that contains KUHR source data.
#
# This should point to the folder named "original_data".
# Examples:
original_data_dir <- "../../../original_data"

# Output directory and file name for the extracted summary.
# (Output is saved to file.path(output_dir, output_file_name), or output_file_name if output_dir == NULL)
output_dir <- NULL
output_file_name <- "asthma_allergy.csv"

# Diagnostic codes of interest (ICPC-2; e.g. 3-char or special codes).
target_codes <- c("R96", "R97")

# Eligible IDs for this project.
# Supports both numeric IDs (e.g., 1234567) and KUHR-prefixed IDs
# (e.g., "k2_1234567").
valid_years <- readRDS("valid_years.rds")
valid_ids <- unique(valid_years$w19_1011_lnr_k2_)

# Optional age restrictions (set to NULL for no bound).
min_age <- 7
max_age <- 9

# Optional observation year restrictions (allowed span: 2006-2024).
min_observation_year <- 2006L
max_observation_year <- 2024L

#############################################################
#                       VALIDATION                          #
#############################################################

if (!is.character(original_data_dir) || length(original_data_dir) != 1L || !nzchar(original_data_dir)) {
  stop("`original_data_dir` must be a non-empty character scalar.")
}
if (!dir.exists(path.expand(original_data_dir))) {
  stop("`original_data_dir` does not exist: ", original_data_dir)
}

if (!is.null(output_dir) && (!is.character(output_dir) || length(output_dir) != 1L || !nzchar(output_dir))) {
  stop("`output_dir` must be a non-empty character scalar.")
}
if (!is.null(output_dir) && !dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!is.character(output_file_name) || length(output_file_name) != 1L || !nzchar(output_file_name)) {
  stop("`output_file_name` must be a non-empty character scalar.")
}
if (!is.character(target_codes) || length(target_codes) == 0L) {
  stop("`target_codes` must be a non-empty character vector.")
}
if (length(valid_ids) == 0L) {
  stop("`valid_ids` is empty. Provide eligible index IDs for the project.")
}

valid_ids <- unique(normalize_id(valid_ids))
valid_ids <- valid_ids[!is.na(valid_ids)]
if (length(valid_ids) == 0L) {
  stop("No valid numeric IDs found in `valid_ids` after parsing. Use values like 1234567 or 'k2_1234567'.")
}
target_codes <- unique(trimws(target_codes))
original_data_dir <- normalizePath(path.expand(original_data_dir), winslash = "/", mustWork = TRUE)

if (!is.null(min_age)) min_age <- as.integer(min_age)
if (!is.null(max_age)) max_age <- as.integer(max_age)
if (!is.null(min_observation_year)) min_observation_year <- as.integer(min_observation_year)
if (!is.null(max_observation_year)) max_observation_year <- as.integer(max_observation_year)

if (!is.null(min_age) && !is.null(max_age) && min_age > max_age) {
  stop("`min_age` cannot be greater than `max_age`.")
}
if (min_observation_year < 2006L || max_observation_year > 2024L || min_observation_year > max_observation_year) {
  stop("Observation years must satisfy 2006 <= min_observation_year <= max_observation_year <= 2024.")
}

#############################################################
#                     FILE INVENTORY                        #
#############################################################

legacy_files <- sort(list.files(original_data_dir, pattern = "6208\\.dsv$", full.names = TRUE))
if (length(legacy_files) < 14L) {
  stop("Expected at least 14 legacy KUHR files in original_data_dir.")
}

legacy_dt <- data.table(
  year = 2006:2019,
  file_path = legacy_files[1:14]
)

recent_dir <- file.path(original_data_dir, "KUHR2020-2024")
if (!dir.exists(recent_dir)) {
  stop("Missing expected recent KUHR directory: ", recent_dir)
}

recent_files <- sort(list.files(recent_dir, pattern = "3967\\.dsv$", full.names = TRUE))
if (length(recent_files) != 5L) {
  stop("Expected exactly 5 recent KUHR files in ", recent_dir)
}
recent_dt <- data.table(
  year = 2020:2024,
  file_path = recent_files
)

file_dt <- rbindlist(list(legacy_dt, recent_dt))[year >= min_observation_year & year <= max_observation_year]
if (nrow(file_dt) == 0L) {
  stop("No KUHR source files selected after applying year limits.")
}

#############################################################
#                    CORE PROCESSING                        #
#############################################################

# Harmonize diagnosis code formatting (i.e., remove whitespace and shorten codes to the same level)
normalize_code <- function(code_vec) {
  code_vec <- trimws(code_vec)
  code_vec[nchar(code_vec) == 7L] <- substr(code_vec[nchar(code_vec) == 7L], 1L, 3L)
  code_vec[nchar(code_vec) == 8L] <- substr(code_vec[nchar(code_vec) == 8L], 1L, 4L)
  code_vec
}

target_codes <- normalize_code(target_codes)

diag_counts_all <- vector("list", nrow(file_dt))

for (idx in seq_len(nrow(file_dt))) {
  year_i <- file_dt$year[idx]
  path_i <- file_dt$file_path[idx]
  message("Processing year ", year_i, " from: ", path_i)
  
  dt_i <- fread(
    path_i,
    select = c("LOPENR_k2_", "DIAGNOSER", "DIAGNOSE_KODEVERK", "FODT", "DATO"),
    showProgress = TRUE
  )
  setnames(dt_i, c("LOPENR_k2_", "FODT", "DATO"), c("index_id", "birth_year", "event_date"))
  
    
  # Parse date and derive year fields for filtering and age restriction.
  dt_i <- dt_i[!is.na(index_id) & !is.na(event_date) & !is.na(birth_year)]
  
  if (!is.null(min_age)) dt_i <- dt_i[year_i - birth_year >= min_age]
  if (!is.null(max_age)) dt_i <- dt_i[year_i - birth_year <= max_age]
  
  if (nrow(dt_i) == 0L) {
    next
  }
  
  
  # Keep only ICPC-2 diagnoses and eligible IDs.
  dt_i[, index_id := normalize_id(index_id)]
  
  dt_i <- dt_i[
    DIAGNOSE_KODEVERK == "ICPC-2" & index_id %in% valid_ids,
    .(index_id, DIAGNOSER, birth_year, event_date)
  ]
  
  if (nrow(dt_i) == 0L) {
    next
  }
  
  
  # Split rows containing multiple diagnosis codes while preserving event date.
  dt_codes <- dt_i[
    , .(code = unlist(strsplit(DIAGNOSER, ",\\s*"))),
    by = .(index_id, event_date)
  ]
  
  dt_codes[, code := normalize_code(code)]
  dt_codes <- dt_codes[code %in% target_codes]
  
  if (nrow(dt_codes) == 0L) {
    next
  }
  
  diag_counts_all[[idx]] <- dt_codes[
    , .(
      n_registrations = .N,
      first_registration_date = min(event_date)
    ),
    by = .(index_id, code)
  ]
  
  rm(dt_i, dt_codes)
  gc(verbose = FALSE)
}

result <- rbindlist(diag_counts_all, use.names = TRUE, fill = TRUE)
if (nrow(result) == 0L) {
  result <- data.table(
    index_id = character(0),
    code = character(0),
    n_registrations = integer(0),
    first_registration_date = as.IDate(character(0))
  )
} else {
  result <- result[
    , .(
      n_registrations = sum(n_registrations),
      first_registration_date = min(first_registration_date)
    ),
    by = .(index_id, code)
  ]
  setorder(result, index_id, code)
  result[,index_id := revert_id(index_id)]
}



output_path <- ifelse(is.null(output_dir),output_file_name, file.path(output_dir, output_file_name))
fwrite(result, output_path)
message("Saved ", nrow(result), " rows to ", output_path)
