#' Convert NUS2000 education level to years of education
#'
#' Converts NUS2000-coded education levels (0–8) to an approximate number of
#' years of education.
#'
#' @param edu_level Integer-like vector of NUS2000 education levels.
#'
#' @return A numeric vector of years (NA for unknown/unmapped values).
#'
#' @export
convert_edu_level_to_years <- function(edu_level) {

  # Coerce safely to integer for indexing
  x <- suppressWarnings(as.integer(edu_level))

  # Lookup table for codes 0..8
  map <- c(
    `0` = 0,
    `1` = 7,
    `2` = 10,
    `3` = 12,
    `4` = 13,
    `5` = 14,
    `6` = 17,
    `7` = 19,
    `8` = 20
  )

  out <- unname(map[as.character(x)])
  out[is.na(x)] <- NA_real_
  out
}

#' Convert NUS2000 education level to education group
#'
#' Groups NUS2000-coded education levels into broad categories.
#'
#' @param edu_level Integer-like vector of NUS2000 education levels.
#'
#' @return A character vector of education groups (NA for unknown/unmapped values).
#'
#' @export
convert_edu_level_to_group <- function(edu_level) {

  x <- suppressWarnings(as.integer(edu_level))

  out <- rep(NA_character_, length(x))

  out[x %in% c(0L, 1L, 2L, 3L)] <- "Basic Education"
  out[x %in% c(4L, 5L)]         <- "High School"
  out[x %in% c(6L)]             <- "Bachelor (or equiv)"
  out[x %in% c(7L, 8L)]         <- "Master (or equiv)"

  out[is.na(x)] <- NA_character_
  out
}
