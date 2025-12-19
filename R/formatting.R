#' Formatting helpers for tables and figures
#'
#' Small helper functions for formatting numeric output in tables and figures.
#'
#' @name formatting_helpers
NULL

#' Format numbers with commas and suppressed leading zero
#'
#' Formats numeric values by adding thousand separators, forcing decimals
#' when needed, and removing leading zeros for values between -1 and 1.
#'
#' @param value Numeric vector.
#' @param nsmall Number of decimals for non-integers.
#'
#' @return A character vector.
#' @export
numformat <- function(value, nsmall = 2) {

  vapply(value, function(x) {

    if (is.na(x)) {
      return(NA_character_)
    }

    if (x == 0) {
      return("0")
    }

    # Check for integer-valued numbers robustly
    is_integer <- isTRUE(all.equal(x, round(x)))

    if (is_integer) {
      formatted_value <- format(round(x), nsmall = 0, big.mark = ",")
    } else {
      formatted_value <- format(round(x, nsmall), nsmall = nsmall, big.mark = ",")
    }

    # Remove leading zero for numbers between -1 and 1
    sub("^(-?)0", "\\1", formatted_value)

  }, FUN.VALUE = character(1))
}

#' Convert proportions to percent strings
#'
#' @param x Numeric vector of proportions.
#' @param digits Number of decimal places.
#'
#' @return A character vector.
#' @export
to_percent <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

#' Format numbers with fixed decimals
#'
#' Forces a fixed number of decimal places, including trailing zeros.
#'
#' @param x Numeric vector.
#' @param digits Number of decimal places.
#'
#' @return A character vector.
#' @export
format_num <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}

#' Format integers with thousand separators
#'
#' @param x Integer-like numeric vector.
#'
#' @return A character vector.
#' @export
format_int <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

#' Scientific notation as exponent (LaTeX)
#'
#' Formats numbers using base-10 scientific notation for use in
#' markdown or LaTeX tables.
#'
#' @param x Numeric vector.
#' @param digits Digits for the mantissa.
#'
#' @return A character vector.
#' @details
#' Returns strings like \eqn{1.23 \times 10^{-4}}. Zero returns \code{"0"}.
#' @export
expSup <- function(x, digits = 3) {

  vapply(x, function(xi) {

    if (is.na(xi)) return(NA_character_)
    if (xi == 0) return("0")

    y <- floor(log10(abs(xi)))
    mantissa <- xi / (10^y)

    sprintf(
      paste0("%.", digits, "f \\\\times 10^{%d}"),
      mantissa, y
    )

  }, FUN.VALUE = character(1))
}
