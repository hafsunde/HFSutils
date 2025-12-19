#' Add noise to achieve unit variance
#'
#' Adds Gaussian noise to a numeric vector so that the resulting
#' sample variance equals 1, if the original variance is below 1.
#'
#' @param x Numeric vector.
#'
#' @return A numeric vector with variance approximately equal to 1.
#'
#' @details
#' If the sample variance of \code{x} is greater than or equal to 1,
#' the input is returned unchanged and a warning is issued.
#'
#' Random noise is drawn from a normal distribution. Results are
#' stochastic unless a random seed is set externally.
#'
#' @export
noise_filler <- function(x) {

  stopifnot(is.numeric(x))

  v <- stats::var(x, na.rm = TRUE)

  if (is.na(v)) {
    warning("Variance is NA; returning input unchanged.")
    return(x)
  }

  if (v >= 1) {
    warning("Variance is already >= 1; no noise added.")
    return(x)
  }

  noise_sd <- sqrt(1 - v)

  noise <- stats::rnorm(
    n = length(x),
    mean = 0,
    sd = noise_sd
  )

  noise[is.na(x)] <- NA_real_

  x + noise
}
