#' Convert standard errors to confidence intervals
#'
#' Computes confidence intervals from point estimates and standard errors.
#' Optionally applies Fisher's z-transformation for correlations.
#'
#' @param estimates Numeric vector of point estimates.
#' @param std_errors Numeric vector of standard errors.
#' @param conf_level Confidence level for the interval. Default is 0.95.
#' @param is_correlation Logical. Set to TRUE if estimates are correlations.
#'
#' @return A data.frame with columns Lower and Upper.
#'
#' @details
#' If \code{is_correlation = TRUE}, confidence intervals are computed on the
#' Fisher z scale and then back-transformed to the correlation scale.
#'
#' @export
SEtoCI <- function(estimates, std_errors, conf_level = 0.95, is_correlation = FALSE) {
  stopifnot(
    is.numeric(estimates),
    is.numeric(std_errors),
    length(estimates) == length(std_errors),
    conf_level > 0 && conf_level < 1
  )

  z_value <- stats::qnorm((1 + conf_level) / 2)

  if (is_correlation) {
    z_estimates <- atanh(estimates)
    lower_bound_z <- z_estimates - z_value * std_errors
    upper_bound_z <- z_estimates + z_value * std_errors

    lower_bound <- tanh(lower_bound_z)
    upper_bound <- tanh(upper_bound_z)
  } else {
    lower_bound <- estimates - z_value * std_errors
    upper_bound <- estimates + z_value * std_errors
  }

  data.frame(
    Lower = lower_bound,
    Upper = upper_bound
  )
}
