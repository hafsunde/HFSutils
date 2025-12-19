#' Fast correlation with Fisher confidence interval
#'
#' Computes a correlation between two numeric vectors and returns a
#' Fisher z-based confidence interval, using pairwise complete observations
#' by default.
#'
#' @param vector1 Numeric vector.
#' @param vector2 Numeric vector.
#' @param confidence_level Confidence level for the interval. Default is 0.95.
#' @param missing_handling Method passed to \code{cor(use = )}.
#'
#' @return
#' A named numeric vector with elements:
#' \describe{
#'   \item{Correlation}{Estimated correlation}
#'   \item{CI_Lower}{Lower confidence bound}
#'   \item{CI_Upper}{Upper confidence bound}
#'   \item{N}{Number of complete observation pairs}
#' }
#'
#' @details
#' Confidence intervals are computed using Fisher's z-transformation.
#' If either input vector is constant zero, the correlation is defined
#' as zero and confidence bounds are returned as NA.
#'
#' @export
cor_fastCI <- function(vector1,
                       vector2,
                       confidence_level = 0.95,
                       missing_handling = "pairwise.complete.obs") {

  stopifnot(
    is.numeric(vector1),
    is.numeric(vector2),
    length(vector1) == length(vector2),
    confidence_level > 0 && confidence_level < 1
  )

  # Handle degenerate zero-variance cases explicitly
  if (all(vector1 == 0, na.rm = TRUE) || all(vector2 == 0, na.rm = TRUE)) {
    return(c(
      Correlation = 0,
      CI_Lower = NA_real_,
      CI_Upper = NA_real_,
      N = sum(!is.na(vector1) & !is.na(vector2))
    ))
  }

  # Compute correlation
  correlation <- stats::cor(vector1, vector2, use = missing_handling)

  if (is.na(correlation)) {
    return(c(
      Correlation = NA_real_,
      CI_Lower = NA_real_,
      CI_Upper = NA_real_,
      N = sum(!is.na(vector1) & !is.na(vector2))
    ))
  }

  # Number of complete pairs
  actual_pairs <- sum(!is.na(vector1) & !is.na(vector2))

  # Fisher CI requires at least 4 observations
  if (actual_pairs <= 3) {
    return(c(
      Correlation = correlation,
      CI_Lower = NA_real_,
      CI_Upper = NA_real_,
      N = actual_pairs
    ))
  }

  # Fisher transformation
  fisher_z <- atanh(correlation)
  stderr <- 1 / sqrt(actual_pairs - 3)

  z_critical <- stats::qnorm((1 + confidence_level) / 2)

  ci_lower <- tanh(fisher_z - z_critical * stderr)
  ci_upper <- tanh(fisher_z + z_critical * stderr)

  c(
    Correlation = correlation,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    N = actual_pairs
  )
}
