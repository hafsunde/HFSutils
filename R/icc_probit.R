#' Estimate ICC from a random-intercept probit model
#'
#' Fits a random-intercept binomial-probit mixed model and computes the
#' liability-scale ICC as
#' \deqn{ICC = \sigma^2_{between} / (\sigma^2_{between} + 1).}
#' Optional confidence intervals are derived from confidence intervals for the
#' random-effect standard deviation.
#'
#' @param data A `data.frame` containing outcome and group variables.
#' @param outcome Name of the binary outcome variable (character scalar).
#' @param group Name of the grouping variable for random intercepts
#'   (character scalar).
#' @param ci_method Method for confidence intervals. One of `"profile"`,
#'   `"Wald"`, or `"none"`.
#'
#' @return If `ci_method = "none"`, returns numeric scalar ICC estimate.
#'   Otherwise returns a named numeric vector with `ICC`, `ICC_lower`, and
#'   `ICC_upper`.
#'
#' @examples
#' \dontrun{
#' d <- sim_icc(num_groups = 1000, num_individuals = 3, ICC = 0.30, prevalence = 0.10)
#' probit_icc(d, outcome = "Y", group = "Group", ci_method = "Wald")
#' }
#'
#' @export
probit_icc <- function(data, outcome, group, ci_method = c("profile", "Wald", "none")) {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("Package 'glmmTMB' is required for probit_icc().")
  }

  ci_method <- match.arg(ci_method)

  f <- stats::as.formula(paste0(outcome, " ~ 1 + (1 | ", group, ")"))
  mod <- glmmTMB::glmmTMB(f, data = data, family = stats::binomial(link = "probit"))

  var_est <- as.numeric(glmmTMB::VarCorr(mod)$cond[[group]][1])
  icc_est <- var_est / (var_est + 1)

  if (ci_method == "none") {
    return(icc_est)
  }

  ci_theta <- stats::confint(mod, parm = "theta_", method = ci_method)[1, ]

  if (ci_method == "profile") {
    sd_lower <- exp(ci_theta[1])
    sd_upper <- exp(ci_theta[2])
  } else {
    sd_lower <- ci_theta[1]
    sd_upper <- ci_theta[2]
  }

  var_lower <- sd_lower^2
  var_upper <- sd_upper^2

  icc_lower <- var_lower / (var_lower + 1)
  icc_upper <- var_upper / (var_upper + 1)

  c(ICC = icc_est, ICC_lower = icc_lower, ICC_upper = icc_upper)
}
