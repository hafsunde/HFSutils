#' Simulate clustered outcomes with a target ICC
#'
#' Simulate clustered data under a random-intercept model where the total latent
#' variance is fixed to 1 and split into between-cluster variance (`ICC`) and
#' within-cluster variance (`1 - ICC`). Optionally dichotomize the latent outcome
#' to a binary variable using a threshold chosen to match a target prevalence.
#'
#' @param num_groups Number of clusters/groups to simulate.
#' @param num_individuals Number of individuals per cluster.
#' @param ICC Target intraclass correlation coefficient on the latent scale.
#'   Must be between 0 and 1.
#' @param prevalence Optional target prevalence for a binary outcome.
#'   If `NULL` (default), returns a continuous latent outcome.
#'
#' @return A `data.frame` with columns:
#' \describe{
#'   \item{Group}{Cluster identifier as a factor.}
#'   \item{Individual}{Row-level individual identifier.}
#'   \item{Y}{Simulated outcome (continuous or binary).}
#' }
#'
#' @examples
#' d_cont <- sim_icc(num_groups = 100, num_individuals = 3, ICC = 0.30)
#' d_bin <- sim_icc(num_groups = 100, num_individuals = 3, ICC = 0.30, prevalence = 0.10)
#'
#' @export
sim_icc <- function(num_groups, num_individuals, ICC, prevalence = NULL) {
  if (ICC < 0 || ICC > 1) {
    stop("`ICC` must be between 0 and 1.")
  }

  sigma_between <- sqrt(ICC)
  sigma_within <- sqrt(1 - ICC)

  group_effects <- stats::rnorm(num_groups, mean = 0, sd = sigma_between)

  group_ids <- rep(seq_len(num_groups), each = num_individuals)
  individual_effects <- stats::rnorm(num_groups * num_individuals, mean = 0, sd = sigma_within)

  y <- group_effects[group_ids] + individual_effects

  if (!is.null(prevalence)) {
    if (prevalence <= 0 || prevalence >= 1) {
      stop("`prevalence` must be between 0 and 1 (exclusive).")
    }

    threshold <- stats::qnorm(1 - prevalence, mean = mean(y), sd = stats::sd(y))
    y <- as.integer(y >= threshold)
  }

  data.frame(
    Group = factor(group_ids),
    Individual = seq_len(num_groups * num_individuals),
    Y = y
  )
}
