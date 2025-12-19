#' Quick descriptive stats
#'
#' @param x Numeric vector.
#' @return A named list with n, mean, sd, min, max, and NA count.
#' @export
describe <- function(x) {
  stopifnot(is.numeric(x))
  list(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = stats::sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    na = sum(is.na(x))
  )
}
