#' Build a square matrix with distinct diagonal/off-diagonal values
#'
#' Utility for constructing square matrices used in OpenMx model specifications,
#' where diagonal and off-diagonal entries need different values (or labels).
#'
#' @param size Matrix dimension.
#' @param diag_value Value (or label) placed on the diagonal.
#' @param offdiag_value Value (or label) placed off the diagonal.
#'
#' @return A `size` x `size` matrix.
#'
#' @examples
#' build_constraint_matrix(3, diag_value = "V", offdiag_value = "r")
#'
#' @export
build_constraint_matrix <- function(size, diag_value, offdiag_value) {
  mat <- matrix(offdiag_value, nrow = size, ncol = size)
  diag(mat) <- diag_value
  mat
}

#' Estimate ICC with an exchangeable OpenMx model
#'
#' Fits an exchangeable covariance model in OpenMx after reshaping long data to
#' wide format (one row per group). For dichotomous outcomes, fits a liability
#' threshold model.
#'
#' @param dt Input data frame/data.table.
#' @param group_id Column name (character scalar) identifying clusters.
#' @param phenotype Column name (character scalar) for the outcome.
#' @param group.max Maximum number of individuals per group retained for model
#'   fitting.
#' @param is_dichotmous Logical; whether to fit a dichotomous (liability
#'   threshold) model.
#' @param factor.levels Ordered outcome levels for dichotomous models.
#'
#' @return A fitted `MxModel` object with an `ICC` algebra.
#'
#' @export
mx.ICC <- function(dt, group_id, phenotype, group.max = 200, is_dichotmous = FALSE, factor.levels = c(0, 1)) {
  if (!requireNamespace("OpenMx", quietly = TRUE)) {
    stop("Package 'OpenMx' is required for mx.ICC().")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for mx.ICC().")
  }

  data.table::setDT(dt)
  data.table::set(dt, j = setdiff(names(dt), c(group_id, phenotype)), value = NULL)
  data.table::setnames(dt, c(group_id, phenotype), c("group_id", "phenotype"))

  dt <- dt[sample(seq_len(nrow(dt))), ]
  dt <- dt[, .SD[1:group.max], by = group_id]

  dt[, within_group_id := paste0("id_", seq_len(.N)), by = group_id]

  wide_dt <- data.table::dcast(dt, group_id ~ within_group_id, value.var = "phenotype")
  wide_dt[, group_id := NULL]

  mat_size <- ncol(wide_dt)

  if (is_dichotmous) {
    message("Dichotomous data set. Running liability threshold model with levels: ", factor.levels[1], " and ", factor.levels[2])
    wide_dt <- OpenMx::mxFactor(as.data.frame(wide_dt), levels = factor.levels)

    mdl <- OpenMx::mxModel(
      name = phenotype,
      OpenMx::mxMatrix(type = "Symm", nrow = mat_size, ncol = mat_size,
                       free = build_constraint_matrix(mat_size, FALSE, TRUE),
                       values = build_constraint_matrix(mat_size, 1, .2),
                       labels = build_constraint_matrix(mat_size, "V", "r"),
                       name = "ExpCov"),
      OpenMx::mxMatrix(type = "Full", nrow = 1, ncol = mat_size, free = FALSE, values = 0, labels = "mean", name = "ExpMean"),
      OpenMx::mxMatrix(type = "Full", nrow = 1, ncol = mat_size, free = TRUE, values = 1, labels = "threshold", name = "ExpThre"),
      OpenMx::mxAlgebra(expression = r / V, name = "ICC"),
      OpenMx::mxExpectationNormal(covariance = "ExpCov", means = "ExpMean", thresholds = "ExpThre", dimnames = colnames(wide_dt), threshnames = colnames(wide_dt)),
      OpenMx::mxFitFunctionML(),
      OpenMx::mxCI("ICC"),
      OpenMx::mxData(observed = wide_dt, type = "raw")
    )
  } else {
    message("Continuous data set")

    mdl <- OpenMx::mxModel(
      name = phenotype,
      OpenMx::mxMatrix(type = "Symm", nrow = mat_size, ncol = mat_size, free = TRUE,
                       values = build_constraint_matrix(mat_size, 1, .2),
                       labels = build_constraint_matrix(mat_size, "V", "r"),
                       name = "ExpCov"),
      OpenMx::mxMatrix(type = "Full", nrow = 1, ncol = mat_size, free = TRUE, values = 0, labels = "mean", name = "ExpMean"),
      OpenMx::mxAlgebra(expression = r / V, name = "ICC"),
      OpenMx::mxExpectationNormal(covariance = "ExpCov", means = "ExpMean", dimnames = colnames(wide_dt)),
      OpenMx::mxFitFunctionML(),
      OpenMx::mxCI("ICC"),
      OpenMx::mxData(observed = as.data.frame(wide_dt), type = "raw")
    )
  }

  OpenMx::mxRun(mdl, intervals = FALSE)
}

#' Extract ICC estimate and confidence interval from an OpenMx fit
#'
#' Convenience extractor for fitted models returned by [mx.ICC()]. If profile
#' confidence intervals are present in the model output they are used; otherwise
#' Wald intervals are computed from the standard error via `SEtoCI()`.
#'
#' @param fit A fitted `MxModel` object.
#' @param group.name Optional label for the grouping variable included in output.
#'
#' @return A one-row `data.frame` with group label, phenotype name, and ICC
#' estimate plus confidence limits.
#'
#' @export
extract.ICC <- function(fit, group.name = NA) {
  if (!inherits(fit, "MxModel")) {
    stop("Object is not an MxModel")
  }

  if (!is.null(fit$output$confidenceIntervals)) {
    results <- fit$output$confidenceIntervals[, c(2, 1, 3)]
  } else {
    icc_se <- OpenMx::mxSE(ICC, fit, silent = TRUE)
    results <- unlist(c(estimate = fit$ICC$result, SEtoCI(fit$ICC$result, icc_se)))
  }

  data.frame(group.name = group.name, phenotype = fit$name, t(as.data.frame(results)))
}
