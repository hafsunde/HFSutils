#' Extract Wald SE and confidence intervals from an OpenMx model object
#'
#' Convenience wrapper to extract estimates and SEs from an OpenMx model slot
#' and compute Wald confidence intervals via \code{SEtoCI()}.
#'
#' @param fit An OpenMx model fit object.
#' @param object Character. Name of the slot inside \code{fit} to extract, e.g. "A", "S", etc.
#' @param is_correlation Logical. If TRUE, compute CI using Fisher z transform in \code{SEtoCI()}.
#'
#' @return A data.frame with columns type, estimate, parameter, SE, lbound, ubound.
#'
#' @details
#' This function assumes \code{OpenMx::mxSE()} works for the requested \code{object}.
#' It is a personal helper and may fail for some model structures.
#'
#' @export
mx.WaldCI <- function(fit, object, is_correlation = FALSE) {

  # Dependency note: mxSE comes from OpenMx. This will error if OpenMx is not installed/loaded.
  se_mat <- mxSE(object, fit)

  # Combine estimates + SE into long format
  results <- combine_SE(fit[[object]]$result, se_mat)
  results <- as.data.frame(results)

  # Rename columns to match your preferred naming convention
  names(results)[names(results) == "Row"] <- "type"
  names(results)[names(results) == "Estimate"] <- "estimate"
  names(results)[names(results) == "Col"] <- "parameter"

  # Preallocate CI columns
  results$lbound <- NA_real_
  results$ubound <- NA_real_

  # Compute Wald CI from SE (your SEtoCI() function in this package)
  results[, c("lbound", "ubound")] <- SEtoCI(
    estimates = results$estimate,
    std_errors = results$SE,
    is_correlation = is_correlation
  )

  results
}


combine_SE <- function(M, SE) {
  # Step 1: Reshape the initial matrix into long format
  # Get row and column names
  row_names <- rownames(M)
  col_names <- colnames(M)

  # Handle cases where rownames or colnames are NULL
  if (is.null(row_names)) {
    row_names <- as.character(1:nrow(M))
  }
  if (is.null(col_names)) {
    col_names <- as.character(1:ncol(M))
  }

  # Step 3: Map indices to actual row and column names
  # Note: This assumes SE has the same shape as M.
  df_long <- data.frame(
    Row = rep(row_names, times = length(col_names)),
    Col = rep(col_names, each = length(row_names)),
    Estimate = as.vector(M),
    SE = as.vector(SE)
  )

  return(df_long)
}


# Presuming confidence intervals are calculated, combines the result object with the CI object
combine_CI <- function(M, CI) {
  # Step 1: Reshape the initial matrix into long format
  # Get row and column names
  row_names <- rownames(M)
  col_names <- colnames(M)

  # Handle cases where rownames or colnames are NULL
  if (is.null(row_names)) {
    row_names <- as.character(1:nrow(M))
  }
  if (is.null(col_names)) {
    col_names <- as.character(1:ncol(M))
  }

  # Create a data frame with all combinations of row and column names
  df_long <- data.frame(
    Row = rep(row_names, times = length(col_names)),
    Col = rep(col_names, each = length(row_names)),
    Estimate = as.vector(M)
  )

  # Step 2: Extract indices from row names of CI
  rn <- rownames(CI)

  # Use regular expressions to extract row and column indices
  row_indices <- as.numeric(sub(".*\\[(\\d+),(\\d+)\\]", "\\1", rn))
  col_indices <- as.numeric(sub(".*\\[(\\d+),(\\d+)\\]", "\\2", rn))

  # Step 3: Map indices to actual row and column names
  CI_df <- data.frame(
    Row = row_names[row_indices],
    Col = col_names[col_indices],
    lbound = CI[, "lbound"],
    ubound = CI[, "ubound"]
  )

  # Step 4: Merge the reshaped matrix and CI data frame
  result <- merge(df_long, CI_df, by = c("Row", "Col"), all.x = TRUE, sort = FALSE)

  return(result)
}
