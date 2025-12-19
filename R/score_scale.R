#' Score a questionnaire or scale with optional reverse coding
#'
#' Computes scale scores from item-level data, with optional reverse coding
#' and a minimum proportion of answered items required.
#'
#' @param df Data frame containing item responses.
#' @param items Character vector of item column names.
#' @param reverse Optional character vector of items to reverse-code.
#' @param min_val Minimum possible item value. If NULL, inferred from data.
#' @param max_val Maximum possible item value. If NULL, inferred from data.
#' @param id_var Name of ID variable in \code{df}.
#' @param out_var Name of score variable if returning a data frame.
#' @param prop_required Proportion of items required to compute a score.
#' @param return_df Logical. If TRUE, return a data.frame with ID and score.
#'   If FALSE (default), return a numeric vector of scores.
#'
#' @return A numeric vector or a data.frame, depending on \code{return_df}.
#'
#' @details
#' Scores are computed as the row mean multiplied by the number of
#' answered items. Scores are set to NA if the proportion of answered
#' items is below \code{prop_required}.
#'
#' @export
score_scale <- function(df,
                        items,
                        reverse = NULL,
                        min_val = NULL,
                        max_val = NULL,
                        id_var = "SubjectID",
                        out_var = "total_score",
                        prop_required = 0.5,
                        return_df = FALSE) {

  stopifnot(is.data.frame(df))
  df <- as.data.frame(df)

  if (!id_var %in% names(df)) stop("id_var not found in data frame.")
  if (!all(items %in% names(df))) stop("Some 'items' not found.")
  if (!is.null(reverse) && !all(reverse %in% items)) {
    stop("All 'reverse' vars must be in 'items'.")
  }
  if (prop_required < 0 || prop_required > 1) {
    stop("'prop_required' must be between 0 and 1.")
  }

  x <- df[items]

  # Infer min and max if not supplied
  if (is.null(min_val)) {
    min_val <- min(
      vapply(x, function(col) min(col, na.rm = TRUE), numeric(1)),
      na.rm = TRUE
    )
  }
  if (is.null(max_val)) {
    max_val <- max(
      vapply(x, function(col) max(col, na.rm = TRUE), numeric(1)),
      na.rm = TRUE
    )
  }

  # Reverse coding: new = (min_val + max_val) - old
  if (!is.null(reverse) && length(reverse) > 0) {
    rev_idx <- match(reverse, items)
    for (j in rev_idx) {
      col <- x[[j]]
      x[[j]] <- ifelse(is.na(col), NA, (min_val + max_val) - col)
    }
  }

  n_items <- length(items)
  n_answered <- rowSums(!is.na(x))
  row_mean <- rowMeans(x, na.rm = TRUE)

  # Score equals mean * number of answered items
  score <- row_mean * n_answered

  prop_answered <- n_answered / n_items
  score[prop_answered < prop_required | n_answered == 0] <- NA_real_

  if (!return_df) {
    return(score)
  }

  out <- df[c(id_var)]
  out[[out_var]] <- score
  rownames(out) <- NULL

  out
}
