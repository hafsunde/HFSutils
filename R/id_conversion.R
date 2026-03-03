#' Convert prefixed IDs to integer IDs
#'
#' Converts IDs with a known prefix (for example `"k2_"`) to their integer
#' core. This is useful when downstream processing is faster on integer IDs.
#'
#' If `id_vec` is not a character vector, or if the first element does not start
#' with `prefix`, the input is returned unchanged and a message is emitted.
#'
#' @param id_vec Vector of IDs to convert.
#' @param prefix Prefix expected in character IDs.
#'
#' @return An integer vector when conversion is applied; otherwise the original
#'   `id_vec` unchanged.
#' @export
normalize_id <- function(id_vec, prefix = "k2_") {
  if (!is.character(id_vec) || !startsWith(id_vec[1], prefix)) {
    message("Unknown id-structure detected, returned unchanged")
    return(id_vec)
  }

  suppressWarnings(as.integer(substring(trimws(id_vec), nchar(prefix) + 1L)))
}

#' Convert integer IDs back to prefixed IDs
#'
#' Converts integer IDs (or integer-like IDs) to prefixed IDs, padding with
#' leading zeros up to `width`.
#'
#' If `id_vec` already appears to be prefixed IDs (based on the first element),
#' the input is returned unchanged and a message is emitted.
#'
#' @param id_vec Vector of IDs to convert.
#' @param prefix Prefix to prepend to the converted IDs.
#' @param width Integer width used for zero-padding.
#' @param pad Logical; whether to left-pad IDs with zeros up to `width`.
#'
#' @return A character vector with IDs in prefixed form.
#' @export
revert_id <- function(id_vec, prefix = "k2_", width = 7L, pad = TRUE) {
  if (is.character(id_vec) && startsWith(id_vec[1], prefix)) {
    message("Original id-structure detected, returned unchanged")
    return(id_vec)
  }

  id_int <- suppressWarnings(as.integer(id_vec))

  if (any(is.na(id_int) & !is.na(id_vec))) {
    stop("Some ids could not be coerced to integer, something is wrong.")
  }

  id_chr <- if (isTRUE(pad)) {
    sprintf(paste0("%0", width, "d"), id_int)
  } else {
    as.character(id_int)
  }

  paste0(prefix, id_chr)
}
