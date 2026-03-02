#' Source all R scripts in a directory
#'
#' Lists files in a directory, sorts them alphabetically, and sources each
#' matching script.
#'
#' @param path Directory path containing scripts to source.
#' @param pattern Regular expression pattern used to match filenames.
#' @param recursive Logical; if `TRUE`, include files in subdirectories.
#' @param local Passed to [base::source()] `local` argument.
#'
#' @return Invisibly returns a list of values returned by [base::source()].
#' @export
source_dir <- function(path, pattern = "\\.R$", recursive = FALSE, local = FALSE) {

  files <- list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )

  files <- sort(files)

  invisible(lapply(files, source, local = local))
}
