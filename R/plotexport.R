#' Save a ggplot in PDF, SVG, and PNG subfolders
#'
#' Save a plot to three file formats in separate 
#' subfolders (
#' \code{pdf}, \code{svg}, and \code{png}).
#' The function uses the same defaults as \code{ggsave()} for plot selection,
#' meaning that the last displayed plot is used when \code{plot} is not supplied.
#'
#' @param filename File name or path **without extension**.
#' @param plot Plot to save. Defaults to the last plot, like \code{ggsave()}.
#' @param path Path to save plots to (passed to \code{ggsave()}).
#' @param ... Additional arguments passed to \code{ggsave()}, such as
#'   \code{width}, \code{height}, \code{units}, and \code{dpi}.
#'
#' @return Invisibly returns a named character vector with file paths for
#'   the saved \code{pdf}, \code{svg}, and \code{png} files.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'
#'   p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'   plotexport("figures/scatter", plot = p, width = 6, height = 4)
#' }
#'
#' @export
plotexport <- function(filename,
                              plot = ggplot2::last_plot(),
                              path = NULL,
                              ...) {
  if (!is.character(filename) || length(filename) != 1 || is.na(filename) || !nzchar(filename)) {
    stop("`filename` must be a single non-empty character string.", call. = FALSE)
  }

  filename_no_ext <- tools::file_path_sans_ext(filename)

  device_map <- list(
    pdf = grDevices::cairo_pdf,
    svg = grDevices::svg,
    png = grDevices::png
  )

  out_files <- vapply(names(device_map), function(ext) {
    dir.create(file.path(dirname(filename_no_ext), ext), recursive = TRUE, showWarnings = FALSE)

    out_file <- file.path(dirname(filename_no_ext), ext, paste0(basename(filename_no_ext), ".", ext))

    ggplot2::ggsave(
      filename = out_file,
      plot = plot,
      device = device_map[[ext]],
      path = path,
      ...
    )

    out_file
  }, character(1))

  invisible(out_files)
}
