#' Custom ggplot2 theme with transparent background
#'
#' A ggplot2 theme based on \code{theme_classic()} with optional transparent
#' backgrounds and configurable foreground color.
#'
#' @param Color Foreground color used for text, lines, and borders.
#' @param BG Background fill color. Default is \code{"transparent"}.
#' @param ... Additional arguments passed to \code{theme_classic()}.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'
#'   ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point() +
#'     HFS_theme()
#' }
#'
#' @export
HFS_theme <- function(Color = "black", BG = "transparent", ...) {

  ggplot2::theme_classic(...) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),

      # Transparent backgrounds
      panel.background = ggplot2::element_rect(fill = BG, color = NA),
      plot.background  = ggplot2::element_rect(fill = BG, color = NA),
      legend.background = ggplot2::element_rect(fill = BG),
      legend.box.background = ggplot2::element_rect(fill = BG, color = BG),

      # Borders and lines
      panel.border = ggplot2::element_rect(fill = NA, color = Color, linewidth = 1),
      axis.line    = ggplot2::element_line(color = Color, linewidth = 1),
      axis.ticks   = ggplot2::element_line(color = Color),
      line         = ggplot2::element_line(color = Color),
      rect         = ggplot2::element_rect(fill = BG, colour = Color),

      # Text
      text       = ggplot2::element_text(color = Color),
      axis.text  = ggplot2::element_text(color = Color),
      title      = ggplot2::element_text(color = Color),
      legend.title = ggplot2::element_text(hjust = 0.5)
    )
}
