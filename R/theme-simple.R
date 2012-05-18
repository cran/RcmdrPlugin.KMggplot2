#' A Simple Black and White Themes
#'
#' These functions are modified graphics themes for Kaplan-Meier plots.
#'
#' @param base_size base font size
#' @param base_family font family name
#' @seealso \code{\link[ggplot2:ggplot]{ggplot}}
#'
#' @rdname theme-simple
#' @aliases theme_simple_natrisk theme_simple_natrisk21
#' @keywords color
#' @export
theme_simple <- function(base_size = 16, base_family = "") {

  mod <- list(
    strip.background = theme_rect(fill = "grey90", colour = "grey90"),
    panel.border = theme_rect2(colour = "grey50"),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank()
  )
  modifyList(theme_bw(base_size = base_size, base_family = base_family), mod)

}



#' @export
theme_simple_natrisk <- function(base_size = 16, base_family = "") {

  mod <- list(
    axis.text.x = theme_blank(),
    axis.text.y = theme_text(family = base_family, size = base_size * 0.8, hjust = 1, lineheight = 0.9, colour = "transparent"),
    axis.title.x = theme_blank(),
    axis.title.y = theme_text(family = base_family, size = base_size, vjust = 0.5, angle = 90, colour = "transparent"),
    axis.ticks = theme_segment(colour = "transparent"),
    legend.position = "none",
    panel.background = theme_rect(fill = "transparent", colour = "transparent"),
    panel.grid.major = theme_line(colour = "transparent", size = 0.2),
    panel.grid.minor = theme_line(colour = "transparent", size = 0.5),
    panel.border = theme_blank(),
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_text(family = base_family, size = base_size * 0.8, angle = -90, colour = "transparent"),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 1, 0, 0.5), "lines")
  )
  modifyList(theme_simple(base_size = base_size, base_family = base_family), mod)

}



#' @export
theme_simple_natrisk21 <- function(base_size = 16, base_family = "") {

  mod <- list(
    axis.line = theme_blank(),
    axis.text.x = theme_blank(),
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(),
    axis.title.x = theme_blank(),
    axis.title.y = theme_blank(),
    legend.position = "none",
    panel.background = theme_rect(fill = "transparent", colour = "transparent"),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.margin = unit(0, "lines"),
    panel.border = theme_blank(),
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_blank(),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 0, 0, -3), "lines")
  )
  modifyList(theme_simple(base_size = base_size, base_family = base_family), mod)

}



#' Theme element: modified rectangle (top and right borders removed).
#' 
#' Most often used for backgrounds and borders.
#' 
#' @seealso \code{\link{polylineGrob}} for underlying grid function
#' @param colour border color
#' @param size border size
#' @param linetype border linetype
#' @keywords dplot
#' @rdname theme-theme_rect2
#' @export
theme_rect2 <- function(colour = "black", size = 0.5, linetype = 1) {
  structure(
    function(x = c(0, 1, 0, 0), y = c(0, 0, 0, 1), ..., default.units = "npc") {
      .pt <- 1 / 0.352777778
      polylineGrob(
        x, y, ..., default.units = default.units,
        gp = gpar(lwd = size * .pt, col = colour, lty = linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}
