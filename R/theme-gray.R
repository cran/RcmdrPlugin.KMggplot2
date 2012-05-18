#' A Modified Gray Theme for Kaplan-Meier Plots
#'
#' These functions are modified graphics themes for Kaplan-Meier plots.
#'
#' @param base_size base font size
#' @param base_family font family name
#' @seealso \code{\link[ggplot2:ggplot]{ggplot}}
#'
#' @rdname theme-gray
#' @aliases theme_gray_natrisk21
#' @keywords color
#' @export
theme_gray_natrisk <- function(base_size = 25, base_family = "") {

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
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_text(family = base_family, size = base_size * 0.8, angle = -90, colour = "transparent"),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 1, 0, 0.5), "lines")
  )
  modifyList(theme_gray(base_size = base_size, base_family = base_family), mod)

}



#' @export
theme_gray_natrisk21 <- function(base_size = 25, base_family = "") {

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
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_blank(),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 0, 0, -3), "lines")
  )
  modifyList(theme_gray(base_size = base_size, base_family = base_family), mod)

}
