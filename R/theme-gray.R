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

  theme_gray(base_size = base_size, base_family = base_family) %+replace%
  theme(
    axis.text.x      = element_blank(),
    axis.text.y      = element_text(family = base_family, size = base_size * 0.8, hjust = 1, lineheight = 0.9, colour = "transparent"),
    axis.title.x     = element_blank(),
    axis.title.y     = element_text(family = base_family, size = base_size, vjust = 0.5, angle = 90, colour = "transparent"),
    axis.ticks       = element_line(colour = "transparent"),
    legend.position  = "none",
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.grid.major = element_line(colour = "transparent", size = 0.2),
    panel.grid.minor = element_line(colour = "transparent", size = 0.5),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.text.x     = element_blank(),
    strip.text.y     = element_text(family = base_family, size = base_size * 0.8, angle = -90, colour = "transparent"),
    plot.background  = element_rect(colour = "transparent", fill = "transparent"),
    plot.title       = element_blank(),
    plot.margin      = unit(c(0, 1, 0, 0.5), "lines")
  )

}



#' @export
theme_gray_natrisk21 <- function(base_size = 25, base_family = "") {

  theme_gray(base_size = base_size, base_family = base_family) %+replace%
  theme(
    axis.line        = element_blank(),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks       = element_blank(),
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    legend.position  = "none",
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin     = unit(0, "lines"),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.text.x     = element_blank(),
    strip.text.y     = element_blank(),
    plot.background  = element_rect(colour = "transparent", fill = "transparent"),
    plot.title       = element_blank(),
    plot.margin      = unit(c(0, 0, 0, -3), "lines")
  )

}
