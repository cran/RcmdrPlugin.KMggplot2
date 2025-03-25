#' Step ribbon plots.
#'
#' \code{geom_stepribbon} is an extension of the \code{geom_ribbon}, and
#' is optimized for Kaplan-Meier plots with pointwise confidence intervals
#' or a confidence band.
#'
#' @section Aesthetics:
#' \code{geom_stepribbon()} understands the following aesthetics. Required
#' aesthetics are displayed in bold and defaults are displayed for optional 
#' aesthetics:
#' \tabular{rl}{
#' • \tab \strong{\code{\link[ggplot2:aes_position]{x}} \emph{or} \code{\link[ggplot2:aes_position]{y}}}  \cr\cr
#' • \tab \strong{\code{\link[ggplot2:aes_position]{ymin}} \emph{or} \code{\link[ggplot2:aes_position]{xmin}}}  \cr\cr
#' • \tab \strong{\code{\link[ggplot2:aes_position]{ymax}} \emph{or} \code{\link[ggplot2:aes_position]{xmax}}}  \cr\cr
#' • \tab \code{\link[ggplot2:aes_colour_fill_alpha]{alpha}}  \cr\cr
#' • \tab \code{\link[ggplot2:aes_colour_fill_alpha]{colour}}  \cr\cr
#' • \tab \code{\link[ggplot2:aes_colour_fill_alpha]{fill}}  \cr\cr
#' • \tab \code{\link[ggplot2:aes_group_order]{group}}  \cr\cr
#' • \tab \code{\link[ggplot2:aes_linetype_size_shape]{linetype}}  \cr\cr
#' • \tab \code{\link[ggplot2:aes_linetype_size_shape]{linewidth}}  \cr\cr
#' }
#' @seealso
#'   \code{\link[ggplot2:geom_ribbon]{geom_ribbon}} \code{geom_stepribbon}
#'   inherits from \code{geom_ribbon}.
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If 
#' specified and inherit.aes = TRUE (the default), it is combined with the
#' default mapping at the top level of the plot. You must supply mapping if
#' there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position A position adjustment to use on the data for this layer.
#' @param na.rm If FALSE, the default, missing values are removed with a
#' warning. If TRUE, missing values are silently removed.
#' @param orientation The orientation of the layer.
#' @param show.legend logical. Should this layer be included in the legends?
#' NA, the default, includes if any aesthetics are mapped. FALSE never includes,
#' and TRUE always includes. It can also be a named logical vector to finely
#' select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them. This is most useful for helper functions that define
#' both data and aesthetics and shouldn't inherit behaviour from the default
#' plot specification, e.g. \code{borders()}.
#' @param outline.type Type of the outline of the area; "both" draws both the
#' upper and lower lines, "upper"/"lower" draws the respective lines only.
#' "full" draws a closed polygon around the area.
#' @param kmplot If \code{TRUE}, missing values are replaced by the previous
#' values. This option is needed to make Kaplan-Meier plots if the last
#' observation has event, in which case the upper and lower values of the
#' last observation are missing. This processing is optimized for results
#' from the survfit function.
#' @param ... Other arguments passed on to layer()'s params argument.
#' @examples
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' h <- ggplot(huron, aes(year))
#' h + geom_stepribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
#'     geom_step(aes(y = level))
#' h + geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
#'     geom_line(aes(y = level))
#' @rdname geom_stepribbon
#' @importFrom ggplot2 layer GeomRibbon
#' @importFrom rlang list2 arg_match0
#' @export
geom_stepribbon <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE,
  outline.type = "both", kmplot = FALSE, ...) {
  outline.type <- rlang::arg_match0(outline.type, c("both", "upper", "lower", "full"))
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStepribbon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      kmplot = kmplot,
      ...
    )
  )
}

#' @rdname geom_stepribbon
#' @format NULL
#' @usage NULL
#' @export
GeomStepribbon <- ggproto(
  "GeomStepribbon", GeomRibbon, 
  
  extra_params = c("na.rm", "kmplot"),
  
  draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
    if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- rbind(data, data)
    data <- data[order(data$x), ]
    data$x <- c(data$x[2:nrow(data)], NA)
    data <- data[complete.cases(data["x"]), ]
    GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
  },
  
  setup_data = function(data, params) {
    if (params$kmplot) {
      data <- data[order(data$PANEL, data$group, data$x), ]
      tmpmin <- tmpmax <- NA
      for (i in 1:nrow(data)) {
        if (is.na(data$ymin[i])) {
          data$ymin[i] <- tmpmin
        }
        if (is.na(data$ymax[i])) {
          data$ymax[i] <- tmpmax
        }
        tmpmin <- data$ymin[i]
        tmpmax <- data$ymax[i]
      }
    }
    data
  }
  
)
