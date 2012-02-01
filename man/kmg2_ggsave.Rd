\name{kmg2_ggsave}
\alias{kmg2_ggsave}
\title{kmg2_ggsave}
\description{
  This is a modified function of ggsave for recordedplot class objects.
}
\usage{
  kmg2_ggsave(filename = default_name(plot), plot = last_plot(),
    device = default_device(filename), path = NULL,
    scale = 1, width = par("din")[1],
    height = par("din")[2], units = c("in", "cm", "mm"),
    dpi = 300, ...)
}
\arguments{
  \item{filename}{file name/filename of plot}

  \item{plot}{plot to save, defaults to last plot
  displayed}

  \item{device}{device to use, automatically extract from
  file name extension}

  \item{path}{path to save plot to (if you just want to set
  path and not filename)}

  \item{scale}{scaling factor}

  \item{width}{width (defaults to the width of current
  plotting window)}

  \item{height}{height (defaults to the height of current
  plotting window)}

  \item{units}{units for width and height when either one
  is explicitly specified (in, cm, or mm)}

  \item{dpi}{dpi to use for raster graphics}

  \item{...}{other arguments passed to graphics device}
}
\examples{
require(ggplot2)

p <- qplot(x=rnorm(10), y=rnorm(10), geom="point")
kmg2_ggsave("myplot.png", p)
}
\seealso{
  \code{\link[ggplot2:ggsave]{ggsave}}
}
\keyword{hplot}
