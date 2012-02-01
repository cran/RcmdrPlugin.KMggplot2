\name{kmg2_ggsave}
\alias{kmg2_ggsave}
\title{kmg2_ggsave}
\description{
  This is a modified function of ggsave for recordedplot class objects.
}
\usage{
  kmg2_ggsave(
    filename=default_name(plot), plot = last_plot(),
    device=default_device(filename), path = NULL, scale=1,
    width=par("din")[1], height=par("din")[2], dpi=300,
    keep = plot$options$keep, drop = plot$options$drop, ...
  )
}
\arguments{
  \item{filename}{file name/filename of plot}
  \item{plot}{plot to save, defaults to last plot displayed}
  \item{device}{device to use, automatically extract from file name extension}
  \item{path}{path to save plot to (if you just want to set path and not filename)}
  \item{scale}{scaling factor}
  \item{width}{width (in inches)}
  \item{height}{height (in inches)}
  \item{dpi}{dpi to use for raster graphics}
  \item{keep}{plot components to keep}
  \item{drop}{plot components to drop}
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
