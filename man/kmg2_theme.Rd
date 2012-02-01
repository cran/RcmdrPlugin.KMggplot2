\name{kmg2_theme}
\alias{kmg2_theme_gray}
\alias{kmg2_theme_gray_natrisk}
\alias{kmg2_theme_gray_natrisk21}
\alias{kmg2_theme_bw}
\alias{kmg2_theme_bw_natrisk}
\alias{kmg2_theme_bw_natrisk21}
\title{Extend graphics theme}
\description{
  These functions are modified graphics themes for ggplot2 package.
}
\usage{
  kmg2_theme_gray(base_size = 25, base_family = "")
}
\arguments{
  \item{base_size}{base font size}
  \item{base_family}{font family name}
}
\examples{
require(ggplot2)

qplot(x=rnorm(10), y=rnorm(10), geom="point") + kmg2_theme_gray(20)
}
\seealso{
  \code{\link[ggplot2:ggplot]{ggplot}}
}
\keyword{color}
