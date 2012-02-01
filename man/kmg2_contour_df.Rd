\name{kmg2_contour_df}
\alias{kmg2_contour_df}
\docType{data}
\title{A dummy data for contour plots}
\description{
  A dummy data for contour plots.
}
\format{
  A data frame with 10000 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{a x-axis variable}
    \item{\code{y}}{a y-axis variable}
    \item{\code{z}}{density}
  }
}
\examples{
try(data(kmg2_contour_df, package = "RcmdrPlugin.KMggplot2"))
try(activeDataSet("kmg2_contour_df"))
# kmg2_plot_contour()
}
\seealso{
  \code{\link{kmg2_plot_contour}}
}
\keyword{data}
