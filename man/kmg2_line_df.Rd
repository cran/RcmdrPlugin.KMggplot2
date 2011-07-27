\name{kmg2_line_df}
\alias{kmg2_line_df}
\docType{data}
\title{A dummy data for line charts}
\description{
  A dummy data for line charts.
}
\format{
  A data frame with 300 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{a discrete x-axis variable}
    \item{\code{y}}{a continuous y-axis variable}
    \item{\code{z}}{a group variable (has three groups "1", "2", and "3")}
  }
}
\examples{
try(data(kmg2_line_df, package = "RcmdrPlugin.KMggplot2"))
try(activeDataSet("kmg2_line_df"))
# kmg2_plot_line()
}
\seealso{
  \code{\link{kmg2_plot_line}}
}
\keyword{data}
