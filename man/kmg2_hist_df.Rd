\name{kmg2_hist_df}
\alias{kmg2_hist_df}
\docType{data}
\title{A dummy data for histograms}
\description{
  A dummy data for histograms.
}
\format{
  A data frame with 7500 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a random variable from a two-component normal-mixture}
    \item{\code{z}}{group (has three groups "mu=1" and "mu=2")}
  }
}
\examples{
try(data(kmg2_hist_df, package = "RcmdrPlugin.KMggplot2"))
try(activeDataSet("kmg2_hist_df"))
# kmg2_plot_hist()
}
\seealso{
  \code{\link{kmg2_plot_hist}}
}
\keyword{data}
