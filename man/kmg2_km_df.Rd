\name{kmg2_km_df}
\alias{kmg2_km_df}
\docType{data}
\title{A dummy data for survival analysis}
\description{
  A dummy data for survival analysis.
}
\format{
  A data frame with 99 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{time from entry (months)}
    \item{\code{event}}{event variable (0: censor, 1: event)}
    \item{\code{trt}}{treatment group (has three groups "Group-A", "Group-B", and "Group-C")}
  }
}
\examples{
try(data(kmg2_km_df, package = "RcmdrPlugin.KMggplot2"))
try(activeDataSet("kmg2_km_df"))
# kmg2_plot_km()
}
\seealso{
  \code{\link{kmg2_plot_km}}
}
\keyword{data}
