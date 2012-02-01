\name{kmg2_listRecall}
\alias{kmg2_listRecall}
\title{A function to convert a list into dot-dot-dot arguments}
\description{
  A function to convert a list into dot-dot-dot arguments.
}
\usage{
kmg2_listRecall(l, fun, ...)
}
\arguments{
  \item{l}{a list object}
  \item{fun}{a function to apply the list object after coverting into dot-dot-dot arguments}
  \item{...}{other arguments for \code{fun}}
}
\examples{
require(tcltk)
tt <- tktoplevel()
b1 <- tkbutton(tt, text = "OK 1", command = function() tkdestroy(tt))
b2 <- tkbutton(tt, text = "OK 2", command = function() tkdestroy(tt))
b3 <- tkbutton(tt, text = "OK 3", command = function() tkdestroy(tt))
b4 <- list(b1, b2, b3)

kmg2_listRecall(b4, tkgrid, sticky = "nw")
}
\keyword{misc}
