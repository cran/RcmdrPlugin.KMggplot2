\docType{class}
\name{plot_base-class}
\alias{plot_base}
\alias{plot_base-class}
\title{Base Class for Plot Windows}
\description{
  \code{plot_base} class is the base class for plot
  windows.
}
\details{
  This class is a framework for implementing subclasses
  which show dialog boxes for graphics editing.
}
\section{Fields}{
  \describe{ \item{\code{top}: }{\code{tkwin} class object;
  parent of widget window.} \item{\code{alternateFrame}:
  }{\code{tkwin} class object; a special frame for some GUI
  parts.} \item{\code{rmlist}: }{List of character;
  deletable temporary objects.} }
}

\section{Contains}{
  NULL
}

\section{Methods}{
  \describe{ \item{\code{plotWindow()}: }{Create the window
  that make plots.} \item{\code{savePlot(plot)}: }{Save the
  plot.} \item{\code{registRmlist(object)}: }{Register
  deletable temporary objects.}
  \item{\code{removeRmlist()}: }{Remove registered
  temporary objects.} \item{\code{setFront()}: }{Set front
  parts of frames.} \item{\code{setBack()}: }{Set back
  parts of frames.} \item{\code{getWindowTitle()}: }{Get
  the title of the window.} \item{\code{getHelp()}: }{Get
  the title of the help document.} \item{\code{getParms()}:
  }{Get graphics settings parameters.}
  \item{\code{checkTheme(index)}: }{Check themes.}
  \item{\code{checkVariable(var)}: }{Check a variable
  length.} \item{\code{checkError(parms)}: }{Check errors.}
  \item{\code{setDataframe(parms)}: }{Set data frames.}
  \item{\code{getGgplot(parms)}: }{Get \code{ggplot}.}
  \item{\code{getGeom(parms)}: }{Get \code{geom}.}
  \item{\code{getScale(parms)}: }{Get \code{scale}.}
  \item{\code{getCoord(parms)}: }{Get \code{coord}.}
  \item{\code{getFacet(parms)}: }{Get \code{facet}.}
  \item{\code{getXlab(parms)}: }{Get \code{xlab}.}
  \item{\code{getYlab(parms)}: }{Get \code{ylab}.}
  \item{\code{getZlab(parms)}: }{Get \code{zlab}.}
  \item{\code{getMain(parms)}: }{Get the main label.}
  \item{\code{getTheme(parms)}: }{Get \code{theme}.}
  \item{\code{getOpts(parms)}: }{Get other \code{opts}.}
  \item{\code{getPlot(parms)}: }{Get the plot object.}
  \item{\code{getMessage()}: }{Get the plot error message.}
  }
}
\seealso{
  Other plot: \code{\link{box-class}},
  \code{\link{checkError,plot_base-method}},
  \code{\link{checkTheme,plot_base-method}},
  \code{\link{checkVariable,plot_base-method}},
  \code{\link{contour-class}},
  \code{\link{discretebar-class}},
  \code{\link{dist-class}}, \code{\link{factorize-class}},
  \code{\link{getCoord,plot_base-method}},
  \code{\link{getFacet,plot_base-method}},
  \code{\link{getGeom,plot_base-method}},
  \code{\link{getGgplot,plot_base-method}},
  \code{\link{getHelp,plot_base-method}},
  \code{\link{getMain,plot_base-method}},
  \code{\link{getMessage,plot_base-method}},
  \code{\link{getOpts,plot_base-method}},
  \code{\link{getParms,plot_base-method}},
  \code{\link{getPlot,plot_base-method}},
  \code{\link{getScale,plot_base-method}},
  \code{\link{getTheme,plot_base-method}},
  \code{\link{getWindowTitle,plot_base-method}},
  \code{\link{getXlab,plot_base-method}},
  \code{\link{getYlab,plot_base-method}},
  \code{\link{getZlab,plot_base-method}},
  \code{\link{hist-class}}, \code{\link{km-class}},
  \code{\link{line-class}}, \code{\link{pie-class}},
  \code{\link{plotWindow,plot_base-method}},
  \code{\link{qq-class}},
  \code{\link{registRmlist,plot_base-method}},
  \code{\link{removeRmlist,plot_base-method}},
  \code{\link{savePlot,plot_base-method}},
  \code{\link{scatter-class}},
  \code{\link{scattermat-class}},
  \code{\link{setBack,plot_base-method}},
  \code{\link{setDataframe,plot_base-method}},
  \code{\link{setFront,plot_base-method}}
}
\keyword{hplot}

