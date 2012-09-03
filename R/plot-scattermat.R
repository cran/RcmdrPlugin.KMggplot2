#' Scatter Plot Matrix Subclass
#'
#' \code{scattermat} class is a subclass for a scatter plot matrix.
#'
#' This class is a subclass which show dialog boxes of a scatter plot matrix for graphics editing.
#'
#' @section Fields:
#' \describe{
#' \item{\code{top}: }{\code{tkwin} class object; parent of widget window.}
#' \item{\code{alternateFrame}: }{\code{tkwin} class object; a special frame for some GUI parts.}
#' \item{\code{vbbox1}: }{\code{variableboxes} class object; the frame to select variables.}
#' \item{\code{lbbox1}: }{\code{textfields} class object; the frame to set the main title.}
#' \item{\code{rbbox1}: }{\code{radioboxes} class object; the frame to set the smoothing type.}
#' \item{\code{tbbox1}: }{\code{toolbox} class object; the frame to set the font, the colour set, other option, and the theme.}
#' }
#' @section Contains:
#' NULL
#' @section Methods:
#' \describe{
#' \item{\code{plotWindow()}: }{Create the window that make plots.}
#' \item{\code{savePlot(plot)}: }{Save the plot.}
#' \item{\code{registRmlist(object)}: }{Register deletable temporary objects.}
#' \item{\code{removeRmlist()}: }{Remove registered temporary objects.}
#' \item{\code{setFront()}: }{Set front parts of frames.}
#' \item{\code{setBack()}: }{Set back parts of frames.}
#' \item{\code{getWindowTitle()}: }{Get the title of the window.}
#' \item{\code{getHelp()}: }{Get the title of the help document.}
#' \item{\code{getParms()}: }{Get graphics settings parameters.}
#' \item{\code{checkTheme(index)}: }{Check themes.}
#' \item{\code{checkVariable(var)}: }{Check a variable length.}
#' \item{\code{checkError(parms)}: }{Check errors.}
#' \item{\code{setDataframe(parms)}: }{Set data frames.}
#' \item{\code{getGgplot(parms)}: }{Get \code{ggplot}.}
#' \item{\code{getGeom(parms)}: }{Get \code{geom}.}
#' \item{\code{getScale(parms)}: }{Get \code{scale}.}
#' \item{\code{getCoord(parms)}: }{Get \code{coord}.}
#' \item{\code{getFacet(parms)}: }{Get \code{facet}.}
#' \item{\code{getXlab(parms)}: }{Get \code{xlab}.}
#' \item{\code{getYlab(parms)}: }{Get \code{ylab}.}
#' \item{\code{getZlab(parms)}: }{Get \code{zlab}.}
#' \item{\code{getMain(parms)}: }{Get the main label.}
#' \item{\code{getTheme(parms)}: }{Get \code{theme}.}
#' \item{\code{getOpts(parms)}: }{Get other \code{opts}.}
#' \item{\code{getPlot(parms)}: }{Get the plot object.}
#' \item{\code{getMessage()}: }{Get the plot error message.}
#' }
#' @family plot
#'
#' @name scattermat-class
#' @aliases scattermat
#' @rdname plot-scattermat
#' @docType class
#' @keywords hplot
#' @export scattermat
scattermat <- setRefClass(

  Class = "scattermat",

  fields = c("vbbox1", "vbbox2", "lbbox1", "rbbox1", "tbbox1"),

  contains = c("plot_base"),

  methods = list(

    setFront = function() {

      vbbox1 <<- variableboxes$new()
      vbbox1$front(
        top       = top, 
        types     = list(nonFactors()),
        titles    = list(
            gettextKmg2("Select variables (three or more)")
        ),
        modes     = list("multiple"),
        initialSelection = list(0:2)
      )

      lbbox1 <<- textfields$new()
      lbbox1$front(
        top        = top,
        initValues = list(""),
        titles     = list(
            gettextKmg2("Title")
        )
      )

      rbbox1 <<- radioboxes$new()
      rbbox1$front(
        top    = top,
        labels = list(
            gettextKmg2("None"),
            gettextKmg2("Smoothing with C.I. (linear regression)"),
            gettextKmg2("Smoothing without C.I. (linear regression)"),
            gettextKmg2("Smoothing with C.I. (loess or gam)"),
            gettextKmg2("Smoothing without C.I. (loess or gam)")
        ),
        title  = gettextKmg2("Smoothing type")
      )

      tbbox1 <<- toolbox$new()
      tbbox1$front(top, showcolourbox = FALSE)

    },

    setBack = function() {

      vbbox1$back()
      lbbox1$back()
      rbbox1$back()
      tbbox1$back(4)

    },

    getWindowTitle = function() {
      
      gettextKmg2("Scatter plot matrix")
      
    },
    
    getHelp = function() {
      
      "plotmatrix"
      
    },

    getParms = function() {

      x      <- getSelection(vbbox1$variable[[1]])
      y      <- character(0)
      z      <- character(0)

      s      <- character(0)
      t      <- character(0)

      xlab   <- ""
      xauto  <- ""
      ylab   <- ""
      yauto  <- ""
      zlab   <- ""
      main   <- tclvalue(lbbox1$fields[[1]]$value)

      size   <- tclvalue(tbbox1$size$value)
      family <- getSelection(tbbox1$family)
      colour <- character(0)
      save   <- tclvalue(tbbox1$goption$value[[1]])
      theme  <- checkTheme(tclvalue(tbbox1$theme$value))

      options(
        kmg2FontSize   = tclvalue(tbbox1$size$value),
        kmg2FontFamily = seq_along(tbbox1$family$varlist)[tbbox1$family$varlist == getSelection(tbbox1$family)] - 1,
        kmg2SaveGraph  = tclvalue(tbbox1$goption$value[[1]]),
        kmg2Theme      = tclvalue(tbbox1$theme$value)
      )

      smoothType   <- tclvalue(rbbox1$value)

      list(
        x = x, y = y, z = z, s = s, t = t,
        xlab = xlab, xauto = xauto, ylab = ylab, yauto = yauto, zlab = zlab, main = main,
        size = size, family = family, colour = colour, save = save, theme = theme,
        smoothType = smoothType
      )

    },

    checkError = function(parms) {

      if (length(parms$x) < 3) {
        errorCondition(
          recall  = windowScatter,
          message = gettextKmg2("Please select more than 3 variables.")
        )
        errorCode <- TRUE
      } else {
        errorCode <- FALSE
      }
      errorCode

    },

    setDataframe = function(parms) {

      command <- do.call(paste, c(parms$x, list(sep = "\", \"")))
      command <- paste0(".df <- ", ActiveDataSet(), "[c(\"", command, "\")]")

      doItAndPrint(command)
      registRmlist(.df)

    },

    getGgplot = function(parms) {

      "plotmatrix(.df) + "

    },

    getGeom = function(parms) {

      if (parms$smoothType == "1") {
        geom <-  ""
      } else if (parms$smoothType == "2") {
        geom <-  "stat_smooth(method = \"lm\") + "
      } else if (parms$smoothType == "3") {
        geom <-  "stat_smooth(method = \"lm\", se = FALSE) + "
      } else if (parms$smoothType == "4") {
        geom <-  "stat_smooth() + "
      } else if (parms$smoothType == "5") {
        geom <-  "stat_smooth(se = FALSE) + "
      }
      geom

    },

    getOpts = function(parms) {

      opts <- list()
      if (length(parms$s) != 0 || length(parms$t) != 0) {
        opts <- c(opts, "panel.margin = unit(0.3, \"lines\")")
      }
      if (nchar(parms$main) != 0) {
        opts <- c(opts, paste0("plot.title = element_text(size = rel(1.2), vjust = 1.5)"))
      }

      if (length(opts) != 0) {
        opts <- do.call(paste, c(opts, list(sep = ", ")))
        opts <- paste0(" + theme(", opts, ")")
      } else {
        opts <- ""
      }
      opts

    },

    getMessage = function() {

      gettextKmg2("Smoothing failed.  Please try another smoothing type, or check the data and variables.")

    }

  )
)



#' Wrapper Function of Scatter Plot Matrix Subclass
#'
#' \code{windowScattermat} function is a wrapper function of \code{scattermat} class for the R-commander menu bar.
#'
#' @rdname plot-scattermat-windowScattermat
#' @keywords hplot
#' @export
windowScattermat <- function() {

  Scattermat <- scattermat$new()
  Scattermat$plotWindow()

}
