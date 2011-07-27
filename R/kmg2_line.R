kmg2_line <- setRefClass(

  Class = "kmg2_line",

  fields = c("top", "buttonsFrame", "alternateFrame"),

  methods = list(

    plotLine = function(title=kmg2_gettextRcmdr("Line chart")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)
      alternateFrame <<- tkframe(top)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(Numeric(), Numeric(), Factors()),
        titles    = list(
            kmg2_gettextRcmdr("X variable (pick one)"),
            kmg2_gettextRcmdr("Y variable (pick one)"),
            kmg2_gettextRcmdr("Stratum variable"))
      )

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
        top        = top,
        initValues = list("<auto>", "<auto>", "<auto>", ""),
        titles     = list(
            kmg2_gettextRcmdr("Horizontal axis label"),
            kmg2_gettextRcmdr("Vertical axis label"),
            kmg2_gettextRcmdr("Legend label"),
            kmg2_gettextRcmdr("Title"))
      )

      rbbox1 <- kmg2_radioboxes$new()
      rbbox1$front(
        top    = alternateFrame,
        labels = list(
            kmg2_gettextRcmdr("Line"),
            kmg2_gettextRcmdr("Step"),
            kmg2_gettextRcmdr("Area")),
        title  = kmg2_gettextRcmdr("Line plot type")
      )

      rbbox2 <- kmg2_radioboxes$new()
      rbbox2$front(
        top    = alternateFrame,
        labels = list(
            kmg2_gettextRcmdr("None"),
            kmg2_gettextRcmdr("Mean"),
            kmg2_gettextRcmdr("Sum")),
        title  = kmg2_gettextRcmdr("Summarization")
      )

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top)

      onOK <- function() {

        x            <- getSelection(vbbox1$vbvariables[[1]])
        y            <- getSelection(vbbox1$vbvariables[[2]])
        z            <- getSelection(vbbox1$vbvariables[[3]])

        xlab         <- tclvalue(lbbox1$lbtboxes[[1]]$tvariable)
        ylab         <- tclvalue(lbbox1$lbtboxes[[2]]$tvariable)
        zlab         <- tclvalue(lbbox1$lbtboxes[[3]]$tvariable)
        main         <- tclvalue(lbbox1$lbtboxes[[4]]$tvariable)

        lineType     <- tclvalue(rbbox1$rbvariable)
        summaryType  <- tclvalue(rbbox2$rbvariable)

        fontSize     <- tclvalue(tbbox1$tbfontsizebox$tvariable)
        colourset    <- getSelection(tbbox1$tbcolourbox)
        fontfoamily  <- getSelection(tbbox1$tbfontfoamilybox)
        saveFile     <- tclvalue(tbbox1$tbcheckbox$cbvariables[[1]])

        if (tclvalue(tbbox1$tbthemebox$rbvariable) == "1")
          theme <- "kmg2_theme_gray"
        else if (tclvalue(tbbox1$tbthemebox$rbvariable) == "2")
          theme <- "kmg2_theme_bw"
        else
          theme <- "kmg2_theme_gray"

        closeDialog()
        if (length(x) == 0) {
          errorCondition(recall=plotLine, message=kmg2_gettextRcmdr("No x variable selected."))
          return()
        }
        if (length(y) == 0) {
          errorCondition(recall=plotLine, message=kmg2_gettextRcmdr("No y variable selected."))
          return()
        }

				xcheck <- na.omit(eval(parse(text=paste(.activeDataSet, "$", x, sep="")), 	envir=.GlobalEnv))
				if (!identical(order(xcheck), seq(along.with=xcheck))){
					response <- tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("x-values are not in order.\nContinue?"),
					icon="warning", type="okcancel", default="ok"))
					if (response == "cancel") {
						onCancel()
						return()
					}
				}

        #######################################################################
        if (nchar(xlab) == 0) {
          xlab <- paste("xlab(NULL) + ", sep="")
        } else if (xlab == "<auto>") {
          xlab <- paste("xlab(\"", x, "\") + ", sep="")
        } else {
          xlab <- paste("xlab(\"", xlab, "\") + ", sep="")
        }
        if (nchar(ylab) == 0) {
          ylab <- paste("ylab(NULL) + ", sep="")
        } else if (ylab == "<auto>") {
          ylab <- paste("ylab(\"", y, "\") + ", sep="")
        } else {
          ylab <- paste("ylab(\"", ylab, "\") + ", sep="")
        }
        if (nchar(main) == 0) {
          main <- ""
        } else {
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        }

        .activeDataSet <- ActiveDataSet()

        if (length(z) == 0) {
          command <- paste(
            ".df <- data.frame(",
            "x = ", .activeDataSet, "$", x, ",",
            "y = ", .activeDataSet, "$", y, ")",
            sep=""
          )
        } else {
          command <- paste(
            ".df <- data.frame(",
            "x = ", .activeDataSet, "$", x, ",",
            "y = ", .activeDataSet, "$", y, ",",
            "z = ", .activeDataSet, "$", z, ")",
            sep=""
          )
        }
        doItAndPrint(command)
				
        if (length(z) == 0) {
					scale <- ""
          zlab <- ""
					z <- ""
					opt <- ""
				} else if (nchar(zlab) == 0) {
  				if (any(lineType == c("1", "2"))) {
            scale <- paste("scale_colour_brewer(palette=\"", colourset, "\") + ", sep="")
            z <- ", colour = z, shape = z"
  				} else if (lineType == "3") {
            scale <- paste("scale_fill_brewer(palette=\"", colourset, "\") + ", sep="")
            z <- ", fill = z"
  				}
          zlab <- ""
					opt <- " + opts(legend.position = \"right\", legend.title = theme_blank())"
				} else if (zlab == "<auto>") {
  				if (any(lineType == c("1", "2"))) {
            scale <- paste("scale_colour_brewer(palette=\"", colourset, "\") + ", sep="")
            zlab <- paste("labs(colour = \"", z, "\", shape = \"", z, "\") + ", sep="")
            z <- ", colour = z, shape = z"
  				} else if (lineType == "3") {
            scale <- paste("scale_fill_brewer(palette=\"", colourset, "\") + ", sep="")
            zlab <- paste("labs(fill = \"", z, "\") + ", sep="")
            z <- ", fill = z"
  				}
					opt <- " + opts(legend.position = \"right\")"
        } else {
  				if (any(lineType == c("1", "2"))) {
            scale <- paste("scale_colour_brewer(palette=\"", colourset, "\") + ", sep="")
            zlab <- paste("labs(colour = \"", zlab, "\", shape = \"", zlab, "\") + ", sep="")
            z <- ", colour = z, shape = z"
  				} else if (lineType == "3") {
            scale <- paste("scale_fill_brewer(palette=\"", colourset, "\") + ", sep="")
            zlab <- paste("labs(fill = \"", zlab, "\") + ", sep="")
            z <- ", fill = z"
  				}
					opt <- " + opts(legend.position = \"right\")"
        }

        if (summaryType == "1") {
					if (lineType == "1") {
						geom <- "geom_point() + geom_line(size = 1) + "
					} else if (lineType == "2") {
						geom <- "geom_point() + geom_step(size = 1) + "
					} else if (lineType == "3") {
						geom <- "geom_area(alpha = 0.3) + "
					}
        } else if (summaryType == "2") {
					if (lineType == "1") {
						geom <- "stat_summary(fun.y = \"mean\", geom = \"point\") + stat_summary(fun.y = \"mean\", geom = \"line\") + "
					} else if (lineType == "2") {
						geom <- "stat_summary(fun.y = \"mean\", geom = \"point\") + stat_summary(fun.y = \"mean\", geom = \"step\") + "
					} else if (lineType == "3") {
						geom <- "stat_summary(fun.y = \"mean\", geom = \"area\", alpha = 0.3) + "
					}
        } else if (summaryType == "3") {
					if (lineType == "1") {
						geom <- "stat_summary(fun.y = \"sum\", geom = \"point\") + stat_summary(fun.y = \"sum\", geom = \"line\") + "
					} else if (lineType == "2") {
						geom <- "stat_summary(fun.y = \"sum\", geom = \"point\") + stat_summary(fun.y = \"sum\", geom = \"step\") + "
					} else if (lineType == "3") {
						geom <- "stat_summary(fun.y = \"sum\", geom = \"area\", alpha = 0.3) + "
					}
        }

        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(x = x, y = y", z, ")) + ",
          geom, scale,
          xlab, ylab, zlab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
					opt,
          sep=""
        )
        doItAndPrint(command)

        doItAndPrint("print(.plot1)")

        if (saveFile == "1") {      
          file <- tclvalue(tkgetSaveFile(filetypes=paste(
              "{\"PNG (Portable Network Graphics)\" {\".png\"}}",
              "{\"PDF (Portable Document Format)\" {\".pdf\"}}",
              "{\"JPEG (Joint Photographic Experts Group)\" {\".jpg\"}}"
            ), defaultextension="png", initialfile="GraphSave"
          ))
          if (file == "") return()
          command <- paste("kmg2_ggsave(\"", file, "\", .plot1)", sep="")
          doItAndPrint(command)
        }
        
				doItAndPrint("rm(.df, .plot1)")
				
        activateMenus()
        tkfocus(CommanderWindow())

      }

      # note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="geom_line")

      vbbox1$back()
      lbbox1$back()
			tkgrid(
				rbbox1$setframe,
        labelRcmdr(alternateFrame, text="    "),
        rbbox2$setframe, stick="nw")
      tkgrid(alternateFrame, stick="nw")
      tkgrid(labelRcmdr(alternateFrame, text="    "), stick="nw")
      tbbox1$back()

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

