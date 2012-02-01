kmg2_scatter <- setRefClass(

  Class = "kmg2_scatter",

  fields = c("top", "buttonsFrame"),

  methods = list(

    plotScatter = function(title=kmg2_gettextRcmdr("Scatter plot")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)

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
        top    = top,
        labels = list(
            kmg2_gettextRcmdr("None"),
            kmg2_gettextRcmdr("Smoothing with C.I. (linear regression)"),
            kmg2_gettextRcmdr("Smoothing without C.I. (linear regression)"),
            kmg2_gettextRcmdr("Smoothing with C.I. (loess or gam)"),
            kmg2_gettextRcmdr("Smoothing without C.I. (loess or gam)")),
        title  = kmg2_gettextRcmdr("Smoothing type")
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

        smoothType   <- tclvalue(rbbox1$rbvariable)

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
          errorCondition(recall=plotScatter, message=kmg2_gettextRcmdr("No x variable selected."))
          return()
        }
        if (length(y) == 0) {
          errorCondition(recall=plotScatter, message=kmg2_gettextRcmdr("No y variable selected."))
          return()
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

        scale <- ""
        if (length(z) == 0) {
          zlab <- ""
					colour <- ""
					zsmooth <- ""
					opts <- ""
				} else if (nchar(zlab) == 0) {
          scale <- paste("scale_colour_brewer(palette=\"", colourset, "\") + ", sep="")
          zlab <- ""
					colour <- ", colour = z, shape = z"
					if (smoothType != "4") {
						zsmooth <- "aes(fill = z), "
					} else {
						zsmooth <- "aes(fill = z)"
					}
					opts <- " + opts(legend.position = \"right\", legend.title = theme_blank())"
        } else {
          scale <- paste("scale_colour_brewer(palette=\"", colourset, "\") + ", sep="")
					if (smoothType == "1") {
						if (zlab == "<auto>") {
							zlab <- paste("labs(colour = \"", z, "\", shape = \"", z, "\") + ", sep="")
						} else {
							zlab <- paste("labs(colour = \"", zlab, "\", shape = \"", zlab, "\") + ", sep="")
						}
					} else {
						if (zlab == "<auto>") {
							zlab <- paste("labs(colour = \"", z, "\", shape = \"", z, "\", fill = \"", z, "\") + ", sep="")
						} else {
							zlab <- paste("labs(colour = \"", zlab, "\", shape = \"", zlab, "\", fill = \"", zlab, "\") + ", sep="")
						}
					}
					colour <- ", colour = z, shape = z"
					if (smoothType != "4") {
						zsmooth <- "aes(fill = z), "
					} else {
						zsmooth <- "aes(fill = z)"
					}
					opts <- " + opts(legend.position = \"right\")"
        }

				if (smoothType == "1") {
					smooth <- ""
				} else if (smoothType == "2") {
					smooth <- paste("stat_smooth(", zsmooth, "method = \"lm\") + ", sep="")
				} else if (smoothType == "3") {
					smooth <- paste("stat_smooth(", zsmooth, "method = \"lm\", se = FALSE) + ", sep="")
				} else if (smoothType == "4") {
					smooth <- paste("stat_smooth(", zsmooth, ") + ", sep="")
				} else if (smoothType == "5") {
					smooth <- paste("stat_smooth(", zsmooth, "se = FALSE) + ", sep="")
				}
				
				geom <- "geom_point() + "

        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(x = x, y = y", colour,")) + ",
          geom, scale, smooth,
          xlab, ylab, zlab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
					opts,
          sep=""
        )
        doItAndPrint(command)

				logger("print(.plot1)")
        response <- tryCatch({
						print(.plot1)
						""
					}, error = function(ex) {
						tclvalue(RcmdrTkmessageBox(
							message=kmg2_gettextRcmdr("Smoothing failed.  Try another smoothing type."),
							title=kmg2_gettextRcmdr("Error"),
		  				icon="error", type="ok", default="ok"
						))
					}
				)
				if (response == "ok") {
          doItAndPrint("rm(.df, .plot1)")
					return()
				}

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
      OKCancelHelp(helpSubject="stat_smooth")

      vbbox1$back()
      lbbox1$back()
      rbbox1$back()
      tbbox1$back()

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

