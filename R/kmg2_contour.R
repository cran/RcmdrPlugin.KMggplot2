kmg2_contour <- setRefClass(

  Class = "kmg2_contour",

  fields = c("top", "buttonsFrame"),

  methods = list(

    plotContour = function(title=kmg2_gettextRcmdr("Contour plot")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(Numeric(), Numeric(), Numeric()),
        titles    = list(
            kmg2_gettextRcmdr("X variable (pick one)"),
            kmg2_gettextRcmdr("Y variable (pick one)"),
            kmg2_gettextRcmdr("Z variable (pick one)"))
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
            kmg2_gettextRcmdr("Coloured lines"),
            kmg2_gettextRcmdr("Heat map")),
        title  = kmg2_gettextRcmdr("Options")
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

        decoType     <- tclvalue(rbbox1$rbvariable)

        fontSize     <- tclvalue(tbbox1$tbfontsizebox$tvariable)
        colourset    <- getSelection(tbbox1$tbcolourbox)
        fontfoamily  <- getSelection(tbbox1$tbfontfoamilybox)
        saveFile     <- tclvalue(tbbox1$tbcheckbox$cbvariables[[1]])

        if (tclvalue(tbbox1$tbthemebox$rbvariable) == "1")
          theme <- "theme_gray"
        else if (tclvalue(tbbox1$tbthemebox$rbvariable) == "2")
          theme <- "theme_bw"
        else
          theme <- "theme_gray"

        closeDialog()
        if (length(x) == 0) {
          errorCondition(recall=plotScatter, message=kmg2_gettextRcmdr("No x variable selected."))
          return()
        }
        if (length(y) == 0) {
          errorCondition(recall=plotScatter, message=kmg2_gettextRcmdr("No y variable selected."))
          return()
        }
        if (length(z) == 0) {
          errorCondition(recall=plotScatter, message=kmg2_gettextRcmdr("No z variable selected."))
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

        command <- paste(
          ".df <- data.frame(",
          "x = ", .activeDataSet, "$", x, ",",
          "y = ", .activeDataSet, "$", y, ",",
          "z = ", .activeDataSet, "$", z, ")",
          sep=""
        )
        doItAndPrint(command)

        if (decoType == "1") {
					geom <- "stat_contour(size = 1) + "
	        scale <- ""
					opts <- ""
					zlab <- ""
				} else if (decoType == "2") {
					geom <- "stat_contour(aes(colour = ..level..), size = 1) + "
					scale <- paste(
						"scale_colour_gradient(low = brewer.pal(3, \"", colourset,
          	"\")[2], high = brewer.pal(3, \"", colourset, "\")[1]) + ",
						sep="")
					opts <- " + opts(legend.position = \"right\")"
          if (nchar(zlab) == 0) {
            opts <- paste(opts, " + opts(legend.title = theme_blank())", sep="")
          } else if (zlab == "<auto>") {
					  zlab <- paste("labs(colour = \"", z, "\") + ", sep="")
          } else {
					  zlab <- paste("labs(colour = \"", zlab, "\") + ", sep="")
          }
				} else {
					geom <- "geom_tile(aes(fill = z)) + stat_contour(size = 1) + "
					scale <- paste(
						"scale_fill_gradient(low = brewer.pal(3, \"", colourset,
          	"\")[2], high = brewer.pal(3, \"", colourset, "\")[1]) + ",
						sep="")
					opts <- " + opts(legend.position = \"right\")"
          if (nchar(zlab) == 0) {
            opts <- paste(opts, " + opts(legend.title = theme_blank())", sep="")
          } else if (zlab == "<auto>") {
						zlab <- paste("labs(fill = \"", z, "\") + ", sep="")
          } else {
						zlab <- paste("labs(fill = \"", zlab, "\") + ", sep="")
          }
				}

        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(x = x, y = y, z = z)) + ",
          geom, scale, 
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
							message=kmg2_gettextRcmdr("Contour plot failed.  This data may be inappropriate."),
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
      OKCancelHelp(helpSubject="stat_contour")

      vbbox1$back()
      lbbox1$back()
      rbbox1$back()
      tbbox1$back()

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

