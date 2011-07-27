kmg2_box <- setRefClass(

  Class = "kmg2_box",

  fields = c("top", "buttonsFrame", "alternateFrame"),

  methods = list(

    plotBox = function(title=kmg2_gettextRcmdr("Box plot / Errorbar plot")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)
      alternateFrame <<- tkframe(top)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(c(Factors(), Numeric()), Numeric(), Factors()),
        titles    = list(
            kmg2_gettextRcmdr("X variable (pick one)"),
            kmg2_gettextRcmdr("Y variable (pick one)"),
            kmg2_gettextRcmdr("Stratum variable"))
      )

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
        top        = top,
        initValues = list("<auto>", "<auto>", ""),
        titles     = list(
            kmg2_gettextRcmdr("Horizontal axis label"),
            kmg2_gettextRcmdr("Vertical axis label"),
            kmg2_gettextRcmdr("Title"))
      )

      rbbox1 <- kmg2_radioboxes$new()
      rbbox1$front(
        top    = alternateFrame,
        labels = list(
            kmg2_gettextRcmdr("Box plot"),
            paste(kmg2_gettextRcmdr("Mean"), "\u00b1", kmg2_gettextRcmdr("S.D.")),
            paste(kmg2_gettextRcmdr("Mean"), "\u00b1", kmg2_gettextRcmdr("S.D. (bar)")),
            kmg2_gettextRcmdr("95% C.I. (t distribution)"),
            kmg2_gettextRcmdr("95% C.I. (bootstrap)")),
        title  = kmg2_gettextRcmdr("Plot type")
      )

      cbbox1 <- kmg2_checkboxes$new()
      cbbox1$front(
        top        = alternateFrame,
        initValues = list("0", "0", "1"),
        labels     = list(
            kmg2_gettextRcmdr("Flipped coordinates"),
            kmg2_gettextRcmdr("Add data point"),
            kmg2_gettextRcmdr("Annotation of plot type (except box plot)")),
        title      = kmg2_gettextRcmdr("Options")
      )

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top, showcolourbox = FALSE)

      onOK <- function() {

        x            <- getSelection(vbbox1$vbvariables[[1]])
        y            <- getSelection(vbbox1$vbvariables[[2]])
        z            <- getSelection(vbbox1$vbvariables[[3]])

        xlab         <- tclvalue(lbbox1$lbtboxes[[1]]$tvariable)
        ylab         <- tclvalue(lbbox1$lbtboxes[[2]]$tvariable)
        main         <- tclvalue(lbbox1$lbtboxes[[3]]$tvariable)

        plotType     <- tclvalue(rbbox1$rbvariable)
        flipedCoordinates <- tclvalue(cbbox1$cbvariables[[1]])
        addJitter    <- tclvalue(cbbox1$cbvariables[[2]])
        addAnnotation<- tclvalue(cbbox1$cbvariables[[3]])

        fontSize     <- tclvalue(tbbox1$tbfontsizebox$tvariable)
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
          errorCondition(recall=plotBox, message=kmg2_gettextRcmdr("No x variable selected."))
          return()
        }
        if (length(y) == 0) {
          errorCondition(recall=plotBox, message=kmg2_gettextRcmdr("No y variable selected."))
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

				if (plotType == "1") {
					if (addJitter == "1") {
						geom <- "geom_boxplot(outlier.colour = \"transparent\") + "
					} else {
						geom <- "geom_boxplot() + "
					}
					annotate <- ""
				} else if (any(plotType == c("2", "3"))) {
					if (plotType == "2") {
						geom <- paste(
							"stat_summary(fun.y = \"mean\", geom = \"point\") + ",	
							"stat_summary(fun.data = \"mean_sdl\", geom = \"errorbar\", mult = 1, width = 0.1) + ", sep="")
					} else {
						geom <- paste(
							"stat_summary(fun.y = \"mean\", geom = \"bar\") + ",	
							"stat_summary(fun.data = \"mean_sdl\", geom = \"errorbar\", mult = 1, width = 0.1) + ", sep="")
					}
					annotate <- paste(
						"grid.text(\"",
						paste("Mean", "\\u00b1", "S.D."),
						"\", x = unit(0.03, \"npc\"), y = unit(0.03, \"npc\"), just = c(\"left\", \"bottom\"), name = \"annotate.text\"); ", sep="")
				} else if (plotType == "4") {
					geom <- paste(
						"stat_summary(fun.y = \"mean\", geom = \"point\") + ",	
						"stat_summary(fun.data = \"mean_cl_normal\", geom = \"errorbar\", conf.int = .95, width = 0.1) + ", sep="")
					annotate <- "grid.text(\"95% C.I.\", x = unit(0.03, \"npc\"), y = unit(0.03, \"npc\"), just = c(\"left\", \"bottom\"), name = \"annotate.text\"); "
				} else if (plotType == "5") {
					geom <- paste(
						"stat_summary(fun.y = \"mean\", geom = \"point\") + ",	
						"stat_summary(fun.data = \"mean_cl_boot\", geom = \"errorbar\", conf.int = .95, width = 0.1) + ", sep="")
					annotate <- "grid.text(\"95% bootstrap C.I.\", x = unit(0.03, \"npc\"), y = unit(0.03, \"npc\"), just = c(\"left\", \"bottom\"), name = \"annotate.text\"); "
				}
				if (addAnnotation == "0") {
					annotate <- ""
				}

				if (length(z) == 0) {
					facet <- ""
				} else {
					facet <- "facet_wrap( ~ z) + "
				}

				if (flipedCoordinates == "1") {
					flip <- "coord_flip() + "
				} else {
					flip <- ""
				}

				if (addJitter == "1") {
					jitter <- "geom_jitter() + "
				} else {
					jitter <- ""
				}

        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(x = factor(x), y = y)) + ",
          geom, facet, flip, jitter,
          xlab, ylab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
          sep=""
        )
        doItAndPrint(command)

        if (annotate == "") GRIDtext <- ""
        else GRIDtext <- paste(
					annotate,
          "grid.gedit(\"annotate.text\", gp=gpar(fontfamily=\"", fontfoamily, "\")); ",
          sep=""
        )
        command <- paste(
          "grid.newpage(); print(.plot1); ",
          GRIDtext,
          ".plot0 <- recordPlot()",
          sep=""
        )
        doItAndPrint(command)

        if (saveFile == "1") {      
          file <- tclvalue(tkgetSaveFile(filetypes=paste(
              "{\"PNG (Portable Network Graphics)\" {\".png\"}}",
              "{\"PDF (Portable Document Format)\" {\".pdf\"}}",
              "{\"JPEG (Joint Photographic Experts Group)\" {\".jpg\"}}"
            ), defaultextension="png", initialfile="GraphSave"
          ))
          if (file == "") return()
          command <- paste("kmg2_ggsave(\"", file, "\", .plot0)", sep="")
          doItAndPrint(command)
        }
        
        doItAndPrint("rm(.df, .plot0, .plot1)")

        activateMenus()
        tkfocus(CommanderWindow())

      }

      # note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="geom_histogram")

      vbbox1$back()
      lbbox1$back()
			tkgrid(
				rbbox1$setframe,
        labelRcmdr(alternateFrame, text="    "),
        cbbox1$setframe, stick="nw")
      tkgrid(alternateFrame, stick="nw")
      tkgrid(labelRcmdr(alternateFrame, text="    "), stick="nw")
      tbbox1$back(4)

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

