kmg2_hist <- setRefClass(

  Class = "kmg2_hist",

  fields = c("top", "buttonsFrame", "alternateFrame"),

  methods = list(

    plotHist = function(title=kmg2_gettextRcmdr("Histogram")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)
      alternateFrame <<- tkframe(top)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(Numeric(), Factors()),
        titles    = list(
            kmg2_gettextRcmdr("Variable (pick one)"),
            kmg2_gettextRcmdr("Stratum variable"))
      )

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
        top        = top,
        initValues = list(
						"<auto>",
						"<auto>",
						""),
        titles     = list(
            kmg2_gettextRcmdr("Horizontal axis label"),
            kmg2_gettextRcmdr("Vertical axis label"),
            kmg2_gettextRcmdr("Title"))
      )

      lbbox2 <- kmg2_labelboxes$new()
      lbbox2$front(
        top        = alternateFrame,
        initValues = list(
						"<auto>"),
        titles     = list(
            kmg2_gettextRcmdr("No. of bins"))
      )

      rbbox1 <- kmg2_radioboxes$new()
      rbbox1$front(
        top    = alternateFrame,
        labels = list(
            kmg2_gettextRcmdr("Frequency counts"),
            kmg2_gettextRcmdr("Percentages"),
            kmg2_gettextRcmdr("Densities")),
        title  = kmg2_gettextRcmdr("Axis scaling")
      )

      cbbox1 <- kmg2_checkboxes$new()
      cbbox1$front(
        top        = alternateFrame,
        initValues = list("0", "0"),
        labels     = list(
            kmg2_gettextRcmdr("Density estimation"),
            kmg2_gettextRcmdr("Heat map")),
        title      = kmg2_gettextRcmdr("Options")
      )

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top)

      onOK <- function() {

        x            <- getSelection(vbbox1$vbvariables[[1]])
        z            <- getSelection(vbbox1$vbvariables[[2]])

        xlab         <- tclvalue(lbbox1$lbtboxes[[1]]$tvariable)
        ylab         <- tclvalue(lbbox1$lbtboxes[[2]]$tvariable)
        main         <- tclvalue(lbbox1$lbtboxes[[3]]$tvariable)

        nbins        <- tclvalue(lbbox2$lbtboxes[[1]]$tvariable)

        scalingType  <- tclvalue(rbbox1$rbvariable)

        densityPlot  <- tclvalue(cbbox1$cbvariables[[1]])
        heatPlot     <- tclvalue(cbbox1$cbvariables[[2]])

        fontSize     <- tclvalue(tbbox1$tbfontsizebox$tvariable)
        fontfoamily  <- getSelection(tbbox1$tbfontfoamilybox)
        colourset    <- getSelection(tbbox1$tbcolourbox)
        saveFile     <- tclvalue(tbbox1$tbcheckbox$cbvariables[[1]])

        if (tclvalue(tbbox1$tbthemebox$rbvariable) == "1")
          theme <- "theme_gray"
        else if (tclvalue(tbbox1$tbthemebox$rbvariable) == "2")
          theme <- "theme_bw"
        else
          theme <- "theme_gray"

        closeDialog()
        if (length(x) == 0) {
          errorCondition(recall=plotHist, message=kmg2_gettextRcmdr("No variable selected."))
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
        if (nchar(main) == 0) {
          main <- ""
        } else {
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        }

        .activeDataSet <- ActiveDataSet()

        if (length(z) == 0) {
          command <- paste(
            ".df <- data.frame(",
            "x = ", .activeDataSet, "$", x, ")",
            sep=""
          )
        } else {
          command <- paste(
            ".df <- data.frame(",
            "x = ", .activeDataSet, "$", x, ",",
            "z = ", .activeDataSet, "$", z, ")",
            sep=""
          )
        }
        doItAndPrint(command)

        if (nbins == "<auto>" || nbins == "Scott" || nbins == "") {
          nbins <- nclass.scott(.df$x)
        } else if (nbins == "Freedman-Diaconis" || nbins == "FD") {
          nbins <- nclass.FD(.df$x)
        } else if (nbins == "Sturges") {
          nbins <- nclass.Sturges(.df$x)
        } else {
          nbins <- as.numeric(nbins)
					if (is.na(nbins)) {
						nbins <- nclass.scott(.df$x)
					}
        }
        command <- paste(".nbins <- pretty(range(.df$x), n = ", nbins, ", min.n = 1)", sep="")
        doItAndPrint(command)

        if (densityPlot == "1") {
          y <- "..density.."
          density <- "stat_density(mapping = aes(ymax = max(..density..)), geom=\"path\", size = 1, linetype = \"dashed\")  + "
        } else {
          if (scalingType == "1") {
            y <- "..count.."
          } else if (scalingType == "2") {
            y <- "..count../sum(..count..)"
          } else if (scalingType == "3") {
            y <- "..density.."
          } else if (scalingType == "4") {
            y <- "..count.."
          }
          density <- ""
        }

        scale1 <- ""
        if (nchar(ylab) == 0) {
          ylab <- "ylab(NULL) + "
        } else if (ylab == "<auto>") {
          if (y == "..count..") {
            ylab <- "ylab(\"Frequency count\") + "
          } else if (y == "..count../sum(..count..)") {
            ylab <- "ylab(\"Percentage\") + "
            scale1 <- "scale_y_continuous(labels = percent) + "
          } else if (y == "..density..") {
            ylab <- "ylab(\"Density\") + "
          }
        } else {
          ylab <- paste("ylab(\"", ylab, "\") + ", sep="")
        }
        
        opts <- ""
        scale2 <- ""
        if (heatPlot == "1") {
          if (y == "..count../sum(..count..)") {
            scale2 <- ", labels = percent"
          }
          heat <- paste(", aes(fill = ", y, ")", sep="")
          scale2 <- paste(
            "scale_fill_gradient(low = brewer.pal(3, \"", colourset,
            "\")[2], high = brewer.pal(3, \"", colourset, "\")[1]", scale2, ") + ", sep="")
          opts <- "+ opts(legend.position = \"none\")"
        } else {
          heat <- ""
        }

				if (length(z) == 0) {
					facet = ""
				} else {
					facet = "facet_wrap( ~ z) + "
				}

        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(x = x, y = ", y, ")) + ",
          "geom_histogram(breaks = .nbins", heat, ") + ",
          density, scale1, scale2, facet,
          xlab, ylab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
          opts,
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

	      command <- "rm(.df, .nbins, .plot1)"
        doItAndPrint(command)

        activateMenus()
        tkfocus(CommanderWindow())

      }

      # note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="geom_histogram")

      vbbox1$back()
      lbbox1$back()

      e <- c(
        list(lbbox2$setframe),
        list(labelRcmdr(alternateFrame, text="    ")),
        list(cbbox1$setframe),
        list(labelRcmdr(alternateFrame, text="    ")),
        list(rbbox1$setframe)
      )
      kmg2_listRecall(lbbox2$back_list, tkgrid, sticky="nw")
      kmg2_listRecall(e, tkgrid, sticky="nw")
      tkgrid(alternateFrame, stick="nw")
      tkgrid(labelRcmdr(alternateFrame, text="    "), stick="nw")

      tbbox1$back()

      tkgrid(buttonsFrame, stick="w")
      dialogSuffix()

    }

  )
)
