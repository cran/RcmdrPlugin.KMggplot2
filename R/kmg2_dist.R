kmg2_dist <- setRefClass(

  Class = "kmg2_dist",

  fields = c("top", "buttonsFrame"),

  methods = list(

    plotContinuous = function(distName, parmInits, parmTitles, title=kmg2_gettextRcmdr("Plot distiribution")) {

			# note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)

      if (length(parmInits) != length(parmTitles)) {
        error("length(parmInits) != length(parmTitles)")
      } else {
        parmLength <- length(parmInits)
      }

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
        top        = top,
		    initValues = parmInits,
		    titles     = parmTitles
      )

      lbbox2 <- kmg2_labelboxes$new()
      lbbox2$front(
        top        = top,
		    initValues = list(
						"<auto>",
						"<auto>",
						"<auto>"),
		    titles     = list(
		        kmg2_gettextRcmdr("Horizontal axis label"),
		        kmg2_gettextRcmdr("Vertical axis label"),
		        kmg2_gettextRcmdr("Title"))
      )

      rbbox1 <- kmg2_radioboxes$new()
      rbbox1$front(
        top    = top,
        labels = list(
		        kmg2_gettextRcmdr("Plot density function"),
		        kmg2_gettextRcmdr("Plot distribution function")),
        title =  kmg2_gettextRcmdr("Function type")
      )

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top, showcolourbox = FALSE)

      onOK <- function() {

        fontSize    <- tclvalue(tbbox1$tbfontsizebox$tvariable)
		    fontfoamily <- getSelection(tbbox1$tbfontfoamilybox)
        saveFile    <- tclvalue(tbbox1$tbcheckbox$cbvariables[[1]])

        if (tclvalue(tbbox1$tbthemebox$rbvariable) == "1")
          theme <- "theme_gray"
        else if (tclvalue(tbbox1$tbthemebox$rbvariable) == "2")
          theme <- "theme_bw"
        else
					theme <- "theme_gray"
				
				parmValues  <- lapply(1:parmLength,
          function(i, lbbox1) tclvalue(lbbox1$lbtboxes[[i]]$tvariable), lbbox1)
				parmValuesList <- ""
				for (i in 1:parmLength) {
					parmValuesList <- paste(parmValuesList, ", ", parmValues[i], sep="")
				}
        funcType    <- tclvalue(rbbox1$rbvariable)

        xlab        <- tclvalue(lbbox2$lbtboxes[[1]]$tvariable)
        ylab        <- tclvalue(lbbox2$lbtboxes[[2]]$tvariable)
        main        <- tclvalue(lbbox2$lbtboxes[[3]]$tvariable)

        closeDialog()

        #######################################################################
        if (nchar(main) == 0) {
						main <- ""
        } else if (main == "<auto>") {
	        if (funcType == "1") {
						main <- paste("d", distName, "(x", parmValuesList, ")", sep="")
          } else {
						main <- paste("p", distName, "(x", parmValuesList, ")", sep="")
					}
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        } else {
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        }
        if (nchar(xlab) == 0) {
          xlab <- paste("xlab(NULL) + ", sep="")
        } else if (xlab == "<auto>") {
          xlab <- paste("xlab(\"x\") + ", sep="")
				} else {
          xlab <- paste("xlab(\"", xlab, "\") + ", sep="")
        }
        if (nchar(ylab) == 0) {
          ylab <- paste("ylab(NULL) + ", sep="")
        } else if (ylab == "<auto>") {
	        if (funcType == "1") {
						ylab <- "ylab(\"Density\") + "
          } else {
						ylab <- "ylab(\"Cumulative Probability\") + "
					}
				} else {
          ylab <- paste("ylab(\"", ylab, "\") + ", sep="")
        }

				command <- paste(
          "q", distName, "(c(0.002, 1-0.002)", parmValuesList, ")", sep="")
				range <- eval(parse(text=command))

        command <- paste(
          ".x  <- seq(", range[1], ", ", range[2], ", length.out = 100)", sep="")
        doItAndPrint(command)
        
        if (funcType == "1") {
					command <- paste(
						".df <- data.frame(x = .x, y = d", distName, "(.x", parmValuesList, "))", sep="")
        } else {
					command <- paste(
						".df <- data.frame(x = .x, y = p", distName, "(.x", parmValuesList, "))", sep="")
        }
        doItAndPrint(command)

				geom <- "geom_line(size = 1.5) + "

        command <- paste(
          ".plot1 <- ggplot(.df, aes(x = x, y = y)) + ",
          geom,
          xlab, ylab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
          sep = ""
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

        doItAndPrint("rm(.x, .df, .plot1)")

        activateMenus()
        tkfocus(CommanderWindow())

      }

			# note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="Distributions")
      lbbox1$back()
      lbbox2$back()
      rbbox1$back()
      tbbox1$back(4)

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    },

    plotDiscrete = function(distName, parmInits, parmTitles, title=kmg2_gettextRcmdr("Plot distiribution")) {

			# note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)

      if (length(parmInits) != length(parmTitles)) {
        error("length(parmInits) != length(parmTitles)")
      } else {
        parmLength <- length(parmInits)
      }

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
        top        = top,
		    initValues = parmInits,
		    titles     = parmTitles
      )

      lbbox2 <- kmg2_labelboxes$new()
      lbbox2$front(
        top        = top,
		    initValues = list(
						"<auto>",
						"<auto>",
						"<auto>"),
		    titles     = list(
		        kmg2_gettextRcmdr("Horizontal axis label"),
		        kmg2_gettextRcmdr("Vertical axis label"),
		        kmg2_gettextRcmdr("Title"))
      )

      rbbox1 <- kmg2_radioboxes$new()
      rbbox1$front(
        top    = top,
        labels = list(
		        kmg2_gettextRcmdr("Plot mass function"),
		        kmg2_gettextRcmdr("Plot distribution function")),
        title =  kmg2_gettextRcmdr("Function type")
      )

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top, showcolourbox = FALSE)

      onOK <- function() {

        fontSize    <- tclvalue(tbbox1$tbfontsizebox$tvariable)
		    fontfoamily <- getSelection(tbbox1$tbfontfoamilybox)
        saveFile    <- tclvalue(tbbox1$tbcheckbox$cbvariables[[1]])

        if (tclvalue(tbbox1$tbthemebox$rbvariable) == "1")
          theme <- "theme_gray"
        else if (tclvalue(tbbox1$tbthemebox$rbvariable) == "2")
          theme <- "theme_bw"
        else
					theme <- "theme_gray"
				
				parmValues  <- lapply(1:parmLength,
          function(i, lbbox1) tclvalue(lbbox1$lbtboxes[[i]]$tvariable), lbbox1)
				parmValuesList <- ""
				for (i in 1:parmLength) {
					parmValuesList <- paste(parmValuesList, ", ", parmValues[i], sep="")
				}
        funcType    <- tclvalue(rbbox1$rbvariable)

        xlab        <- tclvalue(lbbox2$lbtboxes[[1]]$tvariable)
        ylab        <- tclvalue(lbbox2$lbtboxes[[2]]$tvariable)
        main        <- tclvalue(lbbox2$lbtboxes[[3]]$tvariable)

        closeDialog()

        #######################################################################
        if (nchar(main) == 0) {
						main <- ""
        } else if (main == "<auto>") {
	        if (funcType == "1") {
						main <- paste("d", distName, "(x", parmValuesList, ")", sep="")
          } else {
						main <- paste("p", distName, "(x", parmValuesList, ")", sep="")
					}
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        } else {
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        }
        if (nchar(xlab) == 0) {
          xlab <- paste("xlab(NULL) + ", sep="")
        } else if (xlab == "<auto>") {
          xlab <- paste("xlab(\"x\") + ", sep="")
				} else {
          xlab <- paste("xlab(\"", xlab, "\") + ", sep="")
        }
        if (nchar(ylab) == 0) {
          ylab <- paste("ylab(NULL) + ", sep="")
        } else if (ylab == "<auto>") {
	        if (funcType == "1") {
						ylab <- "ylab(\"Probability mass\") + "
          } else {
						ylab <- "ylab(\"Cumulative Probability\") + "
					}
				} else {
          ylab <- paste("ylab(\"", ylab, "\") + ", sep="")
        }

				command <- paste(
          "q", distName, "(c(0.001, 1-0.001)", parmValuesList, ")", sep="")
				range <- eval(parse(text=command))

        command <- paste(
          ".x  <- ", range[1], ":", range[2], sep="")
        doItAndPrint(command)
        
        if (funcType == "1") {
					command <- paste(
						".df <- data.frame(x = .x, y = d", distName, "(.x", parmValuesList, "))", sep="")
        } else {
					command <- paste(
						".df <- data.frame(x = .x, y = p", distName, "(.x", parmValuesList, "))", sep="")
        }
        doItAndPrint(command)

				if (funcType == "1") {
					geom <- "geom_bar(stat = \"identity\") + "
				} else {
					geom <- "geom_step(size=1.5) + "
				}
        command <- paste(
          ".plot1 <- ggplot(.df, aes(x = x, y = y)) + ",
          geom,
          xlab, ylab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
          sep = ""
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

        doItAndPrint("rm(.x, .df, .plot1)")

        activateMenus()
        tkfocus(CommanderWindow())

      }

			# note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="Distributions")

      lbbox1$back()
      lbbox2$back()
      rbbox1$back()
      tbbox1$back(4)

      tkgrid(buttonsFrame, stick="w")
      dialogSuffix()

    }

  )
)
