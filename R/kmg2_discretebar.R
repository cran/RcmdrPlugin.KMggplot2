kmg2_discretebar <- setRefClass(

  Class = "kmg2_discretebar",

  fields = c("top", "buttonsFrame"),

  methods = list(

    plotDiscretebar = function(title=kmg2_gettextRcmdr("Bar chart for discrete variables")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(Factors(), Factors()),
        titles    = list(
            kmg2_gettextRcmdr("Variable (pick one)"),
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

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top)

      onOK <- function() {

        y            <- getSelection(vbbox1$vbvariables[[1]])
        z            <- getSelection(vbbox1$vbvariables[[2]])

        xlab         <- tclvalue(lbbox1$lbtboxes[[1]]$tvariable)
        ylab         <- tclvalue(lbbox1$lbtboxes[[2]]$tvariable)
        main         <- tclvalue(lbbox1$lbtboxes[[3]]$tvariable)

        fontSize     <- tclvalue(tbbox1$tbfontsizebox$tvariable)
        fontfoamily  <- getSelection(tbbox1$tbfontfoamilybox)
        colourset    <- getSelection(tbbox1$tbcolourbox)
        saveFile     <- tclvalue(tbbox1$tbcheckbox$cbvariables[[1]])

        if (tclvalue(tbbox1$tbthemebox$rbvariable) == "1")
          theme <- "kmg2_theme_gray"
        else if (tclvalue(tbbox1$tbthemebox$rbvariable) == "2")
          theme <- "kmg2_theme_bw"
        else
          theme <- "kmg2_theme_gray"

        closeDialog()
        if (length(y) == 0) {
          errorCondition(recall=plotDiscretebar, message=kmg2_gettextRcmdr("No variable selected."))
          return()
        }

        #######################################################################
        if (nchar(xlab) == 0) {
          xlab <- paste("xlab(NULL) + ", sep="")
        } else if (xlab == "<auto>") {
          xlab <- paste("xlab(\"", y, "\") + ", sep="")
        } else {
          xlab <- paste("xlab(\"", xlab, "\") + ", sep="")
        }
        if (nchar(ylab) == 0) {
          ylab <- paste("ylab(NULL) + ", sep="")
        } else if (ylab == "<auto>") {
          ylab <- paste("ylab(\"Percentage\") + ", sep="")
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
            "y = ", .activeDataSet, "$", y, ")",
            sep=""
          )
        } else {
          command <- paste(
            ".df <- data.frame(",
            "y = ", .activeDataSet, "$", y, ", ",
            "z = ", .activeDataSet, "$", z, ")",
            sep=""
          )
        }
        doItAndPrint(command)
				
				geom <- "geom_bar(width = 1) + "
        scale <- paste(
  						"scale_fill_brewer(palette=\"", colourset, "\") + ",  "scale_y_continuous(formatter = \"percent\") + ", sep="")

				if (length(z) == 0) {
					facet <- ""
				} else {
					facet <- "facet_wrap( ~ z) + "
					fontSize <- "12"
				}

        opts <- " + opts(legend.position = \"none\")"

        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(x = y, y = (..count..)/sum(..count..), fill = y)) + ",
          geom, scale, facet, 
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
        
        doItAndPrint("rm(.df, .plot1)")

        activateMenus()
        tkfocus(CommanderWindow())

      }

      # note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="geom_bar")

      vbbox1$back()
      lbbox1$back()
      tbbox1$back()

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

