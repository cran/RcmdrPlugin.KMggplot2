kmg2_scattermatrix <- setRefClass(

  Class = "kmg2_scattermatrix",

  fields = c("top", "buttonsFrame"),

  methods = list(

    plotScattermatrix = function(title=kmg2_gettextRcmdr("Scatter matrix")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(Numeric()),
        titles    = list(
            kmg2_gettextRcmdr("Select variables (three or more)")),
        modes     = list("multiple")
      )

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
        top        = top,
        initValues = list(""),
        titles     = list(
            kmg2_gettextRcmdr("Title"))
      )

      rbbox1 <- kmg2_radioboxes$new()
      rbbox1$front(
        top    = top,
        labels = list(
            kmg2_gettextRcmdr("None"),
            kmg2_gettextRcmdr("Smoosing with C.I. (linear regression)"),
            kmg2_gettextRcmdr("Smoosing without C.I. (linear regression)"),
            kmg2_gettextRcmdr("Smoosing with C.I. (loess or gam)"),
            kmg2_gettextRcmdr("Smoosing without C.I. (loess or gam)")),
        title  = kmg2_gettextRcmdr("Smoosing type")
      )

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top, showcolourbox = FALSE, fontSize = "12")

      onOK <- function() {

        x            <- getSelection(vbbox1$vbvariables[[1]])

        main         <- tclvalue(lbbox1$lbtboxes[[1]]$tvariable)

        smoothType   <- tclvalue(rbbox1$rbvariable)

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
        if (length(x) < 3) {
          errorCondition(recall=plotScattermatrix, message=kmg2_gettextRcmdr("Fewer than 3 variable selected."))
          return()
        }

        #######################################################################
        if (nchar(main) == 0) {
          main <- ""
        } else {
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        }

        .activeDataSet <- ActiveDataSet()

				
				command <- ".varlist <- c("
				for (i in 1:length(x)) {
					command <- paste(command, "\"", x[i], "\"", sep="")
          if (i != length(x)) {
						command <- paste(command, ", ", sep="")
					}
				}
				command <- paste(command, ")", sep="")
        doItAndPrint(command)

				command <- paste(".df0 <- ", .activeDataSet, "[.varlist]", sep="")
        doItAndPrint(command)
				
				command <- ".df <- NULL"
        doItAndPrint(command)

				command <- paste(
					"for(i in 1:length(.varlist)) {",
					"for(j in 1:length(.varlist)) {",
					"if (i != j) {",
					".df <- rbind(.df, data.frame(x = .df0[[.varlist[i]]], y = .df0[[.varlist[j]]], c = i, r = j))",
					"}",
					"}",
					"}",
					sep=""
				)
        doItAndPrint(command)

				command <- ".scatterMatrix.label <- function(variable, value) .varlist[value]"
        doItAndPrint(command)

				geom <- "facet_grid(r ~ c, labeller=\".scatterMatrix.label\", scales = \"free\") + geom_point() + "

				if (smoothType == "1") {
					smooth <- ""
				} else if (smoothType == "2") {
					smooth <- paste("stat_smooth(method = \"lm\") + ", sep="")
				} else if (smoothType == "3") {
					smooth <- paste("stat_smooth(method = \"lm\", se = FALSE) + ", sep="")
				} else if (smoothType == "4") {
					smooth <- paste("stat_smooth() + ", sep="")
				} else if (smoothType == "5") {
					smooth <- paste("stat_smooth(se = FALSE) + ", sep="")
				}

        opts <- " + opts(axis.title.x = theme_blank(), axis.title.y = theme_blank())"

        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(x = x, y = y)) + ",
          geom, smooth,
          main,
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
          doItAndPrint("rm(.varlist, .df0, .df, .scatterMatrix.label, .plot1)")
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
        
        doItAndPrint("rm(.varlist, .df0, .df, .scatterMatrix.label, .plot1)")
				
        activateMenus()
        tkfocus(CommanderWindow())

      }

      # note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="stat_smooth")

      vbbox1$back()
      lbbox1$back()
      rbbox1$back()
      tbbox1$back(4)

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

