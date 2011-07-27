kmg2_qq <- setRefClass(

  Class = "kmg2_qq",

  fields = c("top", "buttonsFrame", "alternateFrame"),

  methods = list(

    plotQQ = function(title=kmg2_gettextRcmdr("Q-Q plot")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)
      alternateFrame <<- tkframe(top)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(Numeric()),
        titles    = list(
            kmg2_gettextRcmdr("Y variable (pick one)"))
      )

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
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
        top    = alternateFrame,
        labels = list(
            kmg2_gettextRcmdr("Normal distribution"),
            kmg2_gettextRcmdr("Log-normal distribution"),
            kmg2_gettextRcmdr("Beta distribution"),
            kmg2_gettextRcmdr("Exponential distribution"),
            kmg2_gettextRcmdr("Gamma distribution"),
            kmg2_gettextRcmdr("Weibull distribution"),
            kmg2_gettextRcmdr("Other distribution")),
        title  = kmg2_gettextRcmdr("Distribution")
      )

      lbbox2 <- kmg2_labelboxes$new()
      lbbox2$front(
        top        = alternateFrame,
        initValues = list(
						"distribution = stats::qnorm, dparams = list(mean = 0, sd = 1)"),
        titles     = list(
            kmg2_gettextRcmdr("Parameters for other distribution"))
      )

      tbbox1 <- kmg2_toolbox$new()
      tbbox1$front(top, showcolourbox = FALSE)

      onOK <- function() {

        y            <- getSelection(vbbox1$vbvariables[[1]])

        xlab         <- tclvalue(lbbox1$lbtboxes[[1]]$tvariable)
        ylab         <- tclvalue(lbbox1$lbtboxes[[2]]$tvariable)
        main         <- tclvalue(lbbox1$lbtboxes[[3]]$tvariable)

        distType     <- tclvalue(rbbox1$rbvariable)
        distParms    <- tclvalue(lbbox2$lbtboxes[[1]]$tvariable)

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
        if (length(y) == 0) {
          errorCondition(recall=plotQQ, message=kmg2_gettextRcmdr("No y variable selected."))
          return()
        }

        #######################################################################
        if (nchar(xlab) == 0) {
          xlab <- paste("xlab(NULL) + ", sep="")
        } else if (xlab == "<auto>") {
          xlab <- paste("xlab(\"Theoretical quantile\") + ", sep="")
        } else {
          xlab <- paste("xlab(\"", xlab, "\") + ", sep="")
        }
        if (nchar(ylab) == 0) {
          ylab <- paste("ylab(NULL) + ", sep="")
        } else if (ylab == "<auto>") {
          ylab <- paste("ylab(\"Sample quantile\") + ", sep="")
        } else {
          ylab <- paste("ylab(\"", ylab, "\") + ", sep="")
        }

        .activeDataSet <- ActiveDataSet()

        command <- paste(
          ".df <- data.frame(",
          "y = ", .activeDataSet, "$", y, ")",
          sep=""
        )
        doItAndPrint(command)
				
				if (distType == "1") {
					command <- paste(
						".objf <- function(p, y) {",
	  				"-sum(stats::dnorm(y, mean = p[1], sd = p[2], log = TRUE))",
						"}",
					sep="")
	        doItAndPrint(command)

					command <- ".est <- optim(c(1, 1), .objf, y = .df$y)"
	        doItAndPrint(command)

					distParms <- "distribution = stats::qnorm, dparams = list(mean = .est$par[1], sd = .est$par[2])"
					
					if (main == "<auto>") {
						main <- paste(
							"Theoretical: qnorm(mean = ", round(.est$par[1], 1),
							", sd = ", round(.est$par[2], 1), ")",
							sep=""
						)
					}

        } else if (distType == "2") {
					if (any(.df$y <= 0)) {
	  				response <- tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("The log-normal distribution defined on the interval (0, +Inf)."), title=kmg2_gettextRcmdr("Error"),
	  				icon="error", type="ok", default="ok"))
	  				if (response == "ok") {
	            command <- "rm(.df)"
	            doItAndPrint(command)
	  					return()
	  				}
					}

					command <- paste(
						".objf <- function(p, y) {",
	  				"-sum(stats::dlnorm(y, meanlog = p[1], sdlog = p[2], log = TRUE))",
						"}",
					sep="")
	        doItAndPrint(command)

					command <- ".est <- optim(c(1, 1), .objf, y = .df$y)"
	        doItAndPrint(command)

					distParms <- "distribution = stats::qlnorm, dparams = list(meanlog = .est$par[1], sdlog = .est$par[2])"

					if (main == "<auto>") {
						main <- paste(
							"Theoretical: qlnorm(meanlog = ", round(.est$par[1], 1),
							", sdlog = ", round(.est$par[2], 1), ")",
							sep=""
						)
					}

        } else if (distType == "3") {
					if (any(.df$y > 1 | .df$y < 0)) {
	  				response <- tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("The beta distribution defined on the interval [0, 1]."), title=kmg2_gettextRcmdr("Error"),
	  				icon="error", type="ok", default="ok"))
	  				if (response == "ok") {
	            command <- "rm(.df)"
	            doItAndPrint(command)
	  					return()
	  				}
					}

					command <- paste(
						".objf <- function(p, y) {",
	  				"-sum(stats::dbeta(y, shape1 = p[1], shape2 = p[2], log = TRUE))",
						"}",
					sep="")
	        doItAndPrint(command)

					command <- ".est <- optim(c(1, 1), .objf, y = .df$y)"
	        doItAndPrint(command)

					distParms <- "distribution = stats::qbeta, dparams = list(shape1 = .est$par[1], shape2 = .est$par[2])"

					if (main == "<auto>") {
						main <- paste(
							"Theoretical: qbeta(shape1 = ", round(.est$par[1], 1),
							", shape2 = ", round(.est$par[2], 1), ")",
							sep=""
						)
					}

        } else if (distType == "4") {
					if (any(.df$y < 0)) {
	  				response <- tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("The exponential distribution defined on the interval [0, +Inf)."), title=kmg2_gettextRcmdr("Error"),
	  				icon="error", type="ok", default="ok"))
	  				if (response == "ok") {
	            command <- "rm(.df)"
	            doItAndPrint(command)
	  					return()
	  				}
					}

					command <- paste(
						".objf <- function(p, y) {",
	  				"-sum(stats::dexp(y, rate = p[1], log = TRUE))",
						"}",
					sep="")
	        doItAndPrint(command)

					command <- ".est <- optim(1, .objf, y = .df$y)"
	        doItAndPrint(command)

					distParms <- "distribution = stats::qexp, dparams = list(rate = .est$par[1])"

					if (main == "<auto>") {
						main <- paste(
							"Theoretical: qexp(rate = ", round(.est$par[1], 1), ")",
							sep=""
						)
					}

        } else if (distType == "5") {
					if (any(.df$y < 0)) {
	  				response <- tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("The gamma distribution defined on the interval [0, +Inf)."), title=kmg2_gettextRcmdr("Error"),
	  				icon="error", type="ok", default="ok"))
	  				if (response == "ok") {
	            command <- "rm(.df)"
	            doItAndPrint(command)
	  					return()
	  				}
					}

					command <- paste(
						".objf <- function(p, y) {",
	  				"-sum(stats::dgamma(y, shape=p[1], scale=p[2], log = TRUE))",
						"}",
					sep="")
	        doItAndPrint(command)

					command <- ".est <- optim(c(1, 1), .objf, y = .df$y)"
	        doItAndPrint(command)

					distParms <- "distribution = stats::qgamma, dparams = list(shape = .est$par[1], scale = .est$par[2])"

					if (main == "<auto>") {
						main <- paste(
							"Theoretical: qgamma(shape = ", round(.est$par[1], 1),
							", scale = ", round(.est$par[2], 1), ")",
							sep=""
						)
					}

        } else if (distType == "6") {
					if (any(.df$y < 0)) {
	  				response <- tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("The weibull distribution defined on the interval [0, +Inf)."), title=kmg2_gettextRcmdr("Error"),
	  				icon="error", type="ok", default="ok"))
	  				if (response == "ok") {
	            command <- "rm(.df)"
	            doItAndPrint(command)
	  					return()
	  				}
					}

					command <- paste(
						".objf <- function(p, y) {",
	  				"-sum(stats::dweibull(y, shape=p[1], scale=p[2], log = TRUE))",
						"}",
					sep="")
	        doItAndPrint(command)

					command <- ".est <- optim(c(1, 1), .objf, y = .df$y)"
	        doItAndPrint(command)

					distParms <- "distribution = stats::qweibull, dparams = list(shape = .est$par[1], scale = .est$par[2])"

					if (main == "<auto>") {
						main <- paste(
							"Theoretical: qweibull(shape = ", round(.est$par[1], 1),
							", scale = ", round(.est$par[2], 1), ")",
							sep=""
						)
					}

				} else {

					if (main == "<auto>") {
						main <- paste(
							"Theoretical: ", distParms,
							sep=""
						)
					}

        }

        if (distType != "7" && (is.na(match(".est", ls(envir = .GlobalEnv, all.names=TRUE))) || .est$convergence != 0)) {
  				response <- tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("Parameter estimation failed."),
  				icon="error", type="ok", default="ok"))
  				if (response == "ok") {
            command <- "rm(.df, .objf, .est)"
            doItAndPrint(command)
  					return()
  				}
        }

        if (nchar(main) == 0) {
					main <- ""
        } else {
          main <- paste("opts(title = \"", main, "\") + ", sep="")
        }

				geom <- paste("stat_qq(", distParms, ") + ", sep="")
				
        command <- paste(
          ".plot1 <- ggplot(data = .df, aes(sample = y)) + ",
          geom,
          xlab, ylab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
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

				if (distType == "7") {
  	      doItAndPrint("rm(.df, .plot1)")
				} else {
	        doItAndPrint("rm(.df, .objf, .est, .plot1)")
				}
        
        activateMenus()
        tkfocus(CommanderWindow())

      }

      # note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="Distributions")

      vbbox1$back()
      lbbox1$back()

      e <- c(
        list(rbbox1$setframe),
        list(labelRcmdr(alternateFrame, text="    ")),
        list(lbbox2$setframe)
      )
      kmg2_listRecall(lbbox2$back_list, tkgrid, sticky="nw")
      kmg2_listRecall(e, tkgrid, sticky="nw")
      tkgrid(alternateFrame, stick="nw")
      tkgrid(labelRcmdr(alternateFrame, text="    "), stick="nw")

      tbbox1$back(4)

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

