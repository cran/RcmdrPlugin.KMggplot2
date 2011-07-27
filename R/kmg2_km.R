kmg2_km <- setRefClass(

  Class = "kmg2_km",

  fields = c("top", "buttonsFrame", "alternateFrame"),

  methods = list(

    plotKM = function(title=kmg2_gettextRcmdr("Kaplan-Meier plot")) {

      # note: The initializeDialog() generates "top"
      initializeDialog(window=top, title=title)
      alternateFrame <<- tkframe(top)

      vbbox1 <- kmg2_variablesbox$new()
      vbbox1$front(
        top       = top, 
        types     = list(Numeric(), Numeric(), Factors()),
        titles    = list(
            kmg2_gettextRcmdr("Time variable"),
            kmg2_gettextRcmdr("Event variable (0=censor, 1=event)"),
            kmg2_gettextRcmdr("Stratum variable"))
      )

      lbbox1 <- kmg2_labelboxes$new()
      lbbox1$front(
        top        = top,
        initValues = list("Time from entry", "Proportion of survival", "<auto>", ""),
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
            kmg2_gettextRcmdr("None"),
            kmg2_gettextRcmdr("Inside"),
            kmg2_gettextRcmdr("Outside")),
        title  = kmg2_gettextRcmdr("No. at risk")
      )

      lbbox2 <- kmg2_labelboxes$new()
      lbbox2$front(
        top        = alternateFrame,
        initValues = list("4"),
        titles     = list(
            kmg2_gettextRcmdr("Tick count"))
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

        tickCount    <- tclvalue(lbbox2$lbtboxes[[1]]$tvariable)

        natriskValue <- tclvalue(rbbox1$rbvariable)

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
        if (length(x) == 0) {
          errorCondition(recall=plotKM, message=kmg2_gettextRcmdr("No Time variable selected."))
          return()
        }
        if (length(y) == 0) {
          errorCondition(recall=plotKM, message=kmg2_gettextRcmdr("No Event variables selected."))
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
            "time=", .activeDataSet, "$", x, ",",
            "surv=", .activeDataSet, "$", y, ",",
            "trt=factor(\"No. at risk\"))",
            sep=""
          )
        } else {
          command <- paste(
            ".df <- data.frame(",
            "time=", .activeDataSet, "$", x, ",",
            "surv=", .activeDataSet, "$", y, ",",
            "trt=",  .activeDataSet, "$", z, ")",
            sep=""
          )
        }
        doItAndPrint(command)
        strataNum <- length(unique(.df$trt))

        command <- ".fit <- survfit(Surv(time=time, event=surv, type=\"right\") ~ trt, .df)"
        response <- tryCatch({
						.fit <- survfit(Surv(time=time, event=surv, type="right") ~ trt, .df)
						""
					}, error = function(ex) {
	  				tclvalue(RcmdrTkmessageBox(message=kmg2_gettextRcmdr("Estimation failed in survfit().  This data may be inappropriate."), title=kmg2_gettextRcmdr("Error"),
	  				icon="error", type="ok", default="ok"))
					}
				)
				if (response == "ok") {
          doItAndPrint("rm(.df)")
					return()
				}
				doItAndPrint(command)

        if (strataNum == 1) {
          command <- ".fit$strata <- rep(factor(.df$trt[1]), nrow(.df))"
        } else {
          command <- ".fit$strata <- rep(unique(.df$trt), .fit$strata)"
        }
        doItAndPrint(command)

        command <- paste(
          ".fit <- data.frame(",
            "time = .fit$time, surv = .fit$surv, strata = .fit$strata,",
            "nrisk = .fit$n.risk, nevent = .fit$n.event, ncensor= .fit$n.censor,",
            "stderr = .fit$std.err, upper = .fit$upper, lower = .fit$lower",
          ")",
          sep=""
        )
        doItAndPrint(command)

        command <- ".a <- unique(.fit$strata)"
        doItAndPrint(command)
        
        command <- paste(
          "for (i in 1:(length(.a))) {",
            ".maxnrisk <- max(subset(.fit, strata==.a[i])$nrisk);",
            ".fit[dim(.fit)[1]+1,] <- list(0, 1, .a[i], .maxnrisk, 0, NA, NA, NA, NA)",
          "}",
          sep=""
        )
        doItAndPrint(command)

        command <- ".cens <- subset(.fit, ncensor==1)"
        doItAndPrint(command)

        command <- paste(".n_majortick <- ", tickCount, "-1", sep="")
        doItAndPrint(command)

        command <- ".by <- signif((max(.fit$time)+.n_majortick/2)/.n_majortick, -round(log10(max(.fit$time)), 0))"
        doItAndPrint(command)

        if (natriskValue == "2" || natriskValue == "3") {
          command <- paste(
            ".natrisk <- by(",
            ".fit, .fit$strata, ",
            "function(x, seq) {",
            "x <- sort_df(x, x$surv); .natrisk <- NULL;",
            "for (i in (1:length(seq))) {",
            "for (j in (1:length(x$surv))) {",
            "if (x$time[j] <= seq[i]) {",
            ".natrisk[i] <- x$nrisk[j] - x$nevent[j]; ",
            "if (sum(x$nevent[(j:length(x$surv))]) == 0) .natrisk[i] <- x$nrisk[j] - x$nevent[j] - x$ncensor[j];",
            "}}};",
            "return(.natrisk)",
            "}, seq(0, .by * .n_majortick, by = .by)",
            ")",
            sep=""
          )
          doItAndPrint(command)
          
          command <- ".m <- dim(.natrisk)"
          doItAndPrint(command)
          
          command <- ".label <- unlist(.natrisk, recursive = FALSE)"
          doItAndPrint(command)
          
          command <- ".x <- rep(seq(0, .by * .n_majortick, by = .by), .m)"
          doItAndPrint(command)
          
          command <- ".y <- NULL; .group <- NULL; .pmax <- 0.05+(.m-1)*0.05;"
          doItAndPrint(command)
          
          command <- paste(
            "for (i in 1:.m) { ",
            ".y <- append(.y, rep(.pmax-(i-1)*0.05, .n_majortick+1)); ",
            ".group <- append(.group, rep(names(.natrisk)[i], .n_majortick+1))",
            "}",
            sep=""
          )
          doItAndPrint(command)
          
          command <- ".natrisk <- data.frame(label=.label, x=.x, y=.y, group=.group)"
          doItAndPrint(command)
          
          if (natriskValue == "3") {
            if (strataNum == 1) {
              command <- paste(
                ".natrisk$y <- rep(0.5, nrow(.natrisk)); ",
                ".natvaxis <- subset(.natrisk, x == 0)", sep="")
            } else {
              command <- paste(
                ".natrisk$y <- ",
                "(-0.8 / (min(.natrisk$y) - max(.natrisk$y))) * ",
                ".natrisk$y + (0.1 + 0.8 * min(.natrisk$y) / ",
                "(min(.natrisk$y) - max(.natrisk$y))); ",
                ".natvaxis <- subset(.natrisk, x == 0)", sep=""
              )
            }
            doItAndPrint(command)
            nriskcommand <- ""
          } else {
            nriskcommand <- "geom_text(data=.natrisk, aes(y=y, x=x, label=label, colour=factor(group)), legend=FALSE) + "
          }
        } else {
          nriskcommand <- ""
        }
        
        scale <- paste(
          "scale_x_continuous(breaks = seq(0, .by * .n_majortick, by = .by), limits=c(0, .by * .n_majortick)) + ", 
          "scale_y_continuous(limits=c(0, 1)) + ",
          "scale_colour_brewer(palette=\"", colourset, "\") + ",
          sep=""
        )
        if (strataNum == 1) {
          opts <- " + opts(legend.position = \"none\")"
					zlab <- ""
        } else {
          opts <- " + opts(legend.justification = c(1, 1))"
          if (nchar(zlab) == 0) {
            opts <- paste(opts, " + opts(legend.title = theme_blank())", sep="")
          } else if (zlab == "<auto>") {
  					zlab <- paste("labs(colour = \"", z, "\") + ", sep="")
          } else {
  					zlab <- paste("labs(colour = \"", zlab, "\") + ", sep="")
          }
          if (main != "") {
            opts <- paste(opts, " + opts(legend.position = c(0.95, 0.9))", sep="")
          }
        }

        geom <- "geom_step(size=1.5) + geom_linerange(data=.cens, aes(x=time, ymin=surv, ymax=surv+0.02), size=1.5) + "

        command <- paste(
          ".plot1 <- ggplot(data=.fit, aes(x=time, y=surv, colour=strata)) + ",
          geom, scale, nriskcommand,
          xlab, ylab, zlab, main,
          theme, "(", fontSize, ", \"", fontfoamily, "\")",
          opts,
          sep=""
        )
        doItAndPrint(command)

        geom <- "geom_text() + "
        if (natriskValue == "3") {
          command <- paste(
            ".plot2 <- ggplot(data=.natrisk, aes(y=y, x=x, label=label, colour=factor(group))) +",
            geom, scale, 
            theme, "_natrisk(", fontSize, ", \"", fontfoamily, "\")",
            sep=""
          )
          doItAndPrint(command)

          scale <- paste(
            "scale_x_continuous(limits=c(-5, 5)) + ",
            "scale_y_continuous(limits=c(0, 1)) + ",
            "scale_colour_brewer(palette=\"", colourset, "\") + ",
            sep=""
          )
          command <- paste(
            ".plot3 <- ggplot(data=.natvaxis, aes(y=y, x=x, label=group, colour=factor(group))) +",
            geom, scale,
            theme, "_natrisk21(", fontSize, ", \"", fontfoamily, "\")",
            sep=""
          )
          doItAndPrint(command)
        }
        
        if (natriskValue == "3") {
          tableMargin <- length(unique(.fit$strata)) + 2
          command <- paste(
            "grid.newpage(); ",
            "pushViewport(viewport(layout = grid.layout(2, 2,",
            "heights = unit(c(1, ", tableMargin, "), c(\"null\", \"lines\")),",
            "widths  = unit(c(7, 1), c(\"lines\", \"null\"))))); ",
            "print(.plot1, vp=viewport(layout.pos.row=1, layout.pos.col=1:2)); ",
            "print(.plot2, vp=viewport(layout.pos.row=2, layout.pos.col=1:2)); ",
            "print(.plot3, vp=viewport(layout.pos.row=2, layout.pos.col=1  )); ",
            "grid.gedit(\"GRID.text\", gp=gpar(fontfamily=\"", fontfoamily, "\")); ",
            ".plot0 <- recordPlot()",
            sep=""
          )
        } else {
          if (natriskValue == "1") GRIDtext <- ""
          else GRIDtext <- paste(
            "grid.gedit(\"GRID.text\", gp=gpar(fontfamily=\"", fontfoamily, "\")); ",
            sep=""
          )
          command <- paste(
            "grid.newpage(); print(.plot1); ",
            GRIDtext,
            ".plot0 <- recordPlot()",
            sep=""
          )
        }
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
        
        if (natriskValue == "3") {
          doItAndPrint("rm(.df, .fit, .a, i, .cens, .n_majortick, .by, .natrisk, .m, .label, .x, .y, .group, .pmax, .maxnrisk, .natvaxis, .plot0, .plot1, .plot2, .plot3)")
        } else if (natriskValue == "2") {
          doItAndPrint("rm(.df, .fit, .a, i, .cens, .n_majortick, .by, .natrisk, .m, .label, .x, .y, .group, .pmax, .maxnrisk, .plot0, .plot1)")
        } else {
          doItAndPrint("rm(.df, .fit, .a, i, .cens, .n_majortick, .by, .maxnrisk, .plot0, .plot1)")
        }

        activateMenus()
        tkfocus(CommanderWindow())

      }

      # note: The OKCancelHelp() generates "buttonsFrame"
      OKCancelHelp(helpSubject="survfit")

      vbbox1$back()
      lbbox1$back()

      e <- c(
        list(lbbox2$setframe),
        list(labelRcmdr(alternateFrame, text="    ")),
        list(rbbox1$setframe)
      )
      kmg2_listRecall(lbbox2$back_list, tkgrid, sticky="nw")
      kmg2_listRecall(e, tkgrid, sticky="nw")
      tkgrid(alternateFrame, stick="nw")
      tkgrid(labelRcmdr(alternateFrame, text="    "), stick="nw")

      tbbox1$back()

      tkgrid(buttonsFrame, stick="nw")
      dialogSuffix()

    }

  )
)

