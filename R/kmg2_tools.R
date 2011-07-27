kmg2_base <- setRefClass(

  Class = "kmg2_base",
  
  fields = c("length", "setframe", "back_list"),

  methods = list(

    back = function(perline = 3) {

      f <- 1
			if (!is.null(back_list)) {
	      for (i in 1:length(back_list)) {
	        if (i == length(back_list)) {
	          kmg2_listRecall(back_list[f:i], tkgrid, sticky="nw")
	        } else if (i %% (perline * 2) == 0) {
	          kmg2_listRecall(back_list[(i - (perline * 2) + 1):i], tkgrid, sticky="nw")
	          tkgrid(labelRcmdr(setframe, text="    "), sticky="nw")
	          f <- i + 1
	        }
	      }
			}

      tkgrid(setframe, sticky="nw")
      tkgrid(labelRcmdr(setframe, text="    "), sticky="nw")

    }

  )
)

kmg2_textbox <- setRefClass(

  Class = "kmg2_textbox",

  fields = c("tvariable"),

  contains = c("kmg2_base"),

  methods = list(

    front = function(top, initialValue, boxwidth, title) {

      setframe <<- tkframe(top)
      tvariable <<- tclVar(initialValue)
      textboxEntry <- ttkentry(setframe, width=boxwidth, textvariable=tvariable)
      tkgrid(labelRcmdr(setframe, text=title, fg="blue"), sticky="nw")
      tkgrid(textboxEntry, sticky="nw")

			back_list <<- as.list(NULL)

    }

  )
)

kmg2_radioboxes <- setRefClass(

  Class = "kmg2_radioboxes",

  fields = c("rbvariable", "rbradioboxes"),

  contains = c("kmg2_base"),

  methods = list(

    front = function(top, labels, title = "", initValue = 1, right.buttons = FALSE) {

      if (length(labels) < initValue) {
        error("length(labels) < initValue")
      } else {
        length <<- length(labels)
      }
      initValues <- 1:length
      setframe <<- tkframe(top)
      rbvariable <<- tclVar(initValue)

      if (title != "") {
        tkgrid(labelRcmdr(setframe, text = title, foreground = "blue"), columnspan = 2, sticky = "w")
      }
      rbradioboxes <<- as.list(NULL)
      for (i in 1:length) {
        rbradioboxes[[i]] <<- ttkradiobutton(setframe, variable = rbvariable, value = initValues[i])
        if (right.buttons) {
          tkgrid(labelRcmdr(setframe, text = labels[[i]], justify = "left"), rbradioboxes[[i]], sticky = "w")
        } else {
          tkgrid(rbradioboxes[[i]], labelRcmdr(setframe, text = labels[[i]], justify = "left"), sticky = "w")
        }
      }

			back_list <<- NULL

    }

  )
)

kmg2_checkboxes <- setRefClass(

  Class = "kmg2_checkboxes",

  fields = c("cbvariables", "cbcheckboxes"),

  contains = c("kmg2_base"),

  methods = list(

    front = function(top, initValues, labels, title = "", right.buttons = FALSE) {

      if (length(initValues) != length(labels)) {
        error("length(initValues) != length(labels)")
      } else {
        length <<- length(initValues)
      }
      setframe <<- tkframe(top)

      if (title != "") {
        tkgrid(labelRcmdr(setframe, text = title, foreground = "blue"), columnspan = 2, sticky = "w")
      }
      cbvariables <<- as.list(NULL)
      cbcheckboxes <<- as.list(NULL)
      for (i in 1:length) {
        cbvariables[[i]] <<- tclVar(initValues[[i]])
        cbcheckboxes[[i]] <<- tkcheckbutton(setframe, variable = cbvariables[[i]])
        if (right.buttons) {
          tkgrid(labelRcmdr(setframe, text = labels[[i]], justify = "left"), cbcheckboxes[[i]], sticky = "w")
        } else {
          tkgrid(cbcheckboxes[[i]], labelRcmdr(setframe, text = labels[[i]], justify = "left"), sticky = "w")
        }
      }

			back_list <<- NULL

    }

  )
)

kmg2_labelboxes <- setRefClass(

  Class = "kmg2_labelboxes",

  fields = c("lbtboxes"),

  contains = c("kmg2_base"),

  methods = list(

    front = function(top, initValues, titles, boxwidth="20") {

      if (length(initValues) != length(titles)) {
        error("length(initValues) != length(titles)")
      } else {
        length <<- length(initValues)
      }
      setframe <<- tkframe(top)

      lbtboxes <<- as.list(NULL)
      for (i in 1:length) {
        lbtboxes[[i]] <<- kmg2_textbox$new()
        lbtboxes[[i]]$front(setframe, initValues[[i]], boxwidth, titles[[i]])
      }

      back_list <<- NULL
      for (i in 1:length) {
        back_list <<- c(back_list, list(lbtboxes[[i]]$setframe))
        if (i < length) {
          back_list <<- c(back_list, list(labelRcmdr(setframe, text="    ")))
        }
      }

    }

  )
)

kmg2_variablesbox <- setRefClass(

  Class = "kmg2_variablesbox",

  fields = c("vbvariables"),

  contains = c("kmg2_base"),

  methods = list(

    front = function(top, types, titles, modes="default") {

      if (length(types) != length(titles)) {
        error("length(types) != length(titles)")
      } else {
        length <<- length(types)
      }
			if (length(modes) == 1 && modes == "default") {
				modes <- lapply(1:length, function(x) "single")
      } else if (length(types) != length(modes)) {
        error("length(types) != length(modes)")
      }
      setframe <<- tkframe(top)

      vbvariables <<- lapply(1:length, function(i, types, titles) {
          variableListBox(
            setframe, unlist(types[i]), selectmode=unlist(modes[i]),
            initialSelection=FALSE, title=unlist(titles[i])
          )
        }, types, titles
      )

      back_list <<- NULL
      for (i in 1:length) {
        back_list <<- c(back_list, list(vbvariables[[i]]$frame))
        if (i < length) {
          back_list <<- c(back_list, list(labelRcmdr(setframe, text="    ")))
        }
      }

    }

  )
)

kmg2_toolbox <- setRefClass(

  Class = "kmg2_toolbox",

  fields = c("tbfontsizebox", "tbfontfoamilybox", "tbcolourbox", "tbcheckbox", "tbthemebox"),

  contains = c("kmg2_base"),

  methods = list(

    front = function(top, showcolourbox = TRUE, fontSize = "25") {

      setframe <<- tkframe(top)

      tbfontsizebox <<- kmg2_textbox$new()
      tbfontsizebox$front(
        top          = setframe,
        initialValue = fontSize,
        boxwidth     = "10", 
        title        = kmg2_gettextRcmdr("Font size")
      )
      
      psfontsname <- names(postscriptFonts())
      tbfontfoamilybox <<- variableListBox(
        parentWindow     = setframe,
        variableList     = psfontsname,
        listHeight       = 5,
        selectmode       = "single",
        initialSelection = 0,
        title            = kmg2_gettextRcmdr("Font family")
      )
      
      if (showcolourbox) {
        brewercolours <- rownames(brewer.pal.info)
        brewercolours <- c("Set1", brewercolours[brewercolours!="Set1"])
        tbcolourbox <<- variableListBox(
          parentWindow     = setframe,
          variableList     = brewercolours,
          listHeight       = 5,
          selectmode       = "single",
          initialSelection = 0,
          title            = kmg2_gettextRcmdr("Colour pattern")
        )
      }

      tbcheckbox <<- kmg2_checkboxes$new()
      tbcheckbox$front(
        top        = setframe,
        initValues = list("savegraph"),
        labels     = list(kmg2_gettextRcmdr("Save graph")),
        title      = kmg2_gettextRcmdr("Graph options")
      )

      tbthemebox <<- kmg2_radioboxes$new()
      tbthemebox$front(
        top    = setframe,
        labels = list("kmg2_theme_gray", "kmg2_theme_bw"),
        title  = kmg2_gettextRcmdr("Theme")
      )

			if (showcolourbox) {
        back_list <<- list(
          tbfontsizebox$setframe,
          labelRcmdr(setframe, text="    "),
          tbfontfoamilybox$frame,
          labelRcmdr(setframe, text="    "),
          tbcolourbox$frame,
          labelRcmdr(setframe, text="    "),
          tbcheckbox$setframe,
          labelRcmdr(setframe, text="    "),
          tbthemebox$setframe
        )
      } else {
        back_list <<- list(
          tbfontsizebox$setframe,
          labelRcmdr(setframe, text="    "),
          tbfontfoamilybox$frame,
          labelRcmdr(setframe, text="    "),
          tbcheckbox$setframe,
          labelRcmdr(setframe, text="    "),
          tbthemebox$setframe
        )
      }

    }

  )
)
