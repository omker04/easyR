shinyServer(function(input, output, session){
  options(shiny.maxRequestSize = 5*1024^3) #setting maximum filesize to 5gb
  options(warn = -1)
  datafile <- reactive({
    file1 <- input$datafile
    if(is.null(file1)){
      return(NULL)
    }else{
      datafile <- fread(file1$datapath, stringsAsFactors = FALSE, na.strings = c(NA, "", "NA", NULL, "null", "NULL", "N/A", " "), integer64 = "numeric") %>% data.frame()
      return(datafile)
    }
  })
  
  observe(if(!is.null(datafile())){
    shinyjs::show(id = "variableSelection")
    updateSelectInput(session, inputId = "response", choices = colnames(datafile()), selected = integer(0))
    observeEvent(input$response,{
      updateSelectInput(session, inputId = "predictors", choices = colnames(datafile())[-which(colnames(datafile()) == input$response)], selected = integer(0))
    })
    observeEvent(input$predictors,{
      if(sum(apply(datafile()[input$predictors], 2, function(x) is.na(as.numeric(x)))) > 0){
        createAlert(session, anchorId = "nonNumeric", alertId = "Alert0",title = "Error!!", content = "Atleast one of the selected Regressor Variable is non-numeric.", style = "error")
      }else{
        closeAlert(session, alertId = "Alert0")
      }
      if(input$predictors %>% length() == 0 || sum(apply(datafile()[input$predictors], 2, is.character)) > 0){
        hide(id = "summary", anim = TRUE)
      }else{
        show(id = "summary", anim = TRUE)
      }
    })
  })
  
  values <- reactiveValues(range = 3, data = NULL, choice = NULL)
  warningInd <- reactive({
    data1 <- datafile()
    if(!is.null(input$response) && !is.null(input$predictors)){
      if(sum(apply(data1[input$predictors], 2, is.character)) > 0){
        return(NULL)
      }
      missingResponse <- ifelse(sum(is.na(data1[input$response])) > 0, TRUE, FALSE)
      if(missingResponse)
        data1 <<- data1[!is.na(data1[input$response]),]
      missingPredictors <- !complete.cases(t(data1[input$predictors]))
      missingPredictorPct <- data1[input$predictors[missingPredictors]] %>% as.data.frame() %>% apply(., 2, function(x){paste0(((which(is.na(x)) %>% length())*100/nrow(datafile())) %>% round(.,5), "%")}) %>% as.data.frame()
      is.outlier <- apply(data1[input$predictors], 2, function(x) {boxplot(x, range = values$range, plot = FALSE)$out %>% length > 0}) %>% as.vector()
      outlier <- apply(data1[input$predictors], 2, function(x) boxplot(x, range = values$range, plot = FALSE)$out)[is.outlier]
      if(input$predictors %>% length() == 1){
        outlier <- outlier %>% list()
        names(outlier) <- input$predictors
      }
      indicator <- c(missingResponse, ifelse(sum(missingPredictors) > 0, TRUE, FALSE))
      return(list(indicator, is.outlier, outlier, missingPredictorPct))
    }else{
      return(NULL)
    }
  })
  
  observe(if(!is.null(warningInd())){
    #data1 <- datafile()
    data1 <<- datafile()
    pred <<- input$predictors
    if(warningInd()[1:2] %>% do.call("sum",.) == 0){
      hide(id = "warningBox", anim = TRUE)
    }else{
      show(id = "warningBox", anim = TRUE)
      closeAlert(session, alertId = "Alert0")
    }
    if(warningInd()[[1]][1]){
      createAlert(session, anchorId = "NAresponse", alertId = "Alert1", title = HTML("Observation Missing in Response Variable. <br> Ignoring the missing rows and continuing with the selection."), style = "warning")
    }else{
      closeAlert(session, alertId = "Alert1")
    }
    if(warningInd()[[1]][2]){
      show(id = "missingImpute", anim = TRUE)
      createAlert(session, anchorId = "NApredictor", alertId = "Alert2", title = "Observation Missing in Regressor Variable(s).", style = "warning", dismiss = FALSE)
    }else{
      hide(id = "missingImpute", anim = TRUE)
      closeAlert(session, alertId = "Alert2")
    }
    if(warningInd()[[2]] %>% sum() > 0){
      show(id = "handleOutlier", anim = TRUE)
      createAlert(session, anchorId = "hasOutlier", alertId = "Alert3", title = "Outier Detected in Regressor Variable(s).", style = "warning",dismiss = FALSE)
    }else{
      hide(id = "handleOutlier", anim = TRUE)
      closeAlert(session, alertId = "Alert3")
    }
  }else{
    hide(id = "warningBox", anim = TRUE)
  })
  
  shinyInput <- function(FUN, len, id, select = NULL, ...) {
    inputs <- character(len)
    if(is.null(select)){
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
    }else{
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ..., selected = select[i]))
      }
    }
    inputs
  }
  shinyValue <- function(id, len) {
    if(is.null(len)) return(NULL)
    else{
    unlist(lapply(seq_len(len), function(i) {
      value <- input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))}
  }
  
  observeEvent(input$boxSlider,{
    values$range <- input$boxSlider
    toCheck <- input$predictors[warningInd()[[2]]]
  })
  
  getBoxPlot <- reactive({
    toCheck <- input$predictors[warningInd()[[2]]]
    par(mfcol = c(1, toCheck %>% length()))
    sapply(toCheck, function(x) {return(boxplot(as.data.frame(data1[x]), range = values$range, horizontal = FALSE, plot = TRUE))})
  })
  output$boxplot <- renderPlot({getBoxPlot()})
  
  
  observe(hide(id = "boxdiv"))
  observeEvent(input$showBoxplot,{
    updateSliderInput(session, "boxSlider", value = 3)
    
    show(id = "boxdiv", anim = TRUE)
    show(id = "hideBoxplot")
    hide(id = "showBoxplot")
    
  })
  observeEvent(input$hideBoxplot,{
    hide(id = "boxdiv", anim = TRUE)
    hide(id = "hideBoxplot")
    show(id = "showBoxplot")
  })

  dfS <- reactiveValues()
  flag <- reactiveValues(Outlier = TRUE, Missing = TRUE)
  
  dfOutlier <- eventReactive(input$predictors, {
    #hide(id = "outlierDetails", anim = TRUE)
    # hide(id = "showLessOut")
    # show(id = "showMoreOut")
    # hide(id = "showBoxplot")
    # hide(id = "hideBoxplot")

    # data = data.frame(
    #   "RegressorsWithOutlier" = input$predictors[warningInd()[[2]]],
    #   "WhatToDo" = shinyInput(radioButtons, input$predictors[warningInd()[[2]]] %>% length, "radioOutlier", label = "", inline = TRUE,
    #                             choices = c("Do Nothing, Keep all Outliers", "Discard All Outliers"), selected = integer(0))
    # 
    # )
    if(flag$Outlier){
      # rowOptions <- colnames(datafile())[-which(colnames(datafile()) == input$response)]
      # data = data.frame(
      #   "Regressors.with.Outlier" = rowOptions,
      #   "What.to.do" = shinyInput(radioButtons, rowOptions %>% length, "radioOutlier", label = "", inline = TRUE, 
      #                             choices = c("Do Nothing, Keep all Outliers", "Discard All Outliers"))
      # )
      data = data.frame(
        "RegressorsWithOutlier" = input$predictors[warningInd()[[2]]],
        "WhatToDo" = shinyInput(radioButtons, input$predictors[warningInd()[[2]]] %>% length, "radioOutlier", label = "", inline = TRUE,
                                  choices = c("Do Nothing, Keep all Outliers", "Discard All Outliers"))
      )
      flag$Outlier <- FALSE
    }else{
      inputsOutlier <- shinyValue('radioOutlier', input$predictors[warningInd()[[2]]] %>% length)
      NAOutVAlue <- which(is.na(inputsOutlier))
      inputsOutlier <- replace(inputsOutlier, NAOutVAlue, 'Do Nothing, Keep all Outliers')
      data = data.frame(
        "RegressorsWithOutlier" = input$predictors[warningInd()[[2]]],
        "WhatToDo" = shinyInput(radioButtons, input$predictors[warningInd()[[2]]] %>% length, "radioOutlier", label = "", inline = TRUE,
                                choices = c("Do Nothing, Keep all Outliers", "Discard All Outliers"), select = inputsOutlier)
      )
    }
    return(data)
  })
  
  
  output$outlierDetails <- DT::renderDataTable({datatable(dfOutlier(),
                                                          escape = FALSE, selection = 'none', rownames = FALSE,
                                                          options = list(preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                                         drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                                                                         sDom  = '<"top">t<"bottom">ip')) %>% formatStyle(.,names(dfOutlier()), background = "orange")})
  observe(hide(id = "outlierDetails", anim = TRUE))
  observeEvent(input$showMoreOut,{
    #getBoxPlot()
    show(id = "outlierDetails", anim = TRUE)
    hide(id = "showMoreOut")
    show(id = "showLessOut")
    hide(id = "hideBoxplot")
    show(id = "showBoxplot")
  })
  observeEvent(input$showLessOut,{
    hide(id = "outlierDetails", anim = TRUE)
    show(id = "showMoreOut")
    hide(id = "showLessOut")
    hide(id = "hideBoxplot")
    hide(id = "showBoxplot")
    hide(id = "boxdiv")
  })
  
  
  dfNA <- eventReactive(input$predictors, {
    if(nrow(warningInd()[[4]]) > 0){
      if(flag$Missing){
        data <- data.frame(
          "RegressorsWithMissingValues" = warningInd()[[4]] %>% rownames(),
          "PercentageOfMissingValues" = warningInd()[[4]][,1],
          "WhatToDo" = shinyInput(radioButtons, warningInd()[[4]] %>% nrow, "radioNA", label = "", inline = TRUE,
                                    choices = c("Replace by Median", "Replace by Mean", "Discard the relevant rows"))
        )
        flag$Missing <- FALSE
      }else{
        inputsMissing <- shinyValue('radioNA', warningInd()[[4]] %>% nrow)
        NAMissVAlue <- which(is.na(inputsMissing))
        inputsMissing <- replace(inputsMissing, NAMissVAlue, 'Replace by Median')
        data <- data.frame(
          "RegressorsWithMissingValues" = warningInd()[[4]] %>% rownames(),
          "PercentageOfMissingValues" = warningInd()[[4]][,1],
          "WhatToDo" = shinyInput(radioButtons, warningInd()[[4]] %>% nrow, "radioNA", label = "", inline = TRUE,
                                    choices = c("Replace by Median", "Replace by Mean", "Discard the relevant rows"), select = inputsMissing)
        )
      }
    }
    return(data)
  })
  output$handleNA <- DT::renderDataTable({datatable(dfNA(), escape = FALSE, selection = 'none', rownames = FALSE,
                                                    options = list(preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                                   drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                                                                   sDom  = '<"top">t<"bottom">ip')) %>% formatStyle(.,names(dfNA()), background = "orange")})
  observe(hide(id = "handleNA", anim = TRUE))
  observeEvent(input$showMoreNA,{
    toggle(id = "handleNA", anim = TRUE)
    toggle(id = "showMoreNA")
    toggle(id = "showLessNA")
  })
  observeEvent(input$showLessNA,{
    toggle(id = "handleNA", anim = TRUE)
    toggle(id = "showMoreNA")
    toggle(id = "showLessNA")
  })
  
  observeEvent(input[['radioOutlier1']],{
    print(input[['radioOutlier1']])
  })
  
  
  observe({
    if(!is.null(warningInd())){
      what2doOut <- c()
      values$data <<- data1
      inData <- data1
      #shinyValueOut <- shinyValue("radioOutlier", input$predictors[warningInd()[[2]]] %>% length)
      shinyValueOut <- shinyValue("radioOutlier", colnames(datafile())[-which(colnames(datafile()) == input$response)] %>% length)
      if(shinyValueOut %>% is.na() %>% prod() != 1){
        what2doOut <- shinyValueOut
        names(what2doOut) <- input$predictors[warningInd()[[2]]]
      }else{
        if(input$predictors[warningInd()[[2]]] %>% length() > 0){
          what2doOut <- rep("Do Nothing, Keep all Outliers", input$predictors[warningInd()[[2]]] %>% length())
          names(what2doOut) <- input$predictors[warningInd()[[2]]]
        }
      }
      what2doNA <- c()
      shinyValueNA <- shinyValue("radioNA", warningInd()[[4]] %>% nrow)
      if(shinyValueNA %>% is.na() %>% prod() != 1){
        what2doNA <- shinyValueNA
      }else{
        if(warningInd()[[4]] %>% nrow %>% is.numeric() > 0)
          what2doNA <- rep("Replace by Median", warningInd()[[4]] %>% nrow %>% as.numeric())
      }
      names(what2doNA) <- warningInd()[[4]] %>% rownames()
      
      outlist <- NULL
      if(input$predictors %>% length() >= 1){
        outlist <- warningInd()[[3]]
        outlist <- sapply(seq_len(outlist %>% length), function(x){!is.na(match(datafile()[,names(outlist)[x]] %>% as.vector(), outlist[[x]] %>% as.vector()))})
        if(!is.null(outlist %>% dim())){
          colnames(outlist) <- input$predictors[warningInd()[[2]]]
          outlist <- apply(outlist, 2, function(x){which(x == TRUE)})
        }
        if(input$predictors %>% length() == 1){
          outlist <- outlist %>% as.vector() %>% list()
          names(outlist) <- input$predictors
        }
        outlist
      }
      
      NAlist <- NULL
      if(input$predictors %>% length() >= 1 && warningInd()[[4]] %>% nrow() > 0){
        NAlist <- lapply(seq_len(warningInd()[[4]] %>% nrow), function(x){which(is.na(datafile()[(warningInd()[[4]] %>% rownames())[x]]))})
        names(NAlist) <- warningInd()[[4]] %>% rownames()
      }
      # print(what2doOut)
      if("Discard All Outliers" %in% what2doOut | "Discard the relevant rows" %in% what2doNA){
        # print("In the delete loop")
        whichOutDelete <- outlist[which(what2doOut == "Discard All Outliers")] %>% unlist() %>% as.vector()
        whichNADelete <- NAlist[which(what2doNA == "Discard the relevant rows")] %>% unlist() %>% as.vector()
        whichDelete <- c(whichNADelete, whichOutDelete) %>% unique()
        #print(whichDelete)
        inData <- inData[-whichDelete,]
      }
      if("Replace by Median" %in% what2doNA){
        replMedian <- names(what2doNA)[which(what2doNA == "Replace by Median")]
        whichReplMedian <- NAlist[which(what2doNA == "Replace by Median")]
        for(i in 1:length(replMedian)){
          inData[whichReplMedian[[i]] %>% as.vector(), replMedian[i]] <- median(inData[,replMedian[i]], na.rm = TRUE)
          #print(inData[whichReplMedian[[i]] %>% as.vector(), ])
        }
      }
      if("Replace by Mean" %in% what2doNA){
        replMean <- names(what2doNA)[which(what2doNA == "Replace by Mean")]
        whichReplMean <- NAlist[which(what2doNA == "Replace by Mean")]
        #print(inData[c(whichReplMean[[1]] %>% as.vector(), 16), ])
        for(i in 1:length(replMean)){
          inData[whichReplMean[[i]] %>% as.vector(), replMean[i]] <- mean(inData[,replMean[i]], na.rm = TRUE)
          #print(inData[whichReplMean[[i]] %>% as.vector(), ])
        }
      }
      
      linModel <<- lm(formula = paste(input$response, "~", paste(input$predictors, collapse = " + ")) %>% as.formula(), data = inData)
      summaryLinModel <<- linModel %>% summary()
      # print(linModel)
      # print(summaryLinModel)
      show(id = "summary", anim = TRUE)
      output$summaryLinModel <- renderPrint({summaryLinModel}, width = "10000")
      
      if(is.null(warningInd())){
        hide(id = "summary", anim = TRUE)
      }
    }
  })
})



