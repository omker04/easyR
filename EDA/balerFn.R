source('global.R')

installedPkgs

easyEDA <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  defaultData <- text
  fname = gsub("\\\\", "/", tempfile())
  
  ui <- miniUI::miniPage(
    gadgetTitleBar('Easy EDA'),
    miniTabstripPanel(
      miniTabPanel('Data', icon = icon('table'),
        miniPage(
          stableColumnLayout(
            textInput(inputId = 'data', label = 'Data', value = defaultData),
            uiOutput('searchResult')
          ),
          #'Select a column to view its Summary',
          shiny::dataTableOutput('dataView')
        )
      ),
      miniTabPanel('Visualize', icon = icon('table'))
    )
  )
  
  server <- function(input, output, session){
    reactiveData <- shiny::reactive({
      dataString <- input$data
      if (!nzchar(dataString)) 
        return(errorMessage("data", "No dataset selected."))
      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data", paste("No dataset named '", 
                                          dataString, "' available.")))
      data <- get(dataString, envir = .GlobalEnv)
      if (is.data.frame(data) | is.matrix(data) | tibble::is.tibble(data)) {
        return(data)
      } else {
        return(errorMessage('data', 'Data must be a data.frame, matrix or tibble object.'))
      }
    })
    output$searchResult <- shiny::renderUI({
      data <- reactiveData()
      if (isErrorMessage(data)) 
        htmltools::h4(style = "color: red;", data$message)
    })
    output$dataView <- shiny::renderDataTable({
      data <- reactiveData()
      if(!isErrorMessage(data))
        return(datatable(data, selection = 'single'))
    })
    
    shiny::observeEvent(input$done, {
      invisible(shiny::stopApp())
    })
  }
  #viewer <- shiny::dialogViewer("Edit", width = 1000, height = 800)
  shiny::runGadget(ui, server)
}

installedPkgs