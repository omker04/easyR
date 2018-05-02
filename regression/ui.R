source('setupPackages.R')

header <- dashboardHeader(title = h2("Regression Analysis"), titleWidth = "20%")
sidebar <- dashboardSidebar(width = "20%",{
  useShinyjs()
  fluidPage(
    fileInput("datafile", label = h4("Please upload the datafile to proceed"), width = "105%", accept = c(".txt", ".csv")),
    hidden(div(id = "variableSelection", {
      fluidRow(
        selectInput("response", label = h4("Please Select the Response Variable"), choices = list(), multiple = FALSE, width = "100%"),
        selectInput("predictors", label = h4("Please Choose the Predictor Variables"), choices = list(), multiple = TRUE, width = "100%")
      )
    })
  ))
})
body <- dashboardBody(
  useShinyjs(),
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
  bsAlert(anchorId = "nonNumeric"),
  fluidPage(
    hidden(div(id = "warningBox",
               box(title = "Warning!!", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 12,{
                 fluidPage(
                   bsAlert(anchorId = "NAresponse"),
                   hidden(div(id = "handleOutlier",
                              box(status = "warning", solidHeader = TRUE, background = "yellow", width = "100%",
                                  fluidPage(
                                    bsAlert(anchorId = "hasOutlier"),
                                    column(offset = 8, width = 2, hidden(actionLink("showBoxplot", label = strong("Show Boxplot"), style="color: white")), hidden(actionLink("hideBoxplot", label = "Hide Boxplot", style="color: white"))),
                                    column(width = 2, actionLink("showMoreOut", label = "Show Details", style="color: white"), hidden(actionLink("showLessOut", label = "Hide Details", style="color: white"))),
                                    DT::dataTableOutput(outputId = "outlierDetails"),
                                    div(id = "boxdiv", 
                                      column(offset = 1, width = 10, sliderInput("boxSlider", label = "Input the Range of the BoxPlot Whiskers:", min = 1, max = 10, step = 0.5, value = 2.5)),
                                      column(width = 1),
                                      column(width = 12, 
                                             plotOutput(outputId = "boxplot")
                                             )
                                      ),
                                    tags$head(tags$style(type="text/css", "#outlierDetails table td {line-height:50%;}"))
                                )
                              ))
                          ),
                   hidden(div(id = "missingImpute",
                              box(status = "warning", solidHeader = TRUE, background = "yellow", width = "100%",
                                  bsAlert(anchorId = "NApredictor"),
                                  column(offset = 10, width = 2, actionLink("showMoreNA", label = "Show Details", style="color: white"), hidden(actionLink("showLessNA", label = "Hide Details", style="color: white"))),
                                  #hidden(div(id = "table2", 
                                             DT::dataTableOutput(outputId = "handleNA"),#)),
                                  tags$head(tags$style(type="text/css", "#handleNA table td {line-height:50%;}"))
                                  )
                              )
                          )
                   )
                 })
               )
           ),
    hidden(div(id = "summary", box(title = "Model Results", status = "success", solidHeader = TRUE, width = 12,
      fluidPage(
          verbatimTextOutput(outputId = "summaryLinModel"),
          column(offset = 10, width = 1, actionButton("cancel", label = "Cancel")),
          column(width = 1, actionButton("done", label = "Done"))
        )
      )
    ))
  )
)

ui <- dashboardPage(header, sidebar, body)
