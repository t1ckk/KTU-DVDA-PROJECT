library(shiny)
library(h2o)
library(tidyverse)

ui <- fluidPage(


    titlePanel("Banking APP"),

    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV file"),
            uiOutput("download")
        ),
        mainPanel(
        h2("Predictions sample:"),
        tableOutput("table")
        )
    )
)


server <- function(input, output) {
  h2o.init()
  model <- h2o.loadModel("../4-model/my_model")
  
  output$table <- renderTable({
    req(input$file)
    # test_data <- h2o.importFile(input$file$datapath)
    # predictions <- h2o.predict(model, test_data)
    head(datasetInput(), n = 10)
  })
  
  datasetInput <- reactive({
    req(input$file)
    test_data <- h2o.importFile(input$file$datapath)
    predictions <- h2o.predict(model, test_data)
    as_tibble(predictions)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$file, "-predictions.csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    })
  
  output$download <- renderUI({
    if(!is.null(input$file)) {
      downloadButton('downloadData', 'Download Output File')
    }
  })
}

shinyApp(ui = ui, server = server)
