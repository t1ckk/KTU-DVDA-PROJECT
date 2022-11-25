library(shiny)
library(h2o)

ui <- fluidPage(


    titlePanel("Banking APP"),

    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV file")
        ),

        mainPanel(
        tableOutput("table")
        )
    )
)


server <- function(input, output) {
  h2o.init()
  # model <- h2o.loadModel("../../4-model/gmb_grid")
  model <- h2o.import_mojo("../../4-model/GBM_1_AutoML_1_20221124_232404.zip")
  
  output$table <- renderTable({
    req(input$file)
    test_data <- h2o.importFile(input$file$datapath)
    predictions <- h2o.predict(model, test_data)
    head(predictions, n = 100)
    # read.csv(input$file$datapath)
  })
}

shinyApp(ui = ui, server = server)
