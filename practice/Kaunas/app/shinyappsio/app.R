library(shinydashboard)
library(shiny)
library(h2o)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Loan dashboard 2022"),
  dashboardSidebar(fileInput("file", "Ikelti csv faila")),
  dashboardBody(
    # valueBox(100, "Basic example"),
    tableOutput("table"),
    dataTableOutput("predictions")
  )
)
server <- function(input, output) {
  h2o.init()
  # model <- h2o.loadModel("my_model")
  model <- h2o.import_mojo("GBM_1_AutoML_1_20221124_232404.zip")
  output$table <- renderTable({
    req(input$file)
    table <- read_csv(input$file$datapath) %>%
      group_by(credit_score) %>%
      summarise(n = n())
    table
  })
  
  output$predictions <- renderDataTable({
    req(input$file)
    df_test <- h2o.importFile(input$file$datapath)
    p <- h2o.predict(model, df_test)
    p %>%
      as_tibble() %>%
      mutate(y = predict) %>%
      select(y) %>%
      rownames_to_column("id") %>%
      head(20)
  })
}
shinyApp(ui, server)