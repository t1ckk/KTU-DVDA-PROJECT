library(shiny)
library(h2o)
library(shinythemes)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Banking APP"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predictions", tableOutput("predictions")),
        tabPanel("ROC curve", tableOutput("roc_plot")),
        tabPanel("Performance", tableOutput("performance_plot"))
      )
    )
  )
)


server <- function(input, output) {
  h2o.init()
  model <- h2o.loadModel("C:/Users/admin/Downloads/Drive Data/model/gmb_grid2")
  
  # Predictions
  output$predictions <- renderTable({
    req(input$file)
    data <- read.csv(input$file$datapath)
    data <- data %>% mutate_all(~na_if(., "NA"))
    test_data <- as.h2o(data)
    predictions <- h2o.predict(model, test_data)
    head(predictions, n = 100)
  })
  output$roc_plot <- renderPlot({
    req(input$file)
    data <- read.csv(input$file$datapath)
    data <- data %>% mutate_all(~na_if(., "NA"))
    test_data <- as.h2o(data)
    roc <- h2o.roc(model, test_data)
    h2o.plot(roc)
  })
  output$performance_plot <- renderPlot({
    req(input$file)
    data <- read.csv(input$file$datapath)
    data <- data %>% mutate_all(~na_if(., "NA"))
    test_data <- as.h2o(data)
    performance <- h2o.performance(model, test_data)
    ggplot(data = performance, aes(x = threshold, y = f1)) + geom_line()
  })
}

shinyApp(ui = ui, server = server)
