library(shiny)
library(h2o)
library(shinythemes)
library(dplyr)
library(DT)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Banking APP"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      sliderInput("n", "Number of predictions to show", min = 10, max = 10000, value = 10)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predictions", DT::dataTableOutput("predictionsTable")),
        tabPanel("Predictions Plot", plotOutput("predictionsPlot"), plotOutput("predictionsPlot2"))
      )
    )
  )
)

server <- function(input, output, session) {
  h2o.init()
  model <- h2o.loadModel("C:/Users/admin/Downloads/Drive Data/model/gmb_grid2")
  
  # Predictions Table
  output$predictionsTable <- DT::renderDataTable({
    req(input$file)
    data <- read.csv(input$file$datapath)
    data <- data %>% mutate_all(~na_if(., "NA"))
    test_data <- as.h2o(data)
    predictions <- h2o.predict(model, test_data)
    predictions <- select(as.data.frame(predictions), p0, p1)
    head(predictions, n = input$n)
    datatable(predictions, options = list(pageLength = input$n))
  })
  
  # Predictions Plot p1
  output$predictionsPlot <- renderPlot({
    req(input$file)
    data <- read.csv(input$file$datapath)
    data <- data %>% mutate_all(~na_if(., "NA"))
    test_data <- as.h2o(data)
    predictions <- h2o.predict(model, test_data)
    predictions <- select(as.data.frame(predictions), predict, p1)
    plot(predictions[, "predict"], predictions[, "p1"], xlab = "predict", ylab = "p1", main = "Predictions Plot")
  })
  
  # Predictions Plot p0
  output$predictionsPlot2 <- renderPlot({
    req(input$file)
    data <- read.csv(input$file$datapath)
    data <- data %>% mutate_all(~na_if(., "NA"))
    test_data <- as.h2o(data)
    predictions <- h2o.predict(model, test_data)
    predictions <- select(as.data.frame(predictions), predict, p0)
    plot(predictions[, "predict"], predictions[, "p0"], xlab = "predict", ylab = "p0", main = "Predictions Plot")
  })
}

shinyApp(ui = ui, server = server)
