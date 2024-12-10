#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

source('common.R')


# Define UI for application
ui <- fluidPage(
  titlePanel("Color Tracker"),
  mainPanel(
    #plotOutput("scatterPlot", height=400),
    plotOutput("barPlot", height=400),
    plotOutput("facetedBarPlot", height=800),
    plotOutput("summaryPlot", height=400)
  )
)

# Define server logic
server <- function(input, output) {
  data <- loadRawData() %>% processRawData
  output$scatterPlot <- renderPlot(data %>% makeScatterPlot)
  output$barPlot <- renderPlot(data %>% makeBarPlot)
  output$facetedBarPlot <- renderPlot(data %>% makeFacetedBarPlot)
  output$summaryPlot <- renderPlot(data %>% makeSummaryBarPlot)
}

# Run the application 
shinyApp(ui = ui, server = server)
