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
library(crul)
library(jsonlite)
library(janitor)
library(lubridate)


# Define UI for application
ui <- fluidPage(
  titlePanel("Color Tracker"),
  mainPanel(
    plotOutput("scatterPlot", height=400),
    plotOutput("barPlot", height=400),
    plotOutput("facetPlot", height=800),
    plotOutput("summaryPlot", height=400)
  )
)

# Define server logic
server <- function(input, output) {
  color_levels <- c("purple", "blue", "turquoise", "orange")
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  spreadsheet_id <- Sys.getenv("SPREADSHEET_ID")
  client <- HttpClient$new(url="https://sheets.googleapis.com")
  response <- client$get(
    path=str_c("/v4/spreadsheets/", spreadsheet_id, "/values/A1:F"),
    query=list(key=api_key))
  if (! response$success()) {
    stop(str_c("Status ", as.character(response$status), " from data source API"))
  }
  response$raise_for_ct_json()
  data <- fromJSON(response$parse())$values %>%
    row_to_names(1) %>%
    as_tibble(.name_repair="check_unique") %>%
    filter(
      Date %>% str_trim %>% str_length > 0,
      Method %>% str_trim %>% str_length > 0,
      `Time 1` %>% str_trim %>% str_length > 0,
      `Time 2` %>% str_trim %>% str_length > 0,
      `Total time` %>% str_trim %>% str_length > 0) %>%
    mutate(
      date=Date %>% mdy,
      method=Method,
      time_1=`Time 1` %>% hms %>% as.numeric("minute"),
      time_2=`Time 2` %>% hms %>% as.numeric("minute"),
      total_time=`Total time` %>% hms %>% as.numeric("minute")) %>%
    separate_wider_delim(method, "/", names=c("color_1", "color_2")) %>%
    pivot_longer(
      cols=c(color_1, color_2),
      names_to="color_type",
      values_to="color") %>%
    mutate(
      color_num=color_type %>% str_split_i(fixed("_"), 2) %>% as.numeric,
      color=factor(color, levels=color_levels),
      time=ifelse(color_num == 1, time_1, time_2))
  output$scatterPlot <- renderPlot({
    data %>%
      ggplot(aes(x=date, y=time, group=color, color=color)) +
      geom_jitter(size=5, height=0.5, width=0) +
      scale_color_identity() +
      theme_bw() +
      ylab("time elapsed (minutes)") +
      ggtitle("Individual Times")
  })
  output$barPlot <- renderPlot({
    data %>%
      # Sort colors so smaller colors appear on lower bars
      mutate(color=factor(color, levels=rev(color_levels))) %>%
      ggplot(aes(x=date, y=time, group=color, fill=color)) +
      geom_bar(stat='identity', position='stack') +
      scale_fill_identity() +
      theme_bw() +
      ylab("time elapsed (minutes)") +
      ggtitle("Individual Times Combined by Date")
  })
  output$facetPlot <- renderPlot({
    data %>%
      mutate(color_copy=color) %>%
      ggplot(aes(x=date, y=time, fill=color_copy)) +
      geom_bar(stat='identity', position='dodge') +
      scale_fill_identity() +
      theme_bw() +
      ylab("time elapsed (minutes)") +
      facet_grid(color ~ .) +
      ggtitle("Individual Times Separated by Color")
  })
  output$summaryPlot <- renderPlot({
    data %>%
      mutate(color_copy=color) %>%
      ggplot(aes(y=time, fill=color_copy)) +
      geom_histogram() +
      scale_fill_identity() +
      theme_bw() +
      ylab("time elapsed (minutes)") +
      facet_grid(~ color) +
      ggtitle("Time Distribution per Color")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
