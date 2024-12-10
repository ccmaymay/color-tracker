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
library(janitor)
library(jsonlite)
library(lubridate)

api_key <- Sys.getenv("GOOGLE_API_KEY")
spreadsheet_id <- Sys.getenv("SPREADSHEET_ID")

color_levels <- c("purple", "blue", "IR blue", "turquoise", "IR turquoise", "orange", "IR purple")
#color_map <- c('purple'='#b76d96', 'blue'='#43679a', 'turquoise'='#8bb4a4', 'orange'='#eb8621')
color_map <- c('purple'='purple', 'blue'='blue', 'IR blue'='#bbccff', 'turquoise'='turquoise', 'IR turquoise'='#ccffee', 'orange'='orange', 'IR purple'='#eeccff')
tz <- 'US/Eastern'

loadRawData <- function() {
  client <- HttpClient$new(url="https://sheets.googleapis.com")
  response <- client$get(
    path=str_c("/v4/spreadsheets/", spreadsheet_id, "/values/A1:F"),
    query=list(key=api_key))
  if (! response$success()) {
    stop(str_c("Status ", as.character(response$status), " from data source API"))
  }
  response$raise_for_ct_json()
  fromJSON(response$parse())$values %>%
    row_to_names(1) %>%
    as_tibble(.name_repair="check_unique")
}

processRawData <- function(data) {
  data %>%
    filter(
      Date %>% str_trim %>% str_length > 0,
      Colors %>% str_trim %>% str_length > 0,
      `Time 1` %>% str_trim %>% str_length > 0,
      `Time 2` %>% str_trim %>% str_length > 0,
      `Total time` %>% str_trim %>% str_length > 0) %>%
    mutate(
      start_datetime=Date %>% map_vec(function(date_chr) {
        if (!is.na(date_chr %>% mdy_hm(tz=tz))) {
          date_chr %>% mdy_hm(tz=tz)
        } else {
          date_chr %>% mdy_hms(tz=tz)
        }
      }),
      adjusted_date=list(Date, start_datetime) %>% pmap_vec(function(date_chr, start_dt) {
        if (!is.na(start_dt)) {
          (start_dt - hours(6)) %>% floor_date("day")
        } else {
          date_chr %>% mdy(tz=tz)
        }
      }),
      adjusted_start_hour=difftime(start_datetime, adjusted_date),
      colors=Colors,
      duration_1_minutes=`Time 1` %>% hms %>% as.numeric("minute"),
      duration_2_minutes=`Time 2` %>% hms %>% as.numeric("minute"),
      total_duration_minutes=`Total time` %>% hms %>% as.numeric("minute"),
      notes=Notes) %>%
    separate_wider_delim(colors, "/", names=c("color_1", "color_2")) %>%
    pivot_longer(
      cols=c(color_1, color_2),
      names_to="color_type",
      values_to="color") %>%
    mutate(
      color_num=color_type %>% str_split_i(fixed("_"), 2) %>% as.numeric,
      color=factor(color, levels=color_levels),
      display_color=factor(
        as.character(color_map[color]),
        levels=as.character(color_map[color_levels])),
      time_minutes=ifelse(color_num == 1, duration_1_minutes, duration_2_minutes)) %>%
    select(
      start_datetime,
      adjusted_date,
      adjusted_start_hour,
      color,
      color_num,
      display_color,
      time_minutes,
      total_duration_minutes,
      notes)
}

makeScatterPlot <- function(data, colors_to_show) {
  data %>%
    mutate(alpha=ifelse(color %in% colors_to_show, 1, 0)) %>%
    ggplot(aes(x=adjusted_date, y=time_minutes, group=display_color, color=display_color, alpha=alpha)) +
    geom_point(size=4) +
    scale_color_identity() +
    scale_alpha_identity() +
    theme_bw() +
    theme(legend.position="none") +
    xlab("date") +
    ylab("time elapsed (minutes)") +
    ggtitle("Elapsed Times")
}

makeStartTimePlot <- function(data, colors_to_show) {
  data %>%
    mutate(alpha=ifelse(color %in% colors_to_show, 1, 0)) %>%
    ggplot(aes(x=adjusted_start_hour, y=time_minutes, group=display_color, color=display_color, alpha=alpha)) +
    geom_point(size=4) +
    scale_color_identity() +
    scale_alpha_identity() +
    theme_bw() +
    theme(legend.position="none") +
    xlab("start hour (relative to midnight)") +
    ylab("time elapsed (minutes)") +
    ggtitle("Elapsed Times versus Start Hour")
}

makeBarPlot <- function(data, colors_to_show) {
  data %>%
    # Sort colors so smaller colors appear on lower bars
    mutate(display_color=factor(display_color, levels=rev(color_map)),
           alpha=ifelse(color %in% colors_to_show, 1, 0)) %>%
    ggplot(aes(x=adjusted_date, y=time_minutes, group=display_color, fill=display_color, alpha=alpha)) +
    geom_bar(stat='identity', position='stack') +
    scale_fill_identity() +
    scale_alpha_identity() +
    theme_bw() +
    xlab("date") +
    ylab("time elapsed (minutes)") +
    ggtitle("Elapsed Times (Stacked)")
}

makeFacetedBarPlot <- function(data, colors_to_show) {
  data %>%
    mutate(alpha=ifelse(color %in% colors_to_show, 1, 0)) %>%
    ggplot(aes(x=adjusted_date, y=time_minutes, fill=display_color, alpha=alpha)) +
    geom_bar(stat='identity', position='dodge') +
    scale_fill_identity() +
    scale_alpha_identity() +
    theme_bw() +
    xlab("date") +
    ylab("time elapsed (minutes)") +
    facet_grid(color ~ .) +
    ggtitle("Elapsed Times Separated by Color")
}

makeSummaryBarPlot <- function(data, colors_to_show) {
  data %>%
    filter(color %in% colors_to_show) %>%
    ggplot(aes(y=time_minutes, fill=display_color)) +
    geom_histogram() +
    scale_fill_identity() +
    theme_bw() +
    ylab("time elapsed (minutes)") +
    facet_grid(~ color) +
    ggtitle("Elapsed Time Distribution per Color")
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Color Tracker"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("colors", "Colors to show:",
                         color_levels,
                         selected=c("purple", "blue", "turquoise", "orange"))),
    mainPanel(
      plotOutput("scatterPlot", height=400),
      plotOutput("barPlot", height=400),
      plotOutput("startTimePlot", height=400),
      plotOutput("summaryPlot", height=400))
  )
)

# Define server logic
server <- function(input, output) {
  data <- loadRawData() %>% processRawData
  output$scatterPlot <- renderPlot(data %>% makeScatterPlot(input$colors))
  output$barPlot <- renderPlot(data %>% makeBarPlot(input$colors))
  output$startTimePlot <- renderPlot(data %>% makeStartTimePlot(input$colors))
  #output$facetedBarPlot <- renderPlot(data %>% makeFacetedBarPlot(input$colors))
  output$summaryPlot <- renderPlot(data %>% makeSummaryBarPlot(input$colors))
}

# Run the application 
shinyApp(ui = ui, server = server)
