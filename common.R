library(tidyverse)
library(crul)
library(janitor)
library(jsonlite)
library(lubridate)

api_key <- Sys.getenv("GOOGLE_API_KEY")
spreadsheet_id <- Sys.getenv("SPREADSHEET_ID")

color_levels <- c("purple", "blue", "turquoise", "orange")
#color_map <- c('purple'='#b76d96', 'blue'='#43679a', 'turquoise'='#8bb4a4', 'orange'='#eb8621')
color_map <- c('purple'='purple', 'blue'='blue', 'turquoise'='turquoise', 'orange'='orange')
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
      adjusted_date=(start_datetime - hours(6)) %>% floor_date("day"),
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
      color=factor(
        as.character(color_map[color]),
        levels=as.character(color_map[color_levels])),
      time_minutes=ifelse(color_num == 1, duration_1_minutes, duration_2_minutes)) %>%
    select(
      start_datetime,
      adjusted_date,
      adjusted_start_hour,
      color,
      color_num,
      time_minutes,
      total_duration_minutes,
      notes)
}

makeScatterPlot <- function(data) {
  data %>%
    ggplot(aes(x=adjusted_date, y=time_minutes, group=color, color=color)) +
    geom_jitter(size=5, height=0.5, width=0) +
    scale_color_identity() +
    theme_bw() +
    xlab("date") +
    ylab("time elapsed (minutes)") +
    ggtitle("Individual Times")
}

makeBarPlot <- function(data) {
  data %>%
    # Sort colors so smaller colors appear on lower bars
    mutate(color=factor(color, levels=rev(color_levels))) %>%
    ggplot(aes(x=adjusted_date, y=time_minutes, group=color, fill=color)) +
    geom_bar(stat='identity', position='stack') +
    scale_fill_identity() +
    theme_bw() +
    xlab("date") +
    ylab("time elapsed (minutes)") +
    ggtitle("Individual Times Combined by Date")
}

makeFacetedBarPlot <- function(data) {
  data %>%
    mutate(color_copy=color) %>%
    ggplot(aes(x=adjusted_date, y=time_minutes, fill=color_copy)) +
    geom_bar(stat='identity', position='dodge') +
    scale_fill_identity() +
    theme_bw() +
    xlab("date") +
    ylab("time elapsed (minutes)") +
    facet_grid(color ~ .) +
    ggtitle("Individual Times Separated by Color")
}

makeSummaryBarPlot <- function(data) {
  data %>%
    mutate(color_copy=color) %>%
    ggplot(aes(y=time_minutes, fill=color_copy)) +
    geom_histogram() +
    scale_fill_identity() +
    theme_bw() +
    ylab("time elapsed (minutes)") +
    facet_grid(~ color) +
    ggtitle("Time Distribution per Color")
}