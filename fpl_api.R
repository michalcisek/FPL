rm(list = ls())
library(jsonlite)

url <- "https://fantasy.premierleague.com/drf/elements/"

data <- fromJSON(url, flatten = TRUE)


url <- "https://fantasy.premierleague.com/drf/bootstrap-static"
l <- fromJSON(url, flatten = TRUE)
names(l)
l[["game-settings"]]


l$`game-settings`
events <- l$events
l$element_types
teams <- l$teams

