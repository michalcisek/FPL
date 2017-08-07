library(jsonlite)
url <- "https://fantasy.premierleague.com/drf/elements/"
ff <- fromJSON(url, flatten = TRUE)


url <- "https://fantasy.premierleague.com/drf/bootstrap-static"
l <- fromJSON(url, flatten = TRUE)
names(l)
l[["game-settings"]]
identical(l$elements, ff)
ff1 <- l$elements
