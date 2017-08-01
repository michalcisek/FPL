library(rvest)
library(stringi)
library(stringr)
library(tidyverse)

url <- "https://fantasy.premierleague.com/player-list/"
html <- read_html(url)

html %>% 
  # html_nodes('td') %>%
  html_nodes('.col-6') %>% 
  html_text() %>% 
  str_replace_all("[\r\n]" , "") %>% 
  matrix(., nrow = 4) %>% 
  t %>% 
  as.data.frame() -> fr

colnames(fr) <- c("Name", "Team", "Points", "Cost")

fr %>% 
  mutate(Name = as.character(Name)) %>% 
  mutate(Team = as.factor(Team)) %>% 
  mutate(Points = as.numeric(as.character(Points))) %>% 
  mutate(Cost = as.character(Cost)) %>% 
  mutate(Cost = substr(Cost, 2, nchar(Cost))) %>% 
  mutate(Cost = as.numeric(Cost))
  
  
url <- "https://www.premierleague.com/players?page=3"
url <- "https://footballapi.pulselive.com/football/players?pageSize=30&compSeasons=79&altIds=true&page=1&type=player&id=-1&compSeasonId=79"
html <- read_html(url)

html %>% 
  html_nodes('td') %>% 
  html_text() %>%
  str_replace_all("[\r\n]" , "") %>% 
  gsub("^\\s+|\\s+$", "", .)
  


