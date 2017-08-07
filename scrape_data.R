rm(list = ls())

library(rvest)
library(stringi)
library(stringr)
library(tidyverse)
library(sqldf)

url <- "https://fantasy.premierleague.com/player-list/"
html <- read_html(url)

html %>% 
  html_nodes('td') %>%
  # html_nodes('.col-6') %>% 
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
  mutate(Cost = as.numeric(Cost)) -> fr
  
levels(fr$Team)[c(2, 3, 8, 9, 11, 12, 13, 15, 16, 17, 19, 20)] <- c("AFC Bournemouth", "Brighton and Hove Albion", 
                                         "Huddersfield Town", "Leicester City", "Manchester City", "Manchester United", 
                                         "Newcastle United", "Tottenham Hotspur", "Stoke City", "Swansea City", 
                                         "West Bromwich Albion", "West Ham United")

players <- readRDS("players_database_clubs.rds")
names(players) <- c("Arsenal", "AFC Bournemouth", "Brighton and Hove Albion", "Burnley", "Chelsea",                 
                "Crystal Palace", "Everton", "Huddersfield Town", "Leicester City", "Liverpool",
                "Manchester City", "Manchester United", "Newcastle United", "Southampton",        
                "Stoke City", "Swansea City", "Tottenham Hotspur", "Watford", "West Bromwich Albion", 
                "West Ham United")  

add_info <- function(row, add_data){
  df <- add_data[[as.character(row$Team)]]
  str_split_fixed(df$Name, " ", 2) %>% 
    cbind(., df[, -which(colnames(df) == "Name")]) -> df  
  colnames(df)[1:2] <- c("Name", "Surname")
  
  sapply(as.character(df$Surname), function(x) levenshteinSim(x, row$Name)) %>% 
    sort(decreasing = T) -> similarities
  
  #jesli identyczne nazwisko
  if(similarities[1] == 1){
    name <- names(similarities[1])
    
    return(df[df$Surname == name, ])
  #jesli nie to zwraca najbardziej podobne wg miary odleglosci pomiedzy dwoma slowami  
  } else{
    sapply(as.character(df$Name), function(x) levenshteinSim(x, row$Name)) %>% 
      sort(decreasing = T) -> similarities1
    
    if(similarities[1] > similarities1[1]){
      name <- names(similarities[1])
      
      return(df[df$Surname == name, ])
    } else{
      name <- names(similarities1[1])
      
      return(df[df$Name == name, ])
    }  
  }
}

install.packages('RecordLinkage')
library(RecordLinkage)

lapply(1:nrow(fr), function(x) add_info(fr[x, ], players)) %>% do.call(rbind, .) -> fr1
add_info(fr[1, ], players)


players <- readRDS("players_database.rds")

str_split_fixed(players$Name, " ", 2) %>% 
  cbind(., players[, -which(colnames(players) == "Name")]) -> players
colnames(players)[1:2] <- c("Name", "Surname")


get_club <- function(url){
  html <- read_html(url)
  
  html %>% 
    html_nodes('.info a') %>% 
    html_text %>% 
    str_replace_all("[\r\n]" , "") %>% 
    str_trim() 
}

clubs <- sapply(players$Links, get_club)
clubs %>% 
  lapply(., function(x) if(identical(x, character(0))) NA else x) %>% 
  unlist %>% 
  unname -> clubs

players %>% 
  mutate(Clubs = factor(clubs)) -> players

fr1 <- sqldf("select * from players a left join fr b on (a.Name=b.Name and a.Clubs=b.Team) or 
                                                        (a.Surname=b.Name and a.Clubs=b.Team)")

nrow(fr1) - sum(is.na(fr1$Points))



