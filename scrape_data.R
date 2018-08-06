rm(list = ls())

library(rvest)
library(stringi)
library(stringr)
library(tidyverse)
library(sqldf)
library(RecordLinkage)

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

club_dict <- readRDS("club_dict.rds")
levels(fr$Team) <- club_dict[match(levels(fr$Team), club_dict$club_name_fpl), "club_name_pl"]

players <- readRDS("players_database_clubs.rds")
names(players) <- club_dict$club_name_pl

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


lapply(1:nrow(fr), function(x) add_info(fr[x, ], players)) %>% do.call(rbind, .) -> fr1

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

colnames(fr1)[7] <- "Name_fpl"
fr1 %>% 
  select(-one_of("Points", "Cost")) -> fr1

saveRDS(fr1, "matching_pl_fpl.rds")
