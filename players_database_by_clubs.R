## 0. Updatujemy Chrome'a do najnowszej wersji
## 1. Sciagamy najnowszy Selenium Server (selenium-server-standalone-x.x.x.jar): 
##    http://selenium-release.storage.googleapis.com/index.html
## 2. Sciagamy najnowszy driver do Chrome'a 
##    https://sites.google.com/a/chromium.org/chromedriver/downloads
## 3. Instalujemy pakiet RSelenium
## 4. Uruchamiamy Selenium Server wpisujac w wierszu polecen: 
##    java -Dwebdriver.chrome=C:\Users\mcisek001\Desktop\chromedriver.exe -jar selenium-server-standalone-3.4.0.jar

rm(list =ls())
# install.packages("RSelenium")
library(RSelenium)
library(tidyverse)
library(sqldf)
library(pbapply)

system('java -Dwebdriver.chrome=chromedriver.exe -jar selenium-server-standalone-3.9.1.jar', wait = F)

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName = "chrome")

club_dict <- readRDS("club_dict.rds")

#update url inside function - se=210 - different number
scrape_players <- function(id, remDr){

  url <- paste0("https://www.premierleague.com/players/?se=274&cl=", id)
  
  # remDr$getStatus()
  remDr$navigate(url)
  
  for(i in 1:5){
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
  }
  
  # Download players, position and country ----------------------------------
  Sys.sleep(2)
  el <- remDr$findElements(using = 'css selector', "td")
  Sys.sleep(2)
  # el <- remDr$findElements(using = 'css selector', ".indexSection")
  
  players <- sapply(el, function(x){x$getElementText()})
  Sys.sleep(2)
  
  players %>% 
    unlist %>% 
    matrix(nrow = 3) %>% 
    t %>% 
    as.data.frame -> players
  
  colnames(players) <- c("Name", "Position", "Country")

  # #missing Position for one player
  # if(id == 20){
  #   players[players$Name == "Josh Sims", "Position"] <- "Midfielder"
  # }
  
  players %>% 
    mutate(Name = as.character(Name)) %>% 
    mutate(Position = factor(Position)) -> players
  
  # Download links to players' profiles -------------------------------------
  el <- remDr$findElements(using = 'css selector', ".playerName")
  Sys.sleep(2)
  
  sapply(el, function(x){x$getElementAttribute('href')}) %>% unlist %>% as.character -> links
  Sys.sleep(2)
  sapply(el, function(x){x$getElementText()}) %>% unlist %>% as.character -> names
  Sys.sleep(2)
  
  links <- data.frame(names, Links = links)
  
  # Merge data --------------------------------------------------------------
  
  players <- sqldf("select a.*, b.Links from players a left join links b on a.Name=b.names")
  
  players %>% 
    mutate(Links = as.character(Links)) -> players
  
  return(players)
}

remDr$open(silent = TRUE)
players_by_clubs <- lapply(club_dict$club_id_pl, function(x) scrape_players(x, remDr))
remDr$close()

rm(remDr)

saveRDS(players_by_clubs, "players_database_clubs.rds")
