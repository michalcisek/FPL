## 0. Updatujemy Chrome'a do najnowszej wersji
## 1. Sciagamy najnowszy Selenium Server (selenium-server-standalone-x.x.x.jar): 
##    http://selenium-release.storage.googleapis.com/index.html
## 2. Sciagamy najnowszy driver do Chrome'a 
##    https://sites.google.com/a/chromium.org/chromedriver/downloads
## 3. Instalujemy pakiet RSelenium
## 4. Uruchamiamy Selenium Server wpisujac w wierszu polecen: 
##    java -Dwebdriver.chrome=C:\Users\rkobiela001\Desktop\chromedriver.exe -jar selenium-server-standalone-3.4.0.jar

rm(list =ls())
# install.packages("RSelenium")
library(RSelenium)
library(tidyverse)
library(sqldf)

system('java -Dwebdriver.chrome=chromedriver.exe -jar selenium-server-standalone-3.4.0.jar', wait = F)

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName = "chrome")

url <- "https://www.premierleague.com/players"

remDr$open(silent = TRUE)
# remDr$getStatus()
remDr$navigate(url)

for(i in 1:50){
  webElem <- remDr$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "end"))
}

# Download players, position and country ----------------------------------

el <- remDr$findElements(using = 'css selector', "td")
# el <- remDr$findElements(using = 'css selector', ".indexSection")

players <- sapply(el, function(x){x$getElementText()})

players %>% 
  unlist %>% 
  matrix(nrow = 3) %>% 
  t %>% 
  as.data.frame -> players

colnames(players) <- c("Name", "Position", "Country")

#missing Position for one player
players[players$Name == "Josh Sims", "Position"] <- "Midfielder"

players %>% 
  mutate(Name = as.character(Name)) %>% 
  mutate(Position = factor(Position)) -> players

# Download links to players' profiles -------------------------------------
el <- remDr$findElements(using = 'css selector', ".playerName")

sapply(el, function(x){x$getElementAttribute('href')}) %>% unlist %>% as.character -> links
sapply(el, function(x){x$getElementText()}) %>% unlist %>% as.character -> names

links <- data.frame(names, Links = links)

# Merge data --------------------------------------------------------------

players <- sqldf("select a.*, b.Links from players a left join links b on a.Name=b.names")

players %>% 
  mutate(Links = as.character(Links)) -> players

# sapply(el, function(x){x$highlightElement()})

remDr$close()

rm(links, el, i, names, remDr, url, webElem)

saveRDS(players, "players_database.rds")
