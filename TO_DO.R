rm(list =ls())

library(RSelenium)
library(tidyverse)
library(sqldf)
library(jsonlite)
library(stringi)

system('java -Dwebdriver.chrome=chromedriver.exe -jar selenium-server-standalone-3.4.0.jar', wait = F)


# Download your team ------------------------------------------------------

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName = "chrome")

url <- "https://fantasy.premierleague.com/"

remDr$open(silent = TRUE)

remDr$navigate(url)

webElem <- remDr$findElement(using = "css", "#ismjs-username")
webElem$sendKeysToElement(list("michalcisek@vp.pl"))

webElem <- remDr$findElement(using = "css", "#ismjs-password")
webElem$sendKeysToElement(list("nasdaq93", key = "enter"))

remDr$navigate("https://fantasy.premierleague.com/drf/my-team/3705646/")

txt <- remDr$findElement(using='css selector', "body")$getElementText()

data <- fromJSON(unlist(txt))

remDr$close()

names(data)
data[["picks"]]

url <- "https://fantasy.premierleague.com/drf/elements/"
players <- fromJSON(url, flatten = TRUE)

possessed_players <- players[(data[["picks"]]$element), ]

# Download shots ----------------------------------------------------------

remDr$open(silent = TRUE)

url <- paste0("https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/6829/Stages/15151/TeamStatistics/",
              "England-Premier-League-", "2017-2018")
remDr$navigate(url)

webElem <- remDr$findElement(using = "css", "#stage-team-stats-options li:nth-child(4) a")
webElem$clickElement()

webElem <- remDr$findElement(using = "css", "#statsAccumulationType")
webElem$clickElement()
webElem$sendKeysToElement(list(key = "down_arrow", key = "down_arrow", key = "down_arrow", key = "enter"))

webElem <- remDr$findElements(using = "css", "#statistics-team-table-detailed")
sapply(webElem, function(x){x$getElementText()}) %>% 
  unlist %>% 
  as.character %>% 
  strsplit("\n") %>% 
  unlist -> fr

header <- fr[1]
fr <- fr[-1]

#how many words in row
fr %>% 
  strsplit(" ") %>% 
  sapply(., length) -> lens

#regex to merge team names in orders to split them later into columns
sapply(1:length(fr), 
       function(x) ifelse(lens[x] == 8, stri_replace_all_regex(fr[x], "(\\p{L}+) (\\p{L}+)", "$1_$2"),
                   ifelse(lens[x] == 9, stri_replace_all_regex(fr[x], "(\\p{L}+) (\\p{L}+) (\\p{L}+)", "$1_$2_$3"), fr[x]))) %>% 
  sapply(., strsplit, " ") %>%
  unname %>% 
  do.call(rbind, .) %>% 
  as.data.frame -> fr1

colnames(fr1) <- unlist(strsplit(header, " "))

remDr$close()

# Download goals ----------------------------------------------------------

remDr$open(silent = TRUE)

url <- "https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/6829/Stages/15151/TeamStatistics/England-Premier-League-2017-2018"
remDr$navigate(url)

webElem <- remDr$findElement(using = "css", "#stage-team-stats-options li:nth-child(4) a")
webElem$clickElement()

webElem <- remDr$findElement(using = "css", "#statsAccumulationType")
webElem$clickElement()
webElem$sendKeysToElement(list(key = "down_arrow", key = "down_arrow", key = "down_arrow", key = "enter"))

webElem <- remDr$findElement(using = "css", "#category")
webElem$clickElement()
webElem$sendKeysToElement(list(key = "down_arrow", key = "enter"))

webElem <- remDr$findElements(using = "css", "#statistics-team-table-detailed")
sapply(webElem, function(x){x$getElementText()}) %>% 
  unlist %>% 
  as.character %>% 
  strsplit("\n") %>% 
  unlist -> fr

header <- fr[1]
fr <- fr[-1]

#how many words in row
fr %>% 
  strsplit(" ") %>% 
  sapply(., length) -> lens

#regex to merge team names in orders to split them later into columns
sapply(1:length(fr), 
       function(x) ifelse(lens[x] == 8, stri_replace_all_regex(fr[x], "(\\p{L}+) (\\p{L}+)", "$1_$2"),
                          ifelse(lens[x] == 9, stri_replace_all_regex(fr[x], "(\\p{L}+) (\\p{L}+) (\\p{L}+)", "$1_$2_$3"), fr[x]))) %>% 
  sapply(., strsplit, " ") %>%
  unname %>% 
  do.call(rbind, .) %>% 
  as.data.frame -> fr1

colnames(fr1) <- unlist(strsplit(header, " "))

remDr$close()





# /bootstrap (more data if authenticated)
# /bootstrap-static
# /bootstrap-dynamic

# /events
# /elements
# /element-types
# /fixtures
# /fixtures/?event={gameweek-number}
# /teams
# /region
# /transfers (requires auth)
# /entry/{entryId}
# /entries (doesn't work in pre-season?)
# /my-team/{teamId} (requires auth)
# /leagues-entered/{teamId} (requires auth)
# /leagues-classic/{leagueId} (must be a member)
# /leagues-classic-standings/{leagueId}
# /leagues-h2h/{leagueId} (must be a member)
# /leagues-h2h-standings/{leagueId}