rm(list = ls())
url <- "https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/6829/Stages/15151/TeamStatistics/England-Premier-League-2017-2018"

css <- list()
options <- list()

#choose season
css[["seasons"]] <- '#seasons'

#choose what type of statistics to show - summary, offensive, defensive, detailed
css[["types"]] <- data.frame(type = c('Summary', 'Defensive', 'Offensive', 'Detailed'),
                        css = c('#stage-team-stats-options li:nth-child(1) a', 
                                '#stage-team-stats-options li:nth-child(2) a',
                                '#stage-team-stats-options li:nth-child(3) a', 
                                '#stage-team-stats-options li:nth-child(4) a'))

#if detailed then choose category 
css[["dtld_cat"]] <- '#category'
options[["dtld_cat"]] <- data.frame(type = c(rep('Defensive', 8), rep('Offensive', 5), rep('Passing', 3)),
                                    subtype = c('Tackles', 'Interception', 'Fouls', 'Cards', 'Offsides', 'Clearances', 
                                                'Blocks', 'Saves', 'Shots', 'Goals', 'Dribbles', 'Possession loss', 
                                                'Aerial', 'Passes', 'Key passes', 'Assists'),
                                    location = c(rep(0, 8), 1, rep(0, 7)))


#if you choose category then select subcategory
css[["dtld_subcat"]] <- '#subcategory'
options[["dtld_subcat"]] <- data.frame(type = c(rep('Shots', 4), rep('Goals', 3), rep('Passes', 2), 
                                                rep('Key passes', 2)),
                                       subtype = c('Zones', 'Situations', 'Accuracy', 'Body Parts', 'Zones', 
                                                   'Situations', 'Body Parts', 'Length', 'Type', 'Length', 
                                                   'Type'),
                                       location = c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0))


#chose frequency of statistics - per game or total?
css[["dtld_accm"]] <- '#statsAccumulationType'
options[["dtld_accm"]] <- data.frame(type = c('Per Game', 'Per 90 Minutes', 'Every X Minute', 'Total'),
                                     location = c(1, 0, 0, 0))




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















