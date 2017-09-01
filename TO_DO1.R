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
                                '#stage-team-stats-options li:nth-child(4) a'),
                        table_css = c('#top-team-stats-summary-content td',
                                      '#top-team-stats-summary-content td',
                                      '#statistics-team-table-offensive',
                                      '#statistics-team-table-detailed'))
options[["types"]] <- data.frame(type = c('Summary', 'Defensive', 'Offensive', 'Detailed'),
                                 location = c(1, 0, 0, 0),
                                 default_location = c(1, 0, 0, 0))

#if detailed then choose category 
css[["dtld_cat"]] <- '#category'
options[["dtld_cat"]] <- data.frame(type = c(rep('Defensive', 8), rep('Offensive', 5), rep('Passing', 3)),
                                    subtype = c('Tackles', 'Interception', 'Fouls', 'Cards', 'Offsides', 'Clearances', 
                                                'Blocks', 'Saves', 'Shots', 'Goals', 'Dribbles', 'Possession loss', 
                                                'Aerial', 'Passes', 'Key passes', 'Assists'),
                                    location = c(rep(0, 8), 1, rep(0, 7)),
                                    default_location = c(rep(0, 8), 1, rep(0, 7)))


#if you choose category then select subcategory
css[["dtld_subcat"]] <- '#subcategory'
options[["dtld_subcat"]] <- data.frame(type = c(rep('Shots', 4), rep('Goals', 3), rep('Passes', 2), 
                                                rep('Key passes', 2)),
                                       subtype = c('Zones', 'Situations', 'Accuracy', 'Body Parts', 'Zones', 
                                                   'Situations', 'Body Parts', 'Length', 'Type', 'Length', 
                                                   'Type'),
                                       location = c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0),
                                       default_location = c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0))


#chose frequency of statistics - per game or total?
css[["dtld_accm"]] <- '#statsAccumulationType'
options[["dtld_accm"]] <- data.frame(type = c('Per Game', 'Per 90 Minutes', 'Every X Minute', 'Total'),
                                     location = c(1, 0, 0, 0),
                                     default_location = c(1, 0, 0, 0))

library(RSelenium)
system('java -Dwebdriver.chrome=chromedriver.exe -jar selenium-server-standalone-3.4.0.jar', wait = F)

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName = "chrome")

change_type <- function(css, options, remDr){
  css1 <- css[["types"]]
  options1 <- options[["types"]]
  
  if(options1$location[length(options1$location)] == 1){
    return(FALSE)
  } else{
    slctd_css <- css1[css1$type == options1[which(options1$location == 1) + 1, "type"], "css"] 
    webElem <- remDr$findElement(using = "css", slctd_css)
    webElem$clickElement()
    
    options[["types"]]$location <<- c(0, options[["types"]]$location[1:(length(options[["types"]]$location) - 1)])
    return(TRUE)
  }
}

convert_table <- function(tbl){
  sapply(tbl, function(x){x$getElementText()}) %>% 
    unlist %>% 
    as.character %>% 
    strsplit("\n") %>% 
    unlist -> tbl
  
  header <- tbl[1]
  tbl <- tbl[-1]
  
  #how many words in row
  tbl %>% 
    strsplit(" ") %>% 
    sapply(., length) -> lens
  
  #regex to merge team names in orders to split them later into columns
  sapply(1:length(tbl), 
         function(x) ifelse(lens[x] == (max(lens) - 1), stri_replace_all_regex(tbl[x], "(\\p{L}+) (\\p{L}+)", "$1_$2"),
                            ifelse(lens[x] == max(lens), stri_replace_all_regex(tbl[x], "(\\p{L}+) (\\p{L}+) (\\p{L}+)", "$1_$2_$3"), tbl[x]))) %>% 
    sapply(., strsplit, " ") %>%
    unname %>% 
    do.call(rbind, .) %>% 
    as.data.frame -> tbl1
  
  colnames(tbl1) <- unlist(strsplit(header, " "))
  
  return(tbl1)
}

change_category <- function(css, options, remDr, type){
  css1 <- css[["dtld_cat"]]
  options1 <- options[["dtld_cat"]]
  
  type_df <- options1[which(options1$type == type), ]

  if(1 %in% type_df$location & type_df[nrow(type_df), "location"] == 0){
    webElem <- remDr$findElement(using = "css", css1)
    webElem$clickElement()
    webElem$sendKeysToElement(list(key = "down_arrow", key = "enter"))
    
    curr_pos <- which(options[["dtld_cat"]]$location == 1)
    options[["dtld_cat"]][curr_pos, "location"] <<- 0
    options[["dtld_cat"]][curr_pos + 1, "location"] <<- 1
  } else{
    trgt_pos <- which(sapply(1:nrow(options1), function(x) identical(options1[x, ], type_df[1, ])))
    curr_pos <- which(options[["dtld_cat"]]$location == 1)
    diff_pos <- curr_pos - trgt_pos
    
    if(diff_pos < 0){
      webElem <- remDr$findElement(using = "css", css1)
      webElem$clickElement()
      cmds <- paste0(paste0(rep("key = 'down_arrow', ", abs(diff_pos)), collapse = ""), "key = 'enter'")
      eval(parse(text = paste0('webElem$sendKeysToElement(list(', cmds, '))')))
        
      options[["dtld_cat"]][curr_pos, "location"] <<- 0
      options[["dtld_cat"]][trgt_pos, "location"] <<- 1        
    } else{
      webElem <- remDr$findElement(using = "css", css1)
      webElem$clickElement()
      cmds <- paste0(paste0(rep("key = 'up_arrow', ", abs(diff_pos)), collapse = ""), "key = 'enter'")
      eval(parse(text = paste0('webElem$sendKeysToElement(list(', cmds, '))')))
      
      options[["dtld_cat"]][curr_pos, "location"] <<- 0
      options[["dtld_cat"]][trgt_pos, "location"] <<- 1        
    }
  }
}

change_subcategory <- function(css, options, remDr, type){
  css1 <- css[["dtld_subcat"]]
  options1 <- options[["dtld_subcat"]]
  type_df <- options1[which(options1$type == type), ]
  
  if(type %in% options[["dtld_subcat"]]$type & type_df$location[nrow(type_df)] != 1){
    
    webElem <- remDr$findElement(using = "css", css1)
    webElem$clickElement()
    webElem$sendKeysToElement(list(key = "down_arrow", key = "enter"))

    crnt_pos <- which(options[["dtld_subcat"]]$location == 1 & options[["dtld_subcat"]]$type == type)
    options[["dtld_subcat"]]$location[crnt_pos] <<- 0
    options[["dtld_subcat"]]$location[crnt_pos + 1] <<- 1
    
  } else if(type_df$location[nrow(type_df)] == 1){
    return("Already done")
  } else{
    return("Not applicable")
  }
}

change_accumulation <- function(css, options, remDr, accm){
  css1 <- css[["dtld_accm"]]
  options1 <- options[["dtld_accm"]]
  
  crnt_pos <- which(options1$location == 1)
  trgt_pos <- which(options1$type == accm)
  
  if(crnt_pos > trgt_pos){
    webElem <- remDr$findElement(using = "css", "#statsAccumulationType")
    webElem$clickElement()
    
    cmds <- paste0(paste0(rep("key = 'up_arrow', ", crnt_pos - trgt_pos), collapse = ""), "key = 'enter'")
    eval(parse(text = paste0("webElem$sendKeysToElement(list(", cmds, "))")))
    
    options[["dtld_accm"]]$location[crnt_pos] <<- 0
    options[["dtld_accm"]]$location[trgt_pos] <<- 1
  } else if(crnt_pos < trgt_pos){
    webElem <- remDr$findElement(using = "css", "#statsAccumulationType")
    webElem$clickElement()
    
    cmds <- paste0(paste0(rep("key = 'down_arrow', ", abs(crnt_pos - trgt_pos)), collapse = ""), "key = 'enter'")
    eval(parse(text = paste0("webElem$sendKeysToElement(list(", cmds, "))")))    
    
    options[["dtld_accm"]]$location[crnt_pos] <<- 0
    options[["dtld_accm"]]$location[trgt_pos] <<- 1
  } else{
    return("Already done")
  }
}

  
remDr$open(silent = TRUE)

remDr$navigate(url)
Sys.sleep(runif(1, 3, 6))

change_type(css, options, remDr = remDr)
change_type(css, options, remDr = remDr)
change_type(css, options, remDr = remDr)

change_category(css, options, remDr, "Offensive")
change_category(css, options, remDr, "Passing")


webElem <- remDr$findElements(using = "css", css[["types"]]$table_css[4])
convert_table(webElem)

change_subcategory(css, options, remDr, "Goals")
change_accumulation(css, options, remDr, "Per Game")

remDr$close()


