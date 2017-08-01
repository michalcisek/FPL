############################################################################################################################
#################################### Scrapowanie Google Shopping ###########################################################
############################################################################################################################
## Instrukcje na podstawie https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html i pr?b i b??d?w
## 0. Updatujemy Chrome'a do najnowszej wersji
## 1. ?ci?gamy najnowszy Selenium Server (selenium-server-standalone-x.x.x.jar): http://selenium-release.storage.googleapis.com/index.html
## 2. ?ci?gamy najnowszy driver do Chrome'a https://sites.google.com/a/chromium.org/chromedriver/downloads
## 3. Instalujemy pakiet R-owy RSelenium
## 4. Uruchamiamy Selenium Server wpisujac w commandline: 
##     >> java -Dwebdriver.chrome=C:\Users\rkobiela001\Desktop\chromedriver.exe -jar selenium-server-standalone-3.4.0.jar
## 5. Po tym wszystkim skrypt powinien dzia?a? i b?dziemy mogli po??czy? si? z chromem przy pomocy rSelenium::remoteDriver
############################################################################################################################
###################################################################################################################o#########
############################################################################################################################

rm(list =ls())
if (!require('RSelenium'))
  install.packages("RSelenium")
library(RSelenium)
library(stringi)
library(dplyr)

eans_tab <- exec.query("SELECT
                       i.INDEX_ID,
                       i.EAN
                       From 
                       dict.[Index] i
                       where
                       i.EAN IN (
                       '5907610742380'
                       ,'5902600067634'
                       ,'5903570149764'
                       ,'5903570147340'
                       ,'7321908318954'
                       ,'5906619091741'
                       ,'5903570068003'
                       ,'5902600067375'
                       ,'5900058128914'
                       ,'5903560912194'
                       ,'5907561136245'
                       ,'5907561136238'
                       ,'5907610740768'
                       ,'7321909303133')
                       ")
getwd()
eans_tab <- read.csv(file='C:\\Users\\rkobiela001\\Desktop\\New folder (2)\\aaa.csv',header = FALSE, sep=';')

colnames(eans_tab) <- c('INDEX_ID','EAN')
eans_tab %>% head
eans_tab <- eans_tab[-1,]
eans_tab[1,]
class(eans_tab$V2)
eans_tab$EAN <- as.character(eans_tab[[2]])

eans_tab$EAN <- substring(eans_tab$EAN, 1,13)

eans_tab <- eans_tab %>% tail(n=320)

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName = "chrome")

url <- "https://www.google.pl/shopping"

ean <- "5903560912194"
ean <- "5906409117231"
eans_tab <- eans_tab %>% filter(EAN == 5908305210221)

remDr$open(silent = TRUE) # open web browser
remDr$getStatus()
remDr$navigate(url)

i <- 1

scrapEan <- function(i){
  #ean=i
  index_id=2222
  j <- i
  ean = eans_tab[i, 'EAN']
  index_id = eans_tab[i, 'INDEX_ID'] %>% as.character() %>%  as.numeric()
  print(i)
  print(index_id)
  tryCatch(
    {  
      webElem <- remDr$findElement(using = 'css', "input[name='q']")
      webElem$clearElement()
      webElem <- remDr$findElement(using = 'css', "input[name='q']")
      webElem$sendKeysToElement(list(ean, key = "enter"))
      Sys.sleep(1)
      Sys.sleep((rpois(1, 5)))
      tryCatch(
        {
          click <- remDr$findElement(using = 'css', "a[class='pstl']")
          click$clickElement()
          Sys.sleep(1)
          tryCatch({
            click <- remDr$findElement(using = 'css', "a[class=' _-bc']")
            click$clickElement()
          },error=function(e){}
          )
          tryCatch({
            Sys.sleep(1)
            
            click <- remDr$findElement(using = 'css', "a[class='pag-detail-link']")
            if(substr(click$getElementText(),20,27)!="recenzje") click$clickElement()
          }
          ,error=function(e){}
          ) 
          prices_table <- remDr$findElement(using = 'id', "os-sellers-content")$getElementText()
          write.csv(x = prices_table, file = 'tabela.csv')
          write(prices_table[[1]])
          prices_table <- readLines('data')
          all_shops <- list()
          
          for(i in 2:length(prices_table)){
            if (i==2){
              sklep <- gsub(" .*$",'',prices_table[2]) 
              sklep %>% print
              if(grepl('z?',prices_table[2])){
                if(grepl('[0-9]',sklep)){
                  cena <- paste(stri_extract_all_regex(stri_extract_all_regex(prices_table[2],' .*$'),
                                                       '[0-9]|,')[[1]], collapse ='')# %>% print
                }else{
                  cena <- paste(stri_extract_all_regex(prices_table[2],'[0-9]|,')[[1]], collapse ='')# %>% print
                }
              }else{
                cena <- paste(stri_extract_all_regex(gsub(") ","",stri_extract_all_regex(prices_table[3],"\\).*$")),
                                                     '[0-9]|,')[[1]], collapse ='')# %>% print
              }   
              #paste('##############',cena,sklep,i,'##########') %>% print
              all_shops[[i]] <- c(index_id,ean,gsub(',','.',cena) %>% as.double(),sklep)#  %>% print
              print('dupa1')
              sklep1 <- c(index_id,ean,gsub(',','.',cena) %>% as.double(),sklep) %>% t %>% as.data.frame()
              names(sklep1) <- c('INDEX_ID',"ean", "price", "shop")
              save.data(sklep1,'dbo.google_shopping_all', append = TRUE)
              print('dupa2')
            }
            
            if (grepl('Sklep',prices_table[i-1])){
              sklep <- gsub(" .*$",'',prices_table[i]) 
              sklep %>% print
              if(grepl('z?',prices_table[i])){
                if(grepl('[0-9]',sklep)){
                  cena <- paste(stri_extract_all_regex(stri_extract_all_regex(prices_table[i],' .*$'),
                                                       '[0-9]|,')[[1]], collapse ='') #%>% print
                }else{
                  cena <- paste(stri_extract_all_regex(prices_table[i],'[0-9]|,')[[1]], collapse ='') #%>% print
                }
              }else{
                cena <- paste(stri_extract_all_regex(gsub(") ","",
                                                          stri_extract_all_regex(prices_table[i+1],"\\).*$")), '[0-9]|,')[[1]], collapse ='') # %>% print
              }
            }
            if (grepl('Sklep',prices_table[i-1])){
              #paste('##############',cena,sklep,i,'##########') %>% print
              all_shops[[i]] <- c(index_id,ean,gsub(',','.',cena) %>% as.double(),sklep)  %>% print
              resRow <- data.frame(index_id,ean, gsub(',','.',cena) %>% as.double(),sklep)
              names(resRow) <- c('INDEX_ID',"ean", "price", "shop")
              print('##########################')
              print(resRow)
              print('##########################')
              save.data(resRow,'dbo.google_shopping_all', append = TRUE)
            }
          }
          
          
          res <- do.call(rbind, Filter(length, all_shops))
          res <- res %>% as.data.frame()
          colnames(res) <- c('INDEX_ID',"ean", "price", "shop")
          res
          
          
        }
        ,
        error=function(e){
          
          click <- remDr$findElement(using = 'css', "a[class='_-dr']")
          click$clickElement()
          Sys.sleep(rpois(1, 5))
          price.raw <- remDr$findElement(using = 'css', "span[class='price']")$getElementText()
          print(price.raw)
          price <- gsub("(\\d+)(,)(\\d+)(.*)", "\\1.\\3", price.raw) %>% as.numeric
          shop.raw <- remDr$findElement(using = 'class', "shop__secondary")$getElementText()
          
          shop <- strsplit(shop.raw[[1]], '–')[[1]][2]
          print(shop)
          resRow <- data.frame(index_id,ean, price, shop)
          names(resRow) <- c('INDEX_ID',"ean", "price", "shop")
          message("Successfully scrapped EAN=", ean)
          save.data(resRow,'dbo.google_shopping_all', append = TRUE)
          resRow}
      )
    }, error=function(e){
      tryCatch({
        err <- remDr$findElement(using = 'css', "div[class='med card-section']")$getElementText()
        shop <- gsub('\\..*',"",err)
        resRow <- data.frame(index_id,ean, NA, shop)
        names(resRow) <- c('INDEX_ID',"ean", "price", "shop")
        save.data(resRow,'dbo.google_shopping_all', append = TRUE)
        resRow
      },
      error=function(e){
        resRow <- data.frame(index_id,ean, NA,'Inny b?ad')
        names(resRow) <- c('INDEX_ID',"ean", "price", "shop")
        save.data(resRow,'dbo.google_shopping_all', append = TRUE)
        resRow
      }
      )
    })
  
  paste("################## Wykonano ",j," z ", nrow(eans_tab), " #####################") %>% print
}

res.list <- lapply(FUN = scrapEan, X = 1:nrow(eans_tab))
res <- do.call(rbind, res.list)
res
remDr$close()