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


system("java -Dwebdriver.chrome=C:\\Users\\mcisek001\\Documents\\selenium\\chromedriver.exe 
       -jar C:\\Users\\mcisek001\\Documents\\selenium\\selenium-server-standalone-3.4.0.jar")


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

el <- remDr$findElements(using = 'css selector', "td")


el$getElementText()

el$getElementAttribute()
el$highlightElements()



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
