library(RSelenium)
library(dplyr)  
library(stringr)


wiki_movies <- all_movies_table[7657:11444,c(1,16,9)]
wiki_movies$newId <- NA
wiki_movies2 <- wiki_movies

driver <- rsDriver(browser=c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$open()


remote_driver$navigate("https://www.google.com/")


for (i in 2458:length(wiki_movies$MoiveTitle)){
  
  if (is.na(wiki_movies$ID[i]) == TRUE){
    print(i)
    if (str_detect(wiki_movies$MoiveTitle[i], "release")){
      next()
    }
    address_element <- remote_driver$findElement(using = 'name', value = 'q')
    address_element$clearElement()
    query <- paste0(wiki_movies$MoiveTitle[i], " movie ", wiki_movies$year[i])
    address_element$sendKeysToElement(list(query))
    
    button_element <- remote_driver$findElement(using = 'class', value = "Tg7LZd")
    button_element$clickElement()
    
    # Sys.sleep(0.5)
    
    link <- remote_driver$findElements(using = "class", value="TbwUpd")
    for (j in 1:5){
      if (str_detect(link[[j]]$getElementText(), "en.wikipedia.org › wiki ›")){
        wiki_movies$newId[i] <- link[[j]]$getElementText()
      }
    }
    
  }
    
}





