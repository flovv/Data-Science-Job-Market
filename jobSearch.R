



require(rvest)

#mov <- html("http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&ns=1&qs=%5B%5D&companyID=0&cityID=0&sourceOfTheSearchField=homepage%3Ageneral&ke=data+scientist&ws=&ra=0")

#http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&ns=1&qs=%5B%5D&companyID=0&cityID=0&sourceOfTheSearchField=homepage%3Ageneral&ke=data+scientist&ws=&ra=0"
#http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&li=25&thr=-1&of=25&ke=data%20scientist&an=paging_next&=undefined&suid=d78dbd35-1af7-4d14-a626-a0606c296d3a




scrapeStepstone <- function(url){
  
  mov <- html(url)
  
  ### works!
 Company <-  mov %>% html_nodes(".company_name span") %>% html_text()
  
 Date <-  mov %>% html_nodes("time") %>% html_text()
  
 City <-   mov %>% html_nodes(".job_location_info span") %>% html_text()
  
# Job <- mov %>% html_nodes("#resultlist_list span") %>% html_text()
 
# mov %>% html_nodes(".job_title span") %>% html_text()
    
 return(data.frame(Company, Date, City))
}


scrapeMonster <- function(url2){


#url2 <- "http://jobsuche.monster.de/Jobs/?q=data-scientist&intcid=swoop_HeroSearch&cy=de"
    mov <- html(url2)
    
    City <- mov %>% html_nodes(".jobLocationSingleLine a") %>% html_text()
    
    Date <- mov %>% html_nodes(".fnt4 .fnt20") %>% html_text()
    
    Company<- mov %>% html_nodes(".companyContainer a") %>% html_text()
    
    Company <- Company[nchar(Company)>0]
    
    return(data.frame(Company, Date, City))
}

scrapeIndeed <- function(url3){
  mov <- html(url3)
  
  City <- mov %>% html_nodes(".location") %>% html_text()
  
  Date <- mov %>% html_nodes(".date") %>% html_text()
  
  Company<- mov %>% html_nodes(".company") %>% html_text()
  
  return(data.frame(Company, Date, City))
}

urlStepStone <- "http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&ns=1&qs=%5B%5D&companyID=0&cityID=0&sourceOfTheSearchField=homepage%3Ageneral&ke=data+scientist&ws=&ra=0"
urlMonster <-  "http://jobsuche.monster.de/Jobs/?q=data-scientist&intcid=swoop_HeroSearch&cy=de"
urlIndeed <- "http://de.indeed.com/Jobs?q=+Data+Scientist&l="

df1 <- scrapeStepstone(urlStepStone)
df1$ScrapeDate <- as.Date(Sys.time())
df1$Site <- "Stepstone"

df2 <- scrapeMonster(urlMonster)
df2$ScrapeDate <- as.Date(Sys.time())
df2$Site <- "Monster"

df3 <- scrapeIndeed(urlIndeed)
df3$ScrapeDate <- as.Date(Sys.time())
df3$Site <- "Indeed"

out <- rbind(df1, df2)
out <- rbind(out, df3)

write.csv2(out, paste0(as.Date(Sys.time()),"-JobSearch.csv"))












