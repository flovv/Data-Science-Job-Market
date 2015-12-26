



require(rvest)

#mov <- html("http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&ns=1&qs=%5B%5D&companyID=0&cityID=0&sourceOfTheSearchField=homepage%3Ageneral&ke=data+scientist&ws=&ra=0")

#http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&ns=1&qs=%5B%5D&companyID=0&cityID=0&sourceOfTheSearchField=homepage%3Ageneral&ke=data+scientist&ws=&ra=0"
#http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&li=25&thr=-1&of=25&ke=data%20scientist&an=paging_next&=undefined&suid=d78dbd35-1af7-4d14-a626-a0606c296d3a
require(plyr)

require(stringr)

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

scrapeStepStoneFull <- function(){
  urlStepStone <- "http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&ke=data%20scientist"
  dfStart <- scrapeStepstone(urlStepStone)
  
  for(i in seq(25,100,25)){
    urlStepStone2 <- paste0(urlStepStone, "&li=",i,"&thr=-1&of=",i)
    dfsub <- scrapeStepstone(urlStepStone2)
    dfStart <- rbind(dfStart, dfsub)
  }
 return(dfStart)
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

scrapeMonsterFull <- function(){
  urlMonster <- "http://jobsuche.monster.de/jobs/?q=%22data%20scientist%22"
  dfstart <- scrapeMonster(urlMonster)
 
  urlMonster2 <- "http://jobsuche.monster.de/jobs/?q=%22data%20scientist%22&pg=2"
  dfsub <- scrapeMonster(urlMonster2)
  
 return(rbind(dfstart,dfsub))
}

scrapeIndeed <- function(url3){
  mov <- html(url3)
  
  City <- mov %>% html_nodes(".location") %>% html_text()
  
  Date <- mov %>% html_nodes(".date") %>% html_text()
  
  Company<- mov %>% html_nodes(".company") %>% html_text()
  
  return(data.frame(Company, Date, City))
}


scrapeIndeedFull <- function(){
  urlIndeed <- "http://de.indeed.com/Jobs?q=+Data+Scientist"
  dfstart <- scrapeIndeed(urlIndeed)
  
  for(i in seq(10,100,10)){
    urlIndeed2 <- paste0(urlIndeed, "&=",i)
    dfsub <- scrapeIndeed(urlIndeed2)
    dfstart <- rbind(dfstart, dfsub)
  }
  return(dfstart)
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

write.csv2(out, paste0("data/",as.Date(Sys.time()),"-JobSearch.csv"))



################################


outIndeed <- scrapeIndeedFull()
write.csv2(out, paste0("",as.Date(Sys.time()),"-JobSearch-Indeed.csv"))


outMonster <- scrapeMonsterFull()
dfsub <- data.frame(City, Company[-1], Date[-1])
colnames(dfsub) <- c("City", "Company", "Date")
outMonster <- rbind(dfstart,dfsub)

write.csv2(outMonster, paste0("",as.Date(Sys.time()),"-JobSearch-Monster.csv"))

outStepStone <- scrapeStepStoneFull()
write.csv2(outMonster, paste0("",as.Date(Sys.time()),"-JobSearch-Stepstone.csv"))



###### count job positions monster!

getMonsterPositions <- function(term){
  term <- URLencode(term)

  url <- paste0("http://jobsuche.monster.de/jobs/?q=%22", term ,"%22&sort=rv.di.dt")
  
  
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#resultsCountHeader .fnt12") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), " ",2)[,1]
  numPositions <- str_replace_all(numPositions, "+", "")
  return(numPositions)
}

getIndeedPositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://de.indeed.com/Jobs?q=", term)
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#searchCount") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), "von ",2)[,2]
  numPositions <- str_replace_all(numPositions, "\\.", "")
  return(numPositions)
}


getStepstonePositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://www.stepstone.de/5/ergebnisliste.html?stf=freeText&ke=", term)

  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#numberResults") %>% html_text()
  
  return(numPositions)

}


getAllPositions <- function(terms){
 
 
  
  o <- llply(terms, getMonsterPositions)  
  jobs <- data.frame(cbind(terms, unlist(o)))
  
  o <- llply(terms, getIndeedPositions)  
  jobs <- cbind(jobs, unlist(o))
    
  o <- llply(terms, getStepstonePositions)  
  jobs <- cbind(jobs, unlist(o))
  colnames(jobs) <- c("Term", "Monster", "Indeed", "Stepstone")
  return(jobs)
}


terms <- c("data scientist", "data analyst", "trader", "SAP ERP", "SEO", "machine learning" )

o <- llply(terms, getMonsterPositions)
unlist(o)

jobs <- data.frame(cbind(terms, unlist(o)))
jobs$V2 <- as.numeric(jobs$V2)

write.csv2(jobs,"jobs.csv")
ggplot(jobs, aes(terms, V2)) + geom_bar(stat="identity") + xlab("Search Terms") + ylab("open positions") + theme_economist()

getIndeedPositions("Big Data")
getIndeedPositions("SVN")
###############################
## languages
terms <- c("SAS", "SPSS", "Stata", "Python", "Matlab", "Java",  "Scala",  "D3.js", "VBA")

#### method
method <-c("Regression", "Neuronale netze", "Web Scraping", "API", "Text Mining" )


## coding environment
enivron <- c("Scrum",  "git", "JIRA" , "Eclipse")


BigD <- c("Spark", "Hadoop", "Mahout", "Hive", "Pig", "MapReduce")


## BI
bi <- c("Excel", "Qlik", "SQL", "Tableau", "ETL")


### buzzwords
buzzwords <- c("Cloud","Data Mining", "Predictive Analytics",  "Machine Learning" , "Big Data")


out <- getAllPositions(terms)
write.csv2(out, "topSkills.csv")

t2 <- c(terms, method, enivron, BigD, bi, buzzwords)
out2 <- getAllPositions(t2)

write.csv2(out2, "moreSkills.csv")

out2$Category <- ifelse(out2$Term %in% buzzwords, "Buzzwords", "Language" )
out2$Category <- ifelse(out2$Term %in% bi, "Business Intelligence", out2$Category )

out2$Category <- ifelse(out2$Term %in% bi, "Business Intelligence", out2$Category )
out2$Category <- ifelse(out2$Term %in% BigD, "Technology", out2$Category )

out2$Category <- ifelse(out2$Term %in% method, "Method", out2$Category )
out2$Category <- ifelse(out2$Term %in% enivron, "Environment", out2$Category )


o <- llply(terms, getMonsterPositions)


skills <- data.frame(cbind(terms, unlist(o)))
skills$V2 <- as.numeric(skills$V2)

############################### cross country comparision

getGermanIndeedPositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://de.indeed.com/Jobs?q=", term)
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#searchCount") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), "von ",2)[,2]
  numPositions <- str_replace_all(numPositions, "\\.", "")
  return(numPositions)
}
#####
getUKIndeedPositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://www.indeed.co.uk/jobs?q=", term) #http://www.indeed.co.uk/jobs?q=%22data+scientist%22
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#searchCount") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), "of ",2)[,2]
  numPositions <- str_replace_all(numPositions, "\\.", "")
  return(numPositions)
}
#####
getDutchIndeedPositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://www.indeed.nl/vacatures?q=", term)
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#searchCount") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), "van ",2)[,2]
  numPositions <- str_replace_all(numPositions, "\\.", "")
  return(numPositions)
}
## Sweden http://se.indeed.com/jobb?q=data+scientist&l=
getSwedishIndeedPositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://se.indeed.com/jobb?q=", term)
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#searchCount") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), "av ",2)[,2]
  numPositions <- str_replace_all(numPositions, "\\.", "")
  return(numPositions)
}
###http://www.indeed.fr/emplois?q=%22data+scientist%22
getFrenchIndeedPositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://www.indeed.fr/emplois?q=", term)
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#searchCount") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), "sur ",2)[,2]
  numPositions <- str_replace_all(numPositions, "\\.", "")
  return(numPositions)
}
#######
getAustrianIndeedPositions <- function(term){
  term <- URLencode(term)
  term <- paste0("%22", term, "%22")
  url <- paste0("http://at.indeed.com/Jobs?q=", term)
  mov <- html(url)  
  numPositions <- mov %>% html_nodes("#searchCount") %>% html_text()
  numPositions <- str_split_fixed(str_trim(numPositions), "von ",2)[,2]
  numPositions <- str_replace_all(numPositions, "\\.", "")
  return(numPositions)
}


#############################

getAllCountries <- function(term){
  Countries <- c("AT", "FR", "SW", "NL", "UK", "DE")

  Positions <- c(getAustrianIndeedPositions(term), getFrenchIndeedPositions(term))
  
  Positions <- c(Positions, getSwedishIndeedPositions(term))
  Positions <- c(Positions, getDutchIndeedPositions(term))
  Positions <- c(Positions,  getUKIndeedPositions(term) )
  Positions <- c(Positions,    getGermanIndeedPositions(term) )
  
  Positions <- str_replace_all(Positions, " ", "")
  
  df <- data.frame(Countries, Positions)
  return(df)
}

Excel <- getAllCountries("Excel")
DS <- getAllCountries("Data Scientist")


Excel$Term <- "Excel"
DS$Term <- "Data Scientist"

write.csv2(rbind(Excel, DS), "Country_comparison.csv")
