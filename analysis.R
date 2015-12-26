



require(ggplot2)


require(stringr)
library(plyr)
require(reshape2)
require(ggthemes)

require(lubridate)
options(stringsAsFactors = FALSE)


indeed <- read.csv2("2015-12-17-JobSearch-Indeed.csv")

indeed$Company <- str_trim(str_replace_all(indeed$Company, "\n", ""))
indeed$firstCity <- str_split_fixed(indeed$City, ",", 2)[,1]
indeed$firstCity <- str_split_fixed(indeed$firstCity, " ", 2)[,1]


############ DATE

  
indeed$Date_str <- ifelse(grepl("Tage", indeed$Date), indeed$Date, 0)
indeed$Date_str <- str_replace_all(indeed$Date_str, "Veröffentlicht:", "")

indeed$Date_str <- str_trim(str_replace_all(indeed$Date_str, "\n", ""))
indeed$Date_str <- str_split_fixed(indeed$Date_str, " ", 3)[,2]
indeed$Date_str <- str_replace_all(indeed$Date_str, "\\+", "")

indeed$Date_str = as.Date(as.Date("2015-12-17")-as.numeric(indeed$Date_str))

indeed$date <- as.Date(indeed$Date, "%d.%m.%y")

indeed[is.na(indeed$date),]$date <- as.Date(indeed[is.na(indeed$date),]$Date_str)
indeed[is.na(indeed$date),]$date <- indeed[is.na(indeed$date),]$ScrapeDate

###############################################################
indeed <- indeed[, c("date", "Company", "firstCity")]


#############################################################
stepstone <- read.csv2("2015-12-17-JobSearch-Stepstone.csv")

stepstone$Date <- str_replace_all(stepstone$Date, "Veröffentlicht:", "")
stepstone$Date <- str_trim(str_replace_all(stepstone$Date, "\n", ""))
stepstone$Date <- str_split_fixed(stepstone$Date, " ", 3)[,2]
stepstone$date = as.Date(as.Date("2015-12-17")-as.numeric(stepstone$Date))
stepstone[is.na(stepstone$date),]$date <- as.Date("2015-12-17")

stepstone$firstCity <- str_split_fixed(stepstone$City, ",", 2)[,1]
stepstone$firstCity <- str_split_fixed(stepstone$firstCity, " ", 2)[,1]

stepstone$Company <- str_trim(str_replace_all(stepstone$Company, "\n", ""))

stepstone <- stepstone[, c("date", "Company", "firstCity")]
#############################################################

monster <- read.csv2("2015-12-17-JobSearch-Monster.csv")

monster$Date <- str_replace_all(monster$Date, "Veröffentlicht:", "")
monster$Date <- str_trim(str_replace_all(monster$Date, "\n", ""))
monster$Date <- str_split_fixed(monster$Date, " ", 3)[,2]
monster$date = as.Date(as.Date("2015-12-17")-as.numeric(monster$Date))
monster[is.na(monster$date),]$date <- as.Date("2015-12-17")

monster$firstCity <- str_split_fixed(monster$City, ",", 2)[,1]
monster$firstCity <- str_split_fixed(monster$firstCity, " ", 2)[,1]

monster$Company <- str_trim(str_replace_all(monster$Company, "\n", ""))

monster <- monster[, c("date", "Company", "firstCity")]

###################################################################################

monster$Platform <- "Monster"
indeed$Platform <- "Indeed"
stepstone$Platform <- "Stepstone"

all <- rbind(monster, indeed)
all <- rbind(all, stepstone)

write.csv(all, "all.csv")

all$weekday <- weekdays(all$date, abbreviate = T)

ddply(all, .(Company), summarise, N=length(Company))
ddply(all, .(firstCity), summarise, N=length(firstCity))

### 6.1 days
all$diff <- as.Date("2015-12-17") - all$date
summary(as.numeric(all$diff))

dd <- ddply(all, .(date), summarise, N=length(firstCity))

ggplot(dd, aes(date, N))+ geom_line() + xlab("Publising Date") + ylab("Number of open Positions") + theme_economist()

ddply(all, .(weekday), summarise, N=length(firstCity))

ddply(all, .(Platform), summarise, N=length(Platform))

## correct by platform
ddply(all, .(Platform, Company), summarise, N=length(firstCity))

ddply(all, .(Platform, firstCity), summarise, N=length(firstCity))

ggplot(all, aes(firstCity)) +geom_histogram()
ggplot(all, aes(weekday)) +geom_histogram()
ggplot(all, aes(Company)) +geom_histogram()

dd <- ddply(all, .(firstCity), summarise, N=length(firstCity))
ggplot(dd[dd$N >2,], aes(firstCity, N)) +geom_bar(stat="identity")


dd <- ddply(all, .(Company), summarise, N=length(firstCity))
ggplot(dd[dd$N >2,], aes(Company, N)) +geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))


## how many open positions?
ddply(all,. (Platform), summarise, N=length(Company))

dd <- ddply(all, .(date,Company, firstCity, Platform), summarise, N=length(firstCity))
open <- unique(dd[, c("Company", "firstCity", "N")])
sum(open$N)




## geo locate!
require(ggmap)
cities <- ddply(all, .(firstCity), summarise, N=length(firstCity))
geocode("Frankfurt", output = "latlon" , source = "google")

cities$long <- NA
cities$lat <- NA
for(i in 1:nrow(cities)){
  geo <- geocode(cities[i,]$firstCity, output = "latlon" , source = "google")
  cities[i,]$lon <- geo$lon
  cities[i,]$lat <- geo$lat
}

myMap <- get_map('Germany', zoom = 6, source = "stamen", maptype = "toner")

ggmap(myMap)+ geom_point(aes(x = lon, y = lat, size=N), data = cities,  alpha = .5, color="darkred") + xlab("") + ylab("")

saveRDS(cities, "cities.rds")


all$scrapedate <- as.Date("2015-12-17")
saveRDS(all, "jobMarketData.rds")
