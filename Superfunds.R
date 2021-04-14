# importing relevant data
bodytext = read.csv("~/Desktop/Penn Stuff/OIDD 245/DP2/DP2 - superfundlinks.csv")
cancer = read.csv("~/Desktop/Penn Stuff/OIDD 245/DP2/DP2 - Cancer Rates.csv")
scores = read.csv("~/Desktop/Penn Stuff/OIDD 245/DP2/DP2 - Site Score.csv")
zipcodes = read.csv("~/Desktop/Penn Stuff/OIDD 245/DP2/DP2 - Zips.csv")
localHPI = read.csv("~/Desktop/Penn Stuff/OIDD 245/DP2/housing_prices.csv")
nationalHPI = read.csv("~/Desktop/Penn Stuff/OIDD 245/DP2/nationalHPI.csv")

#importing libraries
library(stringr)
library(lubridate)
library(dplyr)
library (plyr)
library(ggplot2)
library(tidyverse)
library(syuzhet)
library(wordcloud)
library(tm)
library(ggmap)
library(zipcode)
library(maps)
library(mapproj)
library(mapdata)
library(tidyverse)

#data cleaning
bodytext$body = str_replace_all(bodytext$body, "\n", " ")

scores$listing_year = mdy(scores$Listing.Date)
scores$years = floor_date(scores$listing_year, unit = "year")
scores$years = years[!duplicated(years)]
scores$years = years[order(years)]

housing = localHPI[which(localHPI$Five.Digit.ZIP.Code %in% zipcodes$Zip.Code),]


#data merging/joining
cancer$County = tolower(cancer$County)
cancer$County = str_replace_all(cancer$County, "county", "")
cancer$County = str_replace_all(cancer$County, "[(6)]", "")
cancer$County = str_replace_all(cancer$County, " ", "")

zipcodes$County = tolower(zipcodes$County) 

scores$Site.Name = toupper(scores$Site.Name)

all = inner_join(cancer, zipcodes, by = "County")
all = inner_join(all, scores, by = "Site.Name")

#DATA VISUALIZATIONS
#Superfund sites over time
sites = sites %>% mutate(cumusum=cumsum(sites$n))
plot(years, sites$cumusum, xlab = "Years", ylab = "Number of Superfund Sites", 
     main = "Number of Superfund Sites in Pennsylvania Over Time", type = "l", col = "Red", lwd = 4)

#Word cloud
bodytext$words = strsplit(bodytext$body, " ")

words_freq = table(unlist(bodytext$words))
allwords = cbind.data.frame(names(words_freq),as.integer(words_freq))
allwords = filter(allwords, allwords$`as.integer(words_freq)` > 20)
allwords = filter(allwords, allwords$`names(words_freq)` != "the")
allwords = filter(allwords, allwords$`names(words_freq)` != "and")
allwords = filter(allwords, allwords$`names(words_freq)` != "to")
allwords = filter(allwords, allwords$`names(words_freq)` != "in")
allwords = filter(allwords, allwords$`names(words_freq)` != "of")
allwords = filter(allwords, allwords$`names(words_freq)` != "The")
allwords = filter(allwords, allwords$`names(words_freq)` != "a")

wordcloud(allwords$`names(words_freq)`, allwords$`as.integer(words_freq)`, colors=brewer.pal(8, "RdGy"))

#Map with site scores
us = ggplot2::map_data("state")
pennsylvania = us[which(us$region == "pennsylvania"),]
ggplot() + geom_polygon(data = pennsylvania, aes(x= long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_point(data = scores, aes(x = Long, y = Lat), group = 1, colour = "red", size = scores$Site.Score/5,  alpha = 0.5) +
  labs(title = "Philadelphia Superfund Sites", x = "Longitude", y = "Latitude", 
       subtitle = "Point size is proportional to site score")


#Housing: Ambler
ambler = housing[which(housing$Five.Digit.ZIP.Code == 19002),]
ambler$localHPI = as.numeric(ambler$HPI.with.1990.base)
ggplot(data = ambler, aes(x = as.numeric(Year))) +
  geom_line(aes(y = localHPI, group=1)) +
  labs(title = "Housing Price Index in Ambler, PA", x = "Year", y = "Housing Price Index (Base 1990)", 
       subtitle = "Site of BoRit Asbestos Dump Site") +
  geom_vline(xintercept = 1989, colour = "orange") +
  geom_vline(xintercept = 2009, colour = "red") +
  geom_text(aes(x = 1988, label = "EPA phases out Asbestos", y = 175), 
            angle=90, colour = "orange") +
  geom_text(aes(x = 2008, label = "Ambler becomes designated asbestos Superfund site", y = 110), 
            angle=90, colour = "red")

#HPI Change
currentHPI = localHPI[which(localHPI$Year == 2020),]
superfund_hpi = currentHPI[which(currentHPI$Five.Digit.ZIP.Code %in% zipcodes$Zip.Code),]
superfund_hpi$HPI = as.numeric(superfund_hpi$HPI)
non_superfund_hpi = currentHPI[-which(currentHPI$Five.Digit.ZIP.Code %in% zipcodes$Zip.Code),]
non_superfund_hpi = non_superfund_hpi[which(non_superfund_hpi$Five.Digit.ZIP.Code > 15000),]
non_superfund_hpi = non_superfund_hpi[which(non_superfund_hpi$Five.Digit.ZIP.Code < 19700),]
non_superfund_hpi$HPI = as.numeric(non_superfund_hpi$HPI)
non_superfund_hpi = non_superfund_hpi[-which(is.na(non_superfund_hpi$HPI)),]
housing_average = data.frame("Category" = c("Zip codes with a superfund site", "Zip codes without a superfund site"), 
                                  "Current HPI" = c(mean(superfund_hpi$HPI), mean(non_superfund_hpi$HPI)))

ggplot(data = housing_average, aes(x = Category, y = Current.HPI)) +
  labs(title = "Housing Price Index: Superfund Sites and Lack Thereof", x = "Category", y = "HPI (as of 2020)", 
       subtitle = "An illustration of oblivion to the danger of superfund sites") +
  geom_col(colour = "black", fill = "dark orange") 


#Site score to cancer rate
cor(all$Site.Score, all$Cancer.Rate)

ggplot(data = all, aes(x = Site.Score, y = Cancer.Rate)) +
  labs(title = "Correlation between Site Scores and Cancer Rates", x = "Site Score", y = "Cancer incidence (per 100,000)", 
       subtitle = "Site scores represent the potential threat the site presents to those around it") +
  geom_text(aes(x = 67, label = "Correlation: 0.23", y = 540)) +
  geom_point(color = "darkblue", alpha = 0.5, size = 4) +
  geom_abline(intercept = 466.4444, slope = 0.4489)


#Column chart of cancer rates between having sites or not
counties_with_sites = cancer[which(cancer$County %in% zipcodes$County),]
counties_without_sites = cancer[-which(cancer$County %in% zipcodes$County),]
cancer_rates_average = data.frame("Category" = c("Counties with a superfund site", "Counties without a superfund site"), 
                                  "Cancer Incidence" = c(mean(counties_with_sites$Cancer.Rate), mean(counties_without_sites$Cancer.Rate)))
ggplot(data = cancer_rates_average, aes(x = Category, y = Cancer.Incidence)) +
  labs(title = "Cancer Incidence: Superfund Sites and Lack Thereof", x = "Category", y = "Cancer incidence (per 100,000)") +
  geom_col(colour = "black", fill = "dark red") +
  coord_cartesian(ylim = c(400, 500))
  
