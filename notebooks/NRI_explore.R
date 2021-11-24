info<- read.csv("../data/NRI/NRI_Table_CensusTracts_RhodeIsland/NRI_HazardInfo.csv")
data<- read.csv("../data/NRI/NRI_Table_CensusTracts_RhodeIsland/NRI_Table_CensusTracts_RhodeIsland.csv")
dict<- read.csv("../data/NRI/NRI_Table_CensusTracts_RhodeIsland/NRIDataDictionary.csv")

svi<- read.csv("../data/SVI/RhodeIsland.csv")

library(dplyr)
library(tidyverse)
library(naniar)

#making "Not Applicable" >> NaN

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A","Not Applicable","Not Available", "NOt available","Insufficient Data","No Rating")
data <-replace_with_na_all(data, condition = ~.x %in% na_strings)

#setdiff(svi$FIPS,data$TRACTFIPS)
#View(svi[svi$FIPS %in% c("44005990000","44009990100"),])

svi_rhode<-filter(svi, ST == 44)
merged_data<- merge(svi_rhode, data, by.x = "FIPS", by.y = "TRACTFIPS")


#nanCount <- as.data.frame(colSums(is.na(data)))
#nanCount$columns <- rownames(nanCount)
#rownames(nanCount)<- NULL
#colnames(nanCount) <- c("nan_count", "column")
#nanCount<-(nanCount[order(-nanCount$nan_count),])
#nrow(nanCount[nanCount$nan_count == nrow(data),])
#toBeDropped <- nanCount[nanCount$nan_count == nrow(data),]$column


#dropping all columns with all NaN rows
#data2 <- data[,!(names(data) %in% toBeDropped)]


#categoryCount <- pivot_longer(summarise_all(data2,n_distinct), cols= everything())
#categoryCount <- categoryCount[order(categoryCount$value),]
#hist(categoryCount$value)


#continuous <- data2[categoryCount[categoryCount$value>2,]$name]
#continuous_num <- continuous[,unlist(lapply(continuous, is.numeric))]

#library(corrplot)
#correlation <- continuous_num %>%
#  cor()
#correlation
