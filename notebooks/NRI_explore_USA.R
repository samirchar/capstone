library(dplyr)
library(tidyverse)
library(naniar)



dict<-read.csv("../data/NRI/NRI_Table_CensusTracts_RhodeIsland/NRIDataDictionary.csv")
bigdict<-read.csv("../data/NRI/NRI_Table_CensusTracts/NRIDataDictionary.csv")
setdiff(dict$Field.Alias,bigdict$Field.Alias)
setdiff(bigdict$Field.Alias,dict$Field.Alias)
nrow(bigdict) == nrow(union(dict,bigdict))
#checking if rhode island and country wide nri data dictionary is exactly the same -- THEY ARE





rhodeData<-read.csv("../data/NRI/NRI_Table_CensusTracts_RhodeIsland/NRI_Table_CensusTracts_RhodeIsland.csv")
bigData<-read.csv("../data/NRI/NRI_Table_CensusTracts/NRI_Table_CensusTracts.csv")
NROW(bigData$TRACTFIPS)==n_distinct(bigData$TRACTFIPS)
#checking if tractfips is a unique identifier --- IT IS
rhodeDataFromBig<- bigData[bigData$STATE=="Rhode Island",]
nrow(rhodeDataFromBig) == nrow(union(rhodeDataFromBig,rhodeData))
#checking if state datasets are identical to state subsetted big dataset -- THEY ARE

#Running cleaning code of rhode island on big data table


#making "Not Applicable" >> NaN

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A","Not Applicable","Not Available", "NOt available","Insufficient Data","No Rating")
#cleanData <-replace_with_na_all(bigData, condition = ~.x %in% na_strings) 
#commenting cleaning of entire dataset as it is too large for the process
#instead, looping through the data by state, cleaning it and taking its union.
#initializing the cleaned data with a empty df.
cleanData<- bigData[bigData$State == "random state name for empty df",]


#getting all non-numeric columns
charCols<-colnames(bigData[sapply(bigData, function(x) is.character(x))])

for(state in unique(bigData$STATE)){
  
  stateData<- bigData[bigData$STATE==state,]
  #making all columns uppercase
  for(col in charCols){
    stateData[col] <- sapply(stateData[col],toupper)
  }
  cleanStateData<- replace_with_na_all(stateData, condition = ~.x %in% na_strings)
  cleanData<-union(cleanData,cleanStateData)
  cat(state,"\n")
  
}

#checking if all rows are back in the df
nrow(cleanData) == nrow(bigData)
# THEY ARE all there


#changing OID_ to OBJECTID
colnames(cleanData)[colnames(cleanData) == 'OID_'] <- 'OBJECTID'

write.csv(cleanData,"../data/NRI/NRI_Table_CensusTracts/NRI_Clean_CensusTracts.csv", row.names = FALSE)
