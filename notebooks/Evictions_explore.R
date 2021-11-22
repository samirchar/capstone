library(dplyr)
library(tidyr)

#Reading eviction data for USA on a tract level
data<- read.csv("../data/Evictions/evictions_all-tracts.csv")
#checking number of columns
ncol(data)



#Splitting the Parent.location column into components County, State
#To confirm: Is the county actually county
data <- separate(
  data,
  parent.location,
  c("County","State"),
  sep = ", ",
  remove = FALSE
) 
#Checking whether all states are present in the data
if(n_distinct(data$State) == 52){
  cat("All states")
} else{
  cat("Not all states")
}
#Since all states are not present, checking which ones are absent
#preparing a vector of all state names
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "Washington DC", "West Virginia", "Wisconsin", "Wyoming")
#summarizing the state data
state_summary <- group_by(data,State) %>%
  summarize(rowCount=n(), year_count = n_distinct(year))
#checking which states are absent
absentStates <- setdiff(states,state_summary$State)
print(absentStates)


#checking number of years we have data on across the states
state_summary[,c("State","year_count")]
#All states have different numbers of years. 


#checking whether the tract year combination is a unique identifier or not
distinctness_summary<- group_by(data, GEOID, year) %>%
  summarise(count = n())
#descending order
distinctness_summary<- distinctness_summary[order(-distinctness_summary$count),]
#filtering for geoids with repeating data
repeating_data <- distinctness_summary[distinctness_summary$count>1,]$GEOID
#Viewing geoIDs with repeating data

#making unique data by grouping and taking the first row as from what we observe
#in the geoIDs with repeating data, the first one seems to be correct and the rest seems to be noise of some sort
data_unique <- data %>%
                group_by(GEOID, year) %>%
                filter(row_number() == 1)



#We will now pivot the data by the year and fill it with na's as required.
#hence we will have new columns for each of 17 years

pivot_data <- pivot_wider(data_unique, names_from=c(year), values_from = -c(GEOID,name,County,State,parent.location,year))


#sorting columns according to the data dictionary order
data_dict<- read.csv("../data/Evictions/evictions_data_dict.csv")
pivot_data<-pivot_data[,data_dict$Name]


#Making all character columns uppercase
charCols<-colnames(pivot_data[sapply(pivot_data, function(x) is.character(x))])
for(col in charCols){
  pivot_data[col] <- sapply(pivot_data[col],toupper)
}

#making column names uppercase
colnames(pivot_data)<-toupper(colnames(pivot_data))


#writing final file
write.csv(pivot_data,"../data/Evictions/evictions_data.csv",row.names = FALSE)


#cat(colnames(data))

#rhode<- filter(data, str_detect(data$parent.location,"Rhode Island"))
#rhode_summary <- group_by(rhode,GEOID) %>%
#  summarize(count = n())
