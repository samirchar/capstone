library(dplyr)


evictionDictionary<-read.csv("../data/Evictions/evictions_data_dict.csv")
mortgageDictionary<-read.csv("../data/Mortgages/hmda_allyear_dictionary.csv")

nriDictionary<-read.csv("../data/NRI/NRIDataDictionary.csv")
nriDictionary<-nriDictionary[,c("Field.Name","Field.Alias")]
colnames(nriDictionary)<-c("Name","Description")
nriDictionary<-nriDictionary[nriDictionary$Name %in% colnames(nriData),]

sviDictionary<-read.csv("../data/SVI/SVI_data_dictionary.csv")
colnames(sviDictionary)<-c("Name","Description")

dataDict<-union_all(evictionDictionary,mortgageDictionary)
dataDict<-union_all(dataDict,sviDictionary)
dataDict<-union_all(dataDict,nriDictionary)

dataDict$Name<-toupper(dataDict$Name)

#checking uniqueness of column names and seeing which ones repeat if any
if(length(dataDict$Name)==length(unique(dataDict$Name))){
  cat("All columns are unique\n")
} else {
  cat("\nThere are common column names\nThe count of unique columns is:\n")
  print(n_distinct(dataDict$Name))
  counts = group_by(dataDict,Name) %>%
    summarise(count=n())
  counts[counts$count>1,]
}


#removing repeating column names as they are only state and fips (identical information)

dataDict <- dataDict %>%
  group_by(Name) %>%
  filter(row_number() == 1)


#rechecking uniqueness
if(length(dataDict$Name)==length(unique(dataDict$Name))){
  cat("All columns are unique\n")
} else {
  cat("\nThere are common column names\nThe count of unique columns is:\n")
  print(n_distinct(dataDict$Name))
  counts = group_by(dataDict,Name) %>%
    summarise(count=n())
  counts[counts$count>1,]
}
#it is unique now --- EUREKA!! SEPHER2.0 Data Dictionary IS READY!!!


#cross-check columns with data and subset or vs old dictionary
#sepher_rhodeIsland<- read.csv("SEPHER2.0_rhodeIsland.csv")
#dataDict<-dataDict[dataDict$Name %in% colnames(sepher_rhodeIsland),]


#sepher_oldDict<- read.csv("SEPHER2.0_dataDictionary.csv")
#final writing
write.csv(dataDict,"SEPHER2.0_dataDictionary.csv",row.names=FALSE)
