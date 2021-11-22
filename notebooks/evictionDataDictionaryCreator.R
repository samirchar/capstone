library(dplyr)
eviction_dict<-read.csv("../data/Evictions/evictions_dict.csv")
allYears<- read.csv("../data/Evictions/evictions_all-tracts.csv")
distinctYears<- distinct(allYears["year"])
orderedYears <- distinctYears[order(distinctYears$year),]
years <- data.frame(orderedYears)


nrow(eviction_dict)


eviction_dict$Name <- gsub("-", ".", eviction_dict$Name)
annualData<- eviction_dict[!eviction_dict$Name %in% c("GEOID","name","County","State","parent.location"),]

cross = merge(x=annualData,y=years,all.x=TRUE, all.y=TRUE)

cross$Name <- paste(cross$Name, cross$orderedYears, sep="_")

cross$splitDesc <- strsplit(cross$Description,"\n    - ")


descriptionCreator <- function(year, splitDesc){
  if(length(splitDesc)==5){

    splitDesc[2]<-substr(splitDesc[2], 10, 1000000)
    splitDesc[3]<-substr(splitDesc[2], 10, 1000000)
    splitDesc[5]<-substr(splitDesc[2], 10, 1000000)
    
    if(year %in% c(2000,2001,2002,2003,2004)){
      desc <-paste(splitDesc[1],"for",year,":",splitDesc[2],sep=" ")
    } else if(year %in% c(2005,2006,2007,2008,2009)){
      desc <-paste(splitDesc[1],"for",year,":",splitDesc[3],sep=" ")
    } else if(year %in% c(2010)){
      desc <-paste(splitDesc[1],"for",":",splitDesc[4],sep=" ")
    } else{
      desc <-paste(splitDesc[1],"for",":",year,splitDesc[5],sep=" ")
    }
  } else{
    desc <- splitDesc
  }
  return(desc)
}


#len<-lapply(cross[c("orderedYears","splitDesc")], descriptionCreator)

cross$cleanDescription<-mapply(descriptionCreator, cross$orderedYears, cross$splitDesc)
cross<-cross[,c("Name","cleanDescription")]
colnames(cross)<- c("Name","Description")

uniqueRows<- eviction_dict[eviction_dict$Name %in% c("GEOID","name","County","State","parent.location"),]

evictionDataDictionary<-union(uniqueRows,cross)
evictionDataDictionary$Name<-toupper(evictionDataDictionary$Name)
write.csv(evictionDataDictionary,"eviction_dict.csv",row.names = FALSE)

