library(dplyr)
library(stringr)


#reading all the data

fipsChecker<-function(x){
  if(length(x)<11){
    padded<-str_pad(x, 11, pad = "0")
    formatted<-paste(padded,"GID",sep="")
  } else{
    formatted<-paste(x,"GID",sep="")
  }
  return(formatted)
}




evictionData<-read.csv("../data/Evictions/evictions_data.csv" , colClasses=c(GEOID="character"))
mortgageData<-read.csv("../data/Mortgages/mortgages_all_years_new.csv", colClasses=c(FIPS="character"))
colnames(mortgageData)<-toupper(colnames(mortgageData))

nriData<-read.csv("../data/NRI/NRI_Table_CensusTracts/NRI_Clean_CensusTracts.csv", colClasses=c(TRACTFIPS="character"))
sviData<-read.csv("../data/SVI/SVI_all_years.csv", colClasses=c(FIPS="character"))



evictionData$GEOID<-sapply(evictionData$GEOID, fipsChecker)
mortgageData$FIPS<-sapply(mortgageData$FIPS, fipsChecker)
nriData$TRACTFIPS<-sapply(nriData$TRACTFIPS, fipsChecker)
sviData$FIPS<-sapply(sviData$FIPS, fipsChecker)
#Taking subsets for rhode island 


eviction_rhodeIsland<-evictionData[evictionData$STATE=="RHODE ISLAND",]
mortgage_rhodeIsland<-mortgageData[mortgageData$STATE=="RHODE ISLAND",]
nri_rhodeIsland<-nriData[nriData$STATE=="RHODE ISLAND",]
svi_rhodeIsland<-sviData[sviData$STATE=="RHODE ISLAND",]

colnames(mortgage_rhodeIsland)<-toupper(colnames(mortgage_rhodeIsland))

#creating SEPHER for Rhode Island

sepher<-merge(svi_rhodeIsland,nri_rhodeIsland,by.x = "FIPS", by.y = "TRACTFIPS" ,all.x = TRUE)
sepher$STATE<-sepher$STATE.x
sepher<-sepher[,!(colnames(sepher) %in% c("STATE.x","STATE.y"))]
sepher<-merge(sepher,mortgage_rhodeIsland,by.x = "FIPS", by.y = "FIPS" ,all.x = TRUE)
sepher<-merge(sepher,eviction_rhodeIsland,by.x = "FIPS", by.y = "GEOID" ,all.x = TRUE)

#reading data dictionary for verification

dictionary<-read.csv("SEPHER2.0_dataDictionary.csv")

setdiff(dictionary$Name,colnames(sepher))

#removing difference rows from dictionary and renaming extra columns
sepher$STATE<-sepher$STATE.x
sepher$COUNTY<-sepher$COUNTY.x
sepher<-sepher[,!(colnames(sepher) %in% c("STATE.x","STATE.y","COUNTY.x","COUNTY.y"))]

setdiff(dictionary$Name,colnames(sepher))

cols<-dictionary$Name[dictionary$Name %in% colnames(sepher)]

sepher<-sepher[,cols]

write.csv(sepher,"SEPHER2.0_rhodeIsland.csv",row.names = FALSE)



#CREATING SEPHER WITHOUT MORTGAGE DATA

#initializing empty dataframes
struct_svi<-sviData[sviData$STATE == "random state for empty df",]
struct_nri<-nriData[nriData$STATE == "random state for empty df",]
struct_evi<-evictionData[evictionData$STATE == "random state for empty df",]

sepher_noMorg<-merge(struct_svi,struct_nri, by.x = "FIPS", by.y = "TRACTFIPS" ,all.x = TRUE, all.y = TRUE)
sepher_noMorg$STATE<-sepher_noMorg$STATE.x
sepher_noMorg<-sepher_noMorg[,!(colnames(sepher_noMorg) %in% c("STATE.x","STATE.y"))]
sepher_noMorg<-merge(sepher_noMorg,struct_evi,by.x = "FIPS", by.y = "GEOID" ,all.x = TRUE, all.y = TRUE)
sepher_noMorg$STATE<-sepher_noMorg$STATE.x
sepher_noMorg$COUNTY<-sepher_noMorg$COUNTY.x
sepher_noMorg<-sepher_noMorg[,!(colnames(sepher_noMorg) %in% c("STATE.x","STATE.y","COUNTY.x","COUNTY.y"))]
cols<-dictionary$Name[dictionary$Name %in% colnames(sepher_noMorg)]

sepher_noMorg<-sepher_noMorg[,cols]


for(state in unique(sviData$STATE)){
  
  state_nri<- nriData[nriData$STATE==state,]
  state_svi<- sviData[sviData$STATE==state,]
  state_evi<- evictionData[evictionData$STATE==state,]
  
  #checking which state has the most rows to join o
  
  state_sepher<-merge(state_svi,state_nri, by.x = "FIPS", by.y = "TRACTFIPS" ,all.x = TRUE, all.y = TRUE)
  state_sepher$STATE<-state_sepher$STATE.x
  state_sepher<-state_sepher[,!(colnames(state_sepher) %in% c("STATE.x","STATE.y"))]
  state_sepher<-merge(state_sepher,state_evi,by.x = "FIPS", by.y = "GEOID" ,all.x = TRUE, all.y = TRUE)
  state_sepher$STATE<-state_sepher$STATE.x
  state_sepher$COUNTY<-state_sepher$COUNTY.x
  state_sepher<-state_sepher[,!(colnames(state_sepher) %in% c("STATE.x","STATE.y","COUNTY.x","COUNTY.y"))]
  cols<-dictionary$Name[dictionary$Name %in% colnames(state_sepher)]
  
  state_sepher<-state_sepher[,cols]
  sepher_noMorg<-union(sepher_noMorg,state_sepher)
  cat(state,"\n")
}
#writing file
write.csv(sepher_noMorg,"SEPHER2.0_noMortgage.csv",row.names = FALSE)
#creating a data dictionary for no Mortgage SEPHER
dictionary_noMorg<-dictionary[dictionary$Name %in% colnames(sepher_noMorg),]
View(dictionary_noMorg)
write.csv(dictionary_noMorg,"SEPHER2.0_dataDictionary_noMortgage.csv",row.names = FALSE)










#CREATING SEPHER WITH MORTGAGE DATA

dictionary<-read.csv("SEPHER2.0_dataDictionary.csv")

#initializing empty dataframes
struct_svi<-sviData[sviData$STATE == "random state for empty df",]
struct_nri<-nriData[nriData$STATE == "random state for empty df",]
struct_evi<-evictionData[evictionData$STATE == "random state for empty df",]
struct_mort<-mortgageData[mortgageData$STATE == "random state for empty df",]

sepher<-merge(struct_svi,struct_nri, by.x = "FIPS", by.y = "TRACTFIPS" ,all.x = TRUE, all.y = TRUE)
sepher$STATE<-sepher$STATE.x
sepher<-sepher[,!(colnames(sepher) %in% c("STATE.x","STATE.y"))]
sepher<-merge(sepher,struct_evi,by.x = "FIPS", by.y = "GEOID" ,all.x = TRUE, all.y = TRUE)
sepher$STATE<-sepher$STATE.x
sepher$COUNTY<-sepher$COUNTY.x
sepher<-sepher[,!(colnames(sepher) %in% c("STATE.x","STATE.y","COUNTY.x","COUNTY.y"))]

sepher<-merge(sepher,struct_mort,by.x = "FIPS", by.y = "FIPS" ,all.x = TRUE, all.y = TRUE)
cols<-dictionary$Name[dictionary$Name %in% colnames(sepher)]
#getting structure of skeleton in data dictionary order
sepher<-sepher[,cols]

setdiff(dictionary$Name,colnames(sepher))


mortgageData$STATEID<-substr(mortgageData$FIPS,1,2)
#state="RHODE ISLAND"
for(state in unique(sviData$STATE)){
  
  state_nri<- nriData[nriData$STATE==state,]
  state_svi<- sviData[sviData$STATE==state,]
  state_evi<- evictionData[evictionData$STATE==state,]
  state_mor<- mortgageData[mortgageData$STATEID==unique(state_nri$STATEFIPS),]

  #checking which state has the most rows to join o
  
  state_sepher<-merge(state_svi,state_nri, by.x = "FIPS", by.y = "TRACTFIPS" ,all.x = TRUE, all.y = TRUE)
  #cat(class(state_sepher))
  state_sepher$STATE<-state_sepher$STATE.x
  state_sepher<-state_sepher[,!(colnames(state_sepher) %in% c("STATE.x","STATE.y"))]
  state_sepher<-merge(state_sepher,state_evi,by.x = "FIPS", by.y = "GEOID" ,all.x = TRUE, all.y = TRUE)
  state_sepher$STATE<-state_sepher$STATE.x
  state_sepher$COUNTY<-state_sepher$COUNTY.x
  state_sepher<-state_sepher[,!(colnames(state_sepher) %in% c("STATE.x","STATE.y","COUNTY.x","COUNTY.y"))]
  
  state_sepher<-merge(state_sepher,state_mor,by.x = "FIPS", by.y = "FIPS" ,all.x = TRUE, all.y = FALSE)
  state_sepher<-state_sepher[,!(colnames(state_sepher) %in% c("STATEID"))]
  cols<-dictionary$Name[dictionary$Name %in% colnames(state_sepher)]
  state_sepher<-state_sepher[,cols]
  
  #cat(class(sepher),class(state_sepher),"\nbefore union\n")
  sepher<-dplyr::union(sepher,state_sepher)
  #cat(class(sepher),class(state_sepher),"\nafter union\n")
  cat(state,"\n")
}



View(sepher[1:5,])
#writing file
write.csv(sepher,"SEPHER2.0.csv",row.names = FALSE)

View(mortgageData$stateID)
