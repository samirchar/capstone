mortgageDictionary<-read.csv("../data/Mortgages/hmda_dictionary.csv")


mortgageData<-read.csv("../data/Mortgages/mortgages_all_years.csv")
cols<-colnames(mortgageData)
splits<-strsplit(cols,"_20")
cols_wo_yr<-c()
for(i in splits){
  cols_wo_yr<-c(cols_wo_yr,i[1])
}
setdiff(unique(cols_wo_yr),unique(mortgageDictionary$Name))
setdiff(unique(mortgageDictionary$Name),unique(cols_wo_yr))


years <- data.frame(years=2007:2017)


annualData<- mortgageDictionary[!mortgageDictionary$Name %in% c("FIPS"),]

cross = merge(x=annualData,y=years,all.x=TRUE, all.y=TRUE)
cross$Name <- paste(cross$Name, cross$years, sep="_")
cross$Description <- paste(cross$Description, cross$years, sep=" for ")
uniqueRows<- mortgageDictionary[mortgageDictionary$Name %in% c("FIPS"),]

cross<-cross[,c("Name","Description")]
mortgageDictionary<-union(uniqueRows,cross)
mortgageDictionary$Name<-toupper(mortgageDictionary$Name)

write.csv(mortgageDictionary,"../data/Mortgages/hmda_allyear_dictionary.csv",row.names = FALSE)
