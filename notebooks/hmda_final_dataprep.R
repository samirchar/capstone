library(DataExplorer)
library(dplyr)
library(tidyverse)


#Read all files in loop, append year to columns and concatenate all dfs.

hmda_files <- list.files(path = '../data/interim/HMDA/',pattern = 'hmda_*',full.names = TRUE)

dfs <- list()
for (f in hmda_files){
  df<-read.csv(f,colClasses=c("FIPS"="character"))
  year <- unlist(strsplit(f,'_'))[2]
  cols <- names(df)
  names(df) <- c(cols[1],paste(cols[-1], year, sep='_')) 
  dfs<-append(dfs,list(df))
}

all_years_df <- Reduce(function(x, y) merge(x, y, all=TRUE,by='FIPS'), dfs)

write.csv(all_years_df,'../data/processed/mortgages_all_years.csv',row.names = FALSE)
