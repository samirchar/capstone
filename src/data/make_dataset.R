setwd("Google_Drive/Masters/capstone/capstone/src/data/")
library(stringr)
library(hash)
library(dplyr)
library(tidyverse)
library(data.table)
library(hash)

### Functions ###

handle_missing_values <- function(df,na_mapping, id_index =1){
  
  mask_all_na = rowMeans(is.na(df[,-c(id_index)]))<1
  df <- df[mask_all_na,]
  
  mask_evictions = is.na(df$EVICTIONS)
  df <- df[!mask_evictions,]
  
  print(dim(df))
  
  #Impute missing values 
  for (col in na_mapping$cols) {
    df[is.na(df[,col]),col]<-na_mapping$value
  }
  
  for (i in names(df)) {
    if(grepl( "EALT", i, fixed = TRUE)){
      
      
      hzrd = str_split(i,"_")[[1]][1]
      hzrd_rating_col = paste(hzrd,"EALR",sep="_")
      
      if(hzrd_rating_col %in% names(df)){
        mask_nat_applicable = df[,hzrd_rating_col] == "NOT APPLICABLE"
        
        df[,i][which(mask_nat_applicable)]<-0
      }
      
    }
  }
    
  return(df)
}


read_data <- function(input_file_path,relevant_cols, na_mapping ,year = 2016,nrows=Inf, handle_missing = TRUE){
  
  df <- fread(input_file_path,
              colClasses = c('FIPS'='character'),
              data.table = FALSE,
              nrows = nrows)
  
  #Auxiliar FEMA columns
  aux_cols = c()
  for (i in names(df)) {
    if(grepl( 'EALR', i, fixed = TRUE)){
      aux_cols <- c(aux_cols,i)
      }
  }
  
    
  relevant_cols_filtered <- c()
  for (col in relevant_cols) {
    col_year = paste(col,year,sep='_')
    if ( col_year %in% names(df)) {
      relevant_cols_filtered <- append(relevant_cols_filtered,col_year)
    }
    else {
      relevant_cols_filtered <- append(relevant_cols_filtered,col)
    }
  }
  
  relevant_cols_cleaned <- str_replace(relevant_cols_filtered,paste('_',year,sep=''),'')

  
  if(handle_missing){
    df <- df[,c(relevant_cols_filtered,aux_cols)]
    names(df) <- c(relevant_cols_cleaned,aux_cols)
    
    print('handling missing')
    df <- handle_missing_values(df, na_mapping = na_mapping)
    
    df <- df[,relevant_cols_cleaned]
  }else{
    df <- df[,relevant_cols_filtered]
    names(df) <- relevant_cols_cleaned
  }

return(df)
}


train_test_split <-function(df, train_proportion = 0.8, seed = 123){
  
  train_size <- floor(train_proportion*nrow(df))
  
  ## set the seed to make your partition reproducible
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(df)), size = train_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  return(list(train = train,test = test))
}



### Script ###

#Parameters
input_file_path <- '../../data/processed/sepher2.0_cleaned.csv'
output_folder_path <- '../../data/processed/sepher_model_data'


relevant_cols <- c('FIPS',
                   'POPULATION',
                   'EP_UNEMP',
                   'EP_PCI',
                   'EP_POV',
                   'EP_NOVEH',
                   'EP_NOHSDP',
                   'EP_MOBILE',
                   'EP_MINRTY',
                   'EP_CROWD',
                   'EP_AGE65',
                   'EP_AGE17',
                   'AFAM',
                   'WHITE',
                   'HISPANIC',
                   'WFIR_EALT',
                   'HRCN_EALT',
                   'CFLD_EALT',
                   'RFLD_EALT',
                   'HWAV_EALT',
                   'EVICTIONS')

na_mapping <- hash()
na_mapping <- c(list(cols =c('AFAM',
                             'WHITE',
                             'HISPANIC')
                     ,value=0))

year = 2016
train_proportion = 0.8

#Execution
df <- read_data(input_file_path,relevant_cols,year = year, na_mapping = na_mapping)
split = train_test_split(df,train_proportion = train_proportion)


dir.create(file.path(output_folder_path))
write.csv(split$train,file.path(output_folder_path,'train.csv'),row.names = FALSE)
write.csv(split$test,file.path(output_folder_path,'test.csv'),row.names = FALSE)

