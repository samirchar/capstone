library(stringr)
library(plyr)
library(hash)
#library(dplyr)
library(tidyverse)
library(reshape2)
library(data.table)


create_fips <- function(df){
  
  
  #Cast
  df$census_tract_number <- df$census_tract_number*100
  df$census_tract_number<-as.character(df$census_tract_number)
  df$county_code<-as.character(df$county_code)
  df$state_code<-as.character(df$state_code)
  
  #Remove decimal point
  df$census_tract_number<-str_replace(df$census_tract_number,'\\.','')
  
  #Set NaNs
  df <- na_if(df,"")
  
  #Padding
  df$census_tract_number<- str_pad(df$census_tract_number,6,pad=0)
  df$county_code<- str_pad(df$county_code,3,pad=0)
  df$state_code<- str_pad(df$state_code,2,pad=0)
  
  #Remove NaN census tracts 
  df <- df[!is.na(df$census_tract_number),]
  
  #Create FIPS
  df$FIPS <- paste(df$state_code,df$county_code,df$census_tract_number,sep = '')
  
  return(df)
}


change_level_names<-function(df,mappings){
  
  for (k in keys(mappings)) {
    
    df[,k] <- mapvalues(df[,k], from=mappings[[k]]$from, to=mappings[[k]]$to)
    
  }
  
  return(df)
}


summarize_categoric <- function(df,cat_cols,ids,mappings){
  
  dfs <- list()
  for (col in cat_cols){
    print(paste('summarizing',col))
    df_pivot <- dcast(setDT(df[,c(ids,col)]),paste("FIPS",col,sep="~"),fill=0,value.var = col,fun.aggregate = length)
    
    
    dfs<-append(dfs,list(df_pivot))
  }
  
  print('Joining')
  df_cat_summarized <- Reduce(function(x, y) merge(x, y,by=ids), dfs)
  df_cat_summarized<-data.frame(df_cat_summarized)
  
  ##Remove unwanted summarized cat cols
  summarized_cat_cols <- c()
  for (k in keys(mappings)) {
    summarized_cat_cols<-c(summarized_cat_cols,mappings[[k]]$to)
  }
  
  df_cat_summarized <- df_cat_summarized[,intersect(c(ids,summarized_cat_cols), names(df_cat_summarized))]
  
  return(df_cat_summarized)
}


generate_data <- function(input_file_path,output_file_path,nrows=Inf){
    start.time <- Sys.time()
    #df <- read.csv(input_file_path)
    
    relevant_cols <- c('census_tract_number',
                       'county_code',
                       'state_code',
                       'loan_amount_000s',
                       'population',
                       'applicant_ethnicity_name',
                       'applicant_race_name_1',
                       'action_taken_name',
                       'applicant_income_000s',
                       'loan_purpose_name',
                       'property_type_name',
                       'loan_type_name',
                       'applicant_sex_name',
                       'purchaser_type_name',
                       'denial_reason_name_1')

    df <- fread(input_file_path,select = relevant_cols, data.table = FALSE, nrows = nrows)
    ######## Create FIPS ###########

    df <- create_fips(df)
    ######## Change level names of categorical variables  ############
    
    mappings <- hash()
    mappings[['applicant_ethnicity_name']] <- list(
      from = c("Not Hispanic or Latino","Hispanic or Latino"),
      to = c('nothispanic','hispanic')
    )
    
    mappings[['applicant_race_name_1']] <-list(
      from = c("White","Asian","Native Hawaiian or Other Pacific Islander","Black or African American","American Indian or Alaska Native"),
      to = c('white','asian','natpac','afam',"native")
    )
    
    mappings[['action_taken_name']]<-list(
      from = c("Loan originated","Application denied by financial institution"),
      to = c("originated","denied")
    )
    
    mappings[['loan_purpose_name']] <- list(
      from = c("Home purchase","Home improvement","Refinancing"),
      to = c("newhome","improvement","refinancing")
    )
    
    mappings[['property_type_name']] <- list(
      from = c("One-to-four family dwelling (other than manufactured housing)","Multifamily dwelling","Manufactured housing"),
      to = c("upto4famihouse","multifamily","manufacthousing")
    )
    
    mappings[['loan_type_name']] <- list(
      from = c("Conventional","FHA-insured"),
      to = c("conv_loan","fhainsured_loan")
    )
    
    mappings[['applicant_sex_name']] <- list(
      from = c("Female","Male"),
      to = c("femalehmda","malehmda")
    )
    
    mappings[['purchaser_type_name']] <- list(from = c("Fannie Mae (FNMA)",
                                                       "Ginnie Mae (GNMA)",
                                                       "Freddie Mac (FHLMC)",
                                                       "Farmer Mac (FAMC)",
                                                       "Private securitization",
                                                       "Commercial bank, savings bank or savings association",
                                                       "Life insurance company, credit union, mortgage bank, or finance company"),
                                              to = c("fnma_purchaser",
                                                     "gnma_purchaser",
                                                     "fhlmc_purchaser",
                                                     "famc_purchaser",
                                                     "privsecurization_purchaser",
                                                     "bank_purchaser",
                                                     "lifeins_purchaser")
    )
    
    mappings[['denial_reason_name_1']] <- list( 
      from = c("Debt-to-income ratio",
               "Employment history",
               "Credit history",
               "Collateral",
               "Insufficient cash (downpayment, closing costs)",
               "Unverifiable information",
               "Credit application incomplete",
               "Mortgage insurance denied"),
      
      to = c("den1_debt_inc_ratio",
             "den2_empl_history",
             "den3_crd_hist",
             "den4_collateral",
             "den5_insuff_cash",
             "den6_unbverif_info",
             "den7_appl_incompl",
             "den8_mortg_ins_denied")
    )
    
    df <- change_level_names(df,mappings)

    
    ####### Aggregate data ##########
    
    
    ids = c("FIPS")
    num_cols <- c('loan_amount_000s','population','applicant_income_000s')
    cat_cols <- c('applicant_ethnicity_name',
                  'applicant_race_name_1',
                  'action_taken_name',
                  'loan_purpose_name',
                  'property_type_name',
                  'loan_type_name',
                  'applicant_sex_name',
                  'purchaser_type_name',
                  'denial_reason_name_1')
    
    
    #Summarize categorical columns
    df_cat_summarized <- summarize_categoric(df,cat_cols,ids,mappings)
    
    #Summarize numeric columns
    
    df_num_summarized <- df %>% dplyr::group_by(FIPS)  %>%
      dplyr::summarise(tot_loan_amount = sum(loan_amount_000s,na.rm = TRUE),
                       mean_loan_amount = mean(loan_amount_000s,na.rm = TRUE),
                       num_loans = n(),
                       mean_income = mean(applicant_income_000s,na.rm = TRUE),
                       tot_pop = round(mean(population,na.rm = TRUE))
      )
    
    ######## Write csv #######
    
    df_summarized <- merge(df_num_summarized,df_cat_summarized,by=ids)
    df_summarized <- df_summarized %>% arrange(FIPS)
    
    write.csv(df_summarized,output_file_path,row.names=FALSE)
    
    remove(df,df_summarized,df_cat_summarized,df_num_summarized)
    gc()
    

}