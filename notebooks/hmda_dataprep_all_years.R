setwd("Google_Drive/Masters/capstone/capstone/notebooks/")
source("hmda_dataprep_single_year.R")
input_files_path = '../data/raw/HMDA/'
output_files_path = "../data/interim/HMDA/"

hmda_files <- list.files(path = input_files_path,pattern = 'hmda_*',full.names = TRUE)


for (f in hmda_files){
  year <- unlist(strsplit(f,'_'))[2]
  generate_data(f,paste(output_files_path,paste("hmda",year,"usa.csv",sep="_"),sep='/'))
  
}
 
