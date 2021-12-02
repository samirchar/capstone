library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(tigris)

us_census_tracts <- tracts(state = NULL, cb = TRUE)
st_crs(us_census_tracts)

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')
data <- union_all(train, test)

data['EVICTIONS_MISSING'] <- lapply(data['EVICTIONS'], is.na)
# remove last 3 characters
data['FIPS'] <- lapply(data['FIPS'], function(x) substr(x, 0, nchar(x) - 3))
data <- right_join(us_census_tracts, data, by = c("GEOID" = "FIPS"))
write_csv(data, 'data/geo_sepher.csv')
