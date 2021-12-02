library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(tigris)

us_census_tracts <- tracts(state = NULL, cb = TRUE)
st_crs(us_census_tracts)

data <- read.csv('data/predictions_shap.csv')

data['EVICTIONS_MISSING'] <- lapply(data['EVICTIONS'], is.na)
# remove last 3 characters
data['FIPS'] <- lapply(data['FIPS'], function(x) substr(x, 0, nchar(x) - 3))
data <- right_join(us_census_tracts, data, by = c("GEOID" = "FIPS"))

st_write(obj = data_mod, dsn = "data/preds_shap.shp" )
