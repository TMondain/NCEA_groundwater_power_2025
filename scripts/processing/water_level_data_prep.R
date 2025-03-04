rm(list = ls())

## water level data prep 
library(tidyverse)

# read files
dip <- read.csv("data/raw/combined_dip_data.csv")
site <- read.csv("data/raw/combined_site_data.csv")

head(dip)
head(site)


#### Step 1: calculate mean ----------------------------------------------------
mn_dip <- mean(dip$water_level)
mn_site <- mean(site$water_level)


#### Step 2: calculate 5th centile

# remove negative values
dip <- dip[dip$water_level >= 0,]
head(dip)

head(dip, 50) %>% 
  dplyr::group_by(station_number, station_name, year) %>% 
  dplyr::summarise(cent_5 = quantile(water_level, probs = 0.05, na.rm = TRUE),
                   cent_95 = quantile(water_level, probs = 0.95, na.rm = TRUE))





# 5yr baseline by site


# how many years per site
dip %>% 
  group_by(station_number) %>% 
  summarise(length(unique(year)))
## not the same number of years at each site

# function to keep the first n rows of a group
topn_rows <- function(dataset, group_col, sort_col, nrows_to_keep) {
  
  # get unique sites
  uniq_group <- unique(dataset[,group_col])
  
  # get top n rows of data for each group
  top_nyrs <- do.call(rbind, lapply(uniq_group, function(x) {
    
    # select the station of interest
    sitesub <- dataset[dataset[,group_col] == x,]
    
    # sort by year, select top 5  
    topn_rows <- sort(unique(sitesub[,sort_col]))[1:nrows_to_keep]
    
    # get only first 5yrs' worth data
    sitesub[sitesub[,sort_col] %in% topn_rows,]
    
  }))
  
  top_nyrs
  
}

## use this to extract the data for the first 5 years for each station
top5 <- topn_rows(dataset = dip,
                  group_col = "station_number",
                  sort_col = "year",
                  nrows_to_keep = 5)

# get median of each station
median_5 <- tapply(top5$water_level, top5$station_number, median)

dip$baseline5yr <- median_5[match(dip$station_number, names(median_5))]

# calculate % of 5yr base
dip <- dip %>% 
  mutate(perc_chg = water_level/baseline5yr*100)
