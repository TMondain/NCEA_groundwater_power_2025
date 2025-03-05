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


#### Step 2: calculate 5th centile ---------------------------------------------

## dip data
hist(dip$water_level)

# remove negative values?
# dip <- dip[dip$water_level >= 0,]
head(dip)

dip_quantsum <- dip %>% 
  dplyr::group_by(station_number, station_name, easting, northing, year) %>% 
  dplyr::summarise(median_val = median(water_level, na.rm = TRUE),
                   cent_5 = quantile(water_level, probs = 0.05, na.rm = TRUE),
                   cent_95 = quantile(water_level, probs = 0.95, na.rm = TRUE))


## site data
hist(site$water_level)

# remove negative values?
# site <- site[site$water_level >= 0,]
head(site)

site_quantsum <- site %>% 
  dplyr::group_by(station_number, station_name, easting, northing, year) %>% 
  dplyr::summarise(median_val = median(water_level, na.rm = TRUE),
                   cent_5 = quantile(water_level, probs = 0.05, na.rm = TRUE),
                   cent_95 = quantile(water_level, probs = 0.95, na.rm = TRUE))


## save
dir.create("data/processed")

write.csv(dip_quantsum, 
          "data/processed/dip_data_quantile_summary.csv")

write.csv(site_quantsum, 
          "data/processed/site_data_quantile_summary.csv")

#### Step 3: %chg relative to 5yr baseline by site -----------------------------

## dip
# check how many years per site?
dip %>% 
  group_by(station_number) %>% 
  summarise(length(unique(year)))
## not the same number of years at each site
## decision: make the baseline the first 5 yrs worth of data at that site?

# function to keep the data associated with the first n groups
firstn_groups <- function(dataset, group_col, sort_col, 
                          nrows_to_keep, sort_decreasing = FALSE) {
  
  # get unique sites
  uniq_group <- unique(dataset[,group_col])
  
  # get data associated with first n groups
  first_groupdat <- do.call(rbind, lapply(uniq_group, function(x) {
    
    # select the station of interest
    sitesub <- dataset[dataset[,group_col] == x,]
    
    # sort by group, select first 5 groups
    groups_to_keep <- sort(unique(sitesub[,sort_col]), 
                           decreasing = sort_decreasing)[1:nrows_to_keep]
    
    # get only first 5yrs' worth data
    sitesub[sitesub[,sort_col] %in% groups_to_keep,]
    
  }))
  
  first_groupdat
  
}

## use this to extract the data for the first 5 years for each station
first5 <- firstn_groups(dataset = dip,
                        group_col = "station_number",
                        sort_col = "year",
                        nrows_to_keep = 5, 
                        sort_decreasing = FALSE)

# get median of the first 5 years for each station
median_5 <- tapply(first5$water_level, first5$station_number, median)

dip$baseline5yr <- median_5[match(dip$station_number, names(median_5))]

# calculate % of 5yr base
dip <- dip %>% 
  mutate(perc_chg_ind = water_level/baseline5yr*100)

head(dip)
hist(dip$perc_chg_ind)
hist(dip$perc_chg_ind[dip$perc_chg_ind<500])


## site
# check how many years per site?
site %>% 
  group_by(station_number) %>% 
  summarise(length(unique(year)))
## not the same number of years at each site
## decision: make the baseline the first 5 yrs worth of data at that site?

## use this to extract the data for the first 5 years for each station
first5_site <- firstn_groups(dataset = site,
                             group_col = "station_number",
                             sort_col = "year",
                             nrows_to_keep = 5, 
                             sort_decreasing = FALSE)

# get median of the first 5 years for each station
median_5site <- tapply(first5_site$water_level, first5_site$station_number, median)

site$baseline5yr <- median_5site[match(site$station_number, names(median_5site))]

# calculate % of 5yr base
site <- site %>% 
  mutate(perc_chg_ind = water_level/baseline5yr*100)

head(site)
hist(site$perc_chg)
hist(site$perc_chg[site$perc_chg<500])


## save
write.csv(dip,
          "data/processed/dip_data_5yr_perc_change.csv")

write.csv(site,
          "data/processed/site_data_5yr_perc_change.csv")

