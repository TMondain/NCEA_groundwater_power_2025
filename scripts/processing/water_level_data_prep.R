rm(list = ls())

## water level data prep 
library(tidyverse)

# read files
dip <- read.csv("data/raw/combined_dip_data.csv")
site <- read.csv("data/raw/combined_site_data.csv")

head(dip)
head(site)


# remove negative values
dip <- dip[dip$water_level >= 0,]

# remove negative values
site <- site[site$water_level >= 0,]


#### Step 1: Simulate sampling locations ---------------------------------------

## dip
# get unique sites and select a random 20 per geographic location
head(dip)
sampdip <- dip %>% 
  dplyr::select(station_number, loc_name) %>%
  distinct() %>% 
  dplyr::group_by(loc_name) %>% 
  slice_sample(n = 20)
dim(sampdip)

# bind to full data
dip$sampled_20 <- ifelse(dip$station_number %in% sampdip$station_number, 1, 0)

# check it worked
dip %>%
  select(station_number, loc_name, sampled_20) %>%
  distinct() %>% 
  group_by(loc_name) %>% 
  summarise(sum(sampled_20))


## site
# get unique sites and select a random number equal to nrow(sampdip)
head(site)

sampsite <- site %>% 
  dplyr::select(station_number, loc_name) %>%
  distinct()

sampsite <- sampsite$station_number[sample(1:nrow(sampsite), nrow(sampdip))]

# The North east data are in the southwest?!?!!
# only one southwest data point - in the southwest

# bind to full data
site$sampled_20 <- ifelse(site$station_number %in% sampsite, 1, 0)

# check it worked
site %>%
  select(station_number, loc_name, sampled_20) %>%
  distinct() %>% 
  group_by(loc_name) %>% 
  summarise(sum(sampled_20))


#### Step 2: Calculate %chg relative to 5yr baseline by site -------------------

## dip
# check how many years per site?
dip %>% 
  group_by(station_number) %>% 
  summarise(length(unique(year)))
## not the same number of years at each site
## decision: make the baseline the first 5 yrs worth of data at that site?

# function to keep the data associated with the first n groups
firstn_groups <- function(dataset, group_col, sort_col, 
                          ngroups_to_keep, sort_decreasing = FALSE) {
  
  # get unique sites
  uniq_group <- unique(dataset[,group_col])
  
  # get data associated with first n groups
  first_groupdat <- do.call(rbind, lapply(uniq_group, function(x) {
    
    # select the station of interest
    sitesub <- dataset[dataset[,group_col] == x,]
    
    # sort by group, select first 5 groups
    groups_to_keep <- sort(unique(sitesub[,sort_col]), 
                           decreasing = sort_decreasing)[1:ngroups_to_keep]
    
    # get only first 5yrs' worth data
    sitesub[sitesub[,sort_col] %in% groups_to_keep,]
    
  }))
  
  first_groupdat
  
}

## use this to extract the data for the first 5 years for each station
first5 <- firstn_groups(dataset = dip,
                        group_col = "station_number",
                        sort_col = "year",
                        ngroups_to_keep = 5, 
                        sort_decreasing = FALSE)

# get median of the first 5 years for each station
median_5 <- tapply(first5$water_level, first5$station_number, median)

dip$baseline5yr <- median_5[match(dip$station_number, names(median_5))]
# check number of zeros
sum(dip$baseline5yr==0, na.rm = TRUE)/nrow(dip) * 100
# this will bugger up the baseline calculation

# calculate % of 5yr base
dip <- dip %>% 
  mutate(water_level = sum_total/count, # recalculate water level - because when summarising the file, if the station info was spread across different chunks, it wouldn't have the total number of samples
         sampling_point = station_number,
         perc_chg_ind = water_level/baseline5yr*100)

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
                             ngroups_to_keep = 5, 
                             sort_decreasing = FALSE)

# get median of the first 5 years for each station
median_5site <- tapply(first5_site$water_level, first5_site$station_number, median)

site$baseline5yr <- median_5site[match(site$station_number, names(median_5site))]
sum(site$baseline5yr==0, na.rm = TRUE)/nrow(site) * 100
# this will bugger up the baseline calculation

# calculate % of 5yr base
site <- site %>% 
  mutate(water_level = sum_total/count, # recalculate water level - because when summarising the file, if the station info was spread across different chunks, it wouldn't have the total number of samples
         sampling_point = station_number,
         perc_chg_ind = water_level/baseline5yr*100)

head(site)
hist(site$perc_chg)
hist(site$perc_chg[site$perc_chg<500])


#### Step 2: calculate mean, 5th and 95th centile ---------------------------------------------

## dip data
hist(dip$water_level)
head(dip)

dip_quantsum <- dip %>% 
  mutate(sampling_point = station_number) %>% 
  dplyr::group_by(sampling_point, easting, northing, year) %>% 
  dplyr::summarise(cent_5_value = quantile(water_level, probs = 0.05, na.rm = TRUE),
                   cent_95_value = quantile(water_level, probs = 0.95, na.rm = TRUE))

# add sampled column again
dip_quantsum$sampled_20 <- ifelse(dip_quantsum$sampling_point %in% sampdip$station_number, 1, 0)

## site data
hist(site$water_level)

# remove negative values?
# site <- site[site$water_level >= 0,]
head(site)

site_quantsum <- site %>% 
  mutate(sampling_point = station_number) %>% 
  dplyr::group_by(sampling_point, easting, northing, year) %>% 
  dplyr::summarise(cent_5_value = quantile(water_level, probs = 0.05, na.rm = TRUE),
                   cent_95_value = quantile(water_level, probs = 0.95, na.rm = TRUE))

# add sampled column again
site_quantsum$sampled_20 <- ifelse(site_quantsum$sampling_point %in% sampsite, 1, 0)


## save
dir.create("data/processed")

write.csv(dip_quantsum, 
          "data/processed/dip_data_5_95quantile_summary.csv")

write.csv(site_quantsum, 
          "data/processed/site_data_5_95quantile_summary.csv")


## save
write.csv(dip,
          "data/processed/dip_data_mn_5yrperc_change.csv")

write.csv(site,
          "data/processed/site_data_mn_5yrperc_change.csv")



## save unique site/year combinations
sityr_dip <- dip %>% 
  dplyr::select(sampling_point, station_name, station_number, year) %>% 
  distinct()

sityr_site <- site %>% 
  dplyr::select(sampling_point, station_name, station_number, year) %>% 
  distinct()

write.csv(sityr_dip,
          "data/processed/unique_dip_sites_years.csv")

write.csv(sityr_site,
          "data/processed/unique_nondip_sites_years.csv")







