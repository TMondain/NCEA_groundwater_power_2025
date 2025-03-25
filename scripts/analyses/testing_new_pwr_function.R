# testing new function
source("scripts/functions/water_level_power_analysis.R")

# submitting >1 dataset
datasets = paste("data/processed/dip_data_mn_5yrperc_change.csv", 
                 "data/processed/site_data_mn_5yrperc_change.csv", sep = ";")