# testing new function
source("scripts/functions/run_power_analysis.R")
source("scripts/functions/power_subfunctions.R")



# submitting >1 dataset
datasets = paste("data/processed/dip_data_mn_5yrperc_change.csv", 
                 "data/processed/site_data_mn_5yrperc_change.csv", sep = ";")