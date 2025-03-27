rm(list = ls())

# testing new function
source("scripts/functions/run_power_analysis.R")
source("scripts/functions/power_subfunctions.R")


### test with all the different combinations of data!!



### one dataset only 
# one dataset
pwr <- run_power_analysis(
  datasets = "data/processed/dip_data_mn_5yrperc_change.csv", # one or more datasets
  sample_column = "sampled_20",
  site_column = "sampling_point",
  model_pars = c("month", "year", "sampling_point"), ## change this to station_number
  random_effect = c("sampling_point"), ## try with single random effect
  yearly_samfreq = 12,# number of sampling occasions per year, 1 if annual, 12 if monthly
  yearly_samfreq_column = "month",
  response_var = "water_level",
  effect.size = 0.1,
  nsim = 10,
  samfreq = 1,
  nosite.yr = 100, # number of sites sampled per year across all datasets
  noyear = 5,
  data_proportions = NULL, # try with NULL - don't think it would work
  save_loc = NULL)

pwr


# with > 1 dataset
pwr <- run_power_analysis(
  datasets = paste("data/processed/dip_data_mn_5yrperc_change.csv", 
                   "data/processed/site_data_mn_5yrperc_change.csv", sep = ";"), # one or more datasets
  sample_column = "sampled_20",
  site_column = "sampling_point",
  model_pars = c("month", "year", "sampling_point"), # all of the model parameters
  random_effect = c("month", "sampling_point"), # which of model_pars are random effects
  yearly_samfreq = 12,# number of sampling occasions per year, 1 if annual, 12 if monthly
  yearly_samfreq_column = "month",
  response_var = "perc_chg_ind",
  effect.size = 0.05,
  nsim = 10,
  samfreq = 1,
  nosite.yr = 100, # number of sites sampled per year across all datasets
  noyear = 5,
  data_proportions = c(0.8, 0.2), # doesn't have to add up to 1
  save_loc = NULL)

pwr
