rm(list = ls())

# testing new function
source("scripts/functions/run_power_analysis.R")
source("scripts/functions/power_subfunctions.R")


### test with all the different combinations of data!!



### one dataset only 


# datasets = "data/processed/dip_data_mn_5yrperc_change.csv" # one or more datasets
# sample_column = "sampled_20"
# site_column = "sampling_point"
# model_pars = c("month", "year", "sampling_point") ## change this to station_number
# random_effect = c("month", "sampling_point") ## try with single random effect
# yearly_samfreq = 12# number of sampling occasions per year, 1 if annual, 12 if monthly
# yearly_samfreq_column = "month"
# response_var = "water_level"
# effect.size = 0.1
# nsim = 10
# samfreq = 1
# nosite.yr = 100 # number of sites sampled per year across all datasets
# noyear = 5
# data_proportions = NULL # try with NULL - don't think it would work
# save_loc = NULL

pwr <- run_power_analysis(
  datasets = "data/processed/dip_data_mn_5yrperc_change.csv", # one or more datasets
  sample_column = "sampled_20",
  site_column = "sampling_point",
  model_pars = c("month", "year", "sampling_point"), ## change this to station_number
  random_effect = c("month", "sampling_point"), ## try with single random effect
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



# submitting >1 dataset
datasets = paste("data/processed/dip_data_mn_5yrperc_change.csv", 
                 "data/processed/site_data_mn_5yrperc_change.csv", sep = ";")



dataset <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/water_quality_data_cleaned_IndSampOptions.csv")

scenarios <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/scenarios/N_scenarios.csv")

colnames(scenarios) <- c("X", "nosite.yr", "noyear", "effect.size", "days", "samfreq", "prox")

# number of simulations
nsim = 100

# column to do sampling by
sample_column = "ps_samp"

# response variable
response_var = grep("_value", colnames(dataset), value = TRUE)

# create pars dataframe
pars <- data.frame(data_location = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/water_quality_data_cleaned_IndSampOptions.csv",
                   response_var = rep(response_var, each = nrow(scenarios)),
                   nsim = nsim,
                   sample_column = sample_column,
                   do.call("rbind", replicate(length(response_var), 
                                              scenarios, simplify = FALSE))
)

pars$save_loc <- "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/"


dim(pars)


## resubmit failed outputs
{
  pars <- pars %>% 
    mutate(id_col = paste(response_var, nsim, nosite.yr, noyear, effect.size, days, samfreq, sep = "_"))
  
  # read the combined file
  outputs <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/outputs/power_datasets/combined_power_outputs.csv")
  
  # check which scenarios failed
  outputs <- outputs %>% 
    mutate(id_col = paste(response_var, nsim, nosite.yr, noyear, effect.size, days, samfreq, sep = "_"))
  
  pars <- pars[which(!pars$id_col %in% outputs$id_col),]
  unique(pars$response_var)
}
