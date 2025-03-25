# testing new function
source("scripts/functions/run_power_analysis.R")
source("scripts/functions/power_subfunctions.R")


run_power_analysis(
    datasets, # one or more datasets
    # dataset_dip,
    # dataset_telem,
    sample_column = NULL,
    site_column,
    model_pars,
    random_effect,
    yearly_samfreq,# number of sampling occasions per year, 1 if annual, 12 if monthly
    yearly_samfreq_column_name,
    response_var,
    effect.size,
    nsim,
    samfreq,
    nosite.yr, # number of sites sampled per year across all datasets
    noyear,
    data_proportions,
    save_loc) 

# submitting >1 dataset
datasets = paste("data/processed/dip_data_mn_5yrperc_change.csv", 
                 "data/processed/site_data_mn_5yrperc_change.csv", sep = ";")