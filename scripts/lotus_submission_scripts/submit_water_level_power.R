# submit power analysis to lotus 
rm(list = ls())

########################################################################
# Libraries
########################################################################
library(readxl)
library(extraDistr)
library(EnvStats)
library(glmmTMB)
library(crayon)
library(gdata)
library(truncnorm)
library(ggplot2)
library(truncdist)
library(rslurm)
library(tidyverse)
########################################################################
# Reading inputs
########################################################################

source("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/scripts/functions/water_level_power_analysis.R")

pars <- expand.grid(response_var = c("water_level", "perc_chg_ind", "cent_5_value", "cent_95_value"),
                    sample_column = "sampled_20",
                    nsim = 100,
                    nosite.yr = seq(100, 600, by = 100),
                    noyear = c(5, 10, 20, 25),
                    prop_cont = c(0.2, 0.5, 0.8, 1),
                    effect.size = c(0.001, 0.002, 0.01),
                    samfreq = 1,
                    save_loc = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/water_level")

dim(pars)
pars <- (dplyr::arrange(pars, response_var))
# View(pars)
colnames(pars)

dataset_dip <- rep(c("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/processed/water_level/dip_data_mn_5yrperc_change.csv", 
                     "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/processed/water_level/dip_data_5_95quantile_summary.csv"), 
                   each = dim(pars)[1]/2)

dataset_telem <- rep(c("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/processed/water_level/site_data_mn_5yrperc_change.csv", 
                       "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/processed/water_level/site_data_5_95quantile_summary.csv"), 
                     each = dim(pars)[1]/2)

days <- rep(c(12, 1), each = dim(pars)[1]/2)

model_pars_mnth = rep(paste(c("month", "year", "sampling_point"), collapse = ";"), dim(pars)[1]/2)
model_pars_annual = rep(paste(c("year", "sampling_point"), collapse = ";"), dim(pars)[1]/2)

random_effect_mnth = rep(paste(c("month", "sampling_point"), collapse = ";"), dim(pars)[1]/2)
random_effect_annual = rep("sampling_point", dim(pars)[1]/2)

pars <- cbind(dataset_dip,
              dataset_telem,
              pars,
              days,
              model_pars = c(model_pars_mnth, model_pars_annual),
              random_effect = c(random_effect_mnth, random_effect_annual))

dim(pars)
# View(pars)
# colnames(pars)

# convert these to as.character
pars$sample_column <- as.character(pars$sample_column)
pars$response_var <- as.character(pars$response_var)
pars$save_loc <- as.character(pars$save_loc)

# View(pars)

# submit
sjob <- slurm_apply(water_level_power_analysis, 
                    pars, 
                    jobname = "ncea_pwr_lvl",
                    nodes = nrow(pars),
                    cpus_per_node = 1, 
                    submit = TRUE,
                    slurm_options = list(account = "ceh_generic",
                                         partition = "standard",
                                         qos = "short",
                                         time = "03:59:59",
                                         mem = "10000",
                                         output = "pwr_%a.out",
                                         error = "pwr_%a.err"),
                    sh_template = "jasmin_submit_sh.txt")






### testing

rwind = 292


cols2vectors <- function(param_df, rownums, envir = .GlobalEnv) {
  for(i in 1:ncol(param_df)) {
    assign(colnames(param_df)[i], c(param_df[rownums, i]), envir = envir)
  }
}

cols2vectors(pars, rwind)


# pwr <- power_analysis(data_location = pars$data_location[rwind],
#                       response_var = pars$response_var[rwind],
#                       nsim = pars$nsim[rwind],
#                       noyear = pars$noyear[rwind],
#                       nosite.yr = pars$nosite.yr[rwind],
#                       samfreq = pars$samfreq[rwind],
#                       effect.size = pars$effect.size[rwind],
#                       days = pars$days[rwind],
#                       save_loc = NULL)



# # example function call
# water_level_power_analysis(
#   dataset_dip = read.csv("data/processed/dip_data_5_95quantile_summary.csv"),
#   dataset_telem = read.csv("data/processed/site_data_5_95quantile_summary.csv"),
#   sample_column = "sampled_20",
#   model_pars = c("year", "sampling_point"),
#   random_effect = c("sampling_point"),
#   days = 1, # number of sampling occasions per year, 1 if annual, 12 if monthly
#   response_var = "cent_5_value",
#   effect.size = 0.002,
#   nsim = 100,
#   samfreq = 1,
#   nosite.yr = 100, # number of sites sampled per year across all datasets
#   noyear = 5,
#   prop_cont = 0.2, # proportion of continuous data
#   save_loc = NULL)



### below is stuff from the water quality analysis - use as a template

# source("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/scripts/functions/power_analysis.R")
# 
# dataset <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/water_quality_data_cleaned_IndSampOptions.csv")
# 
# scenarios <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/scenarios/N_scenarios.csv")
# 
# colnames(scenarios) <- c("X", "nosite.yr", "noyear", "effect.size", "days", "samfreq", "prox")
# 
# # number of simulations
# nsim = 100
# 
# # column to do sampling by
# sample_column = "ps_samp"
# 
# # response variable
# response_var = grep("_value", colnames(dataset), value = TRUE)
# 
# # create pars dataframe
# pars <- data.frame(data_location = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/water_quality_data_cleaned_IndSampOptions.csv",
#                    response_var = rep(response_var, each = nrow(scenarios)),
#                    nsim = nsim,
#                    sample_column = sample_column,
#                    do.call("rbind", replicate(length(response_var), 
#                                               scenarios, simplify = FALSE))
# )
# 
# pars$save_loc <- "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/"
# 
# 
# dim(pars)
# 
# 
# ## resubmit failed outputs
# {
#   pars <- pars %>% 
#     mutate(id_col = paste(response_var, nsim, nosite.yr, noyear, effect.size, days, samfreq, sep = "_"))
#   
#   # read the combined file
#   outputs <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/outputs/power_datasets/combined_power_outputs.csv")
#   
#   # check which scenarios failed
#   outputs <- outputs %>% 
#     mutate(id_col = paste(response_var, nsim, nosite.yr, noyear, effect.size, days, samfreq, sep = "_"))
#   
#   pars <- pars[which(!pars$id_col %in% outputs$id_col),]
#   unique(pars$response_var)
# }
# 
# pars$id_col <- NULL
# pars$X <- NULL
# pars$prox <- NULL
# 
# dim(pars)
# 
# # submit
# sjob <- slurm_apply(power_analysis, 
#                     pars, 
#                     jobname = "ncea_pwr",
#                     nodes = nrow(pars),
#                     cpus_per_node = 1, 
#                     submit = TRUE,
#                     slurm_options = list(account = "ceh_generic",
#                                          partition = "standard",
#                                          qos = "standard",
#                                          time = "23:59:59",
#                                          mem = "10000",
#                                          output = "pwr_%a.out",
#                                          error = "pwr_%a.err"),
#                     sh_template = "jasmin_submit_sh.txt")





# 
# cols.num <- grep("_value", colnames(dataset), value = TRUE)
# dataset[cols.num] <- sapply(dataset[cols.num],as.numeric)
# 
# sapply(dataset[cols.num],function(x) range(x, na.rm = TRUE))
# sapply(dataset[cols.num],function(x) min(x, na.rm = TRUE))
# sapply(dataset[cols.num],function(x) sum(x == 0, na.rm = TRUE))
# 
# 
# # # to store results
# # fpower0 <- fpower_LR0 <- fpower_LR0_anova <-rep(NA,length(scenarios[,1]))
# 
# rwind <- 1508
# 
# # pwr <- power_analysis(data_location = dataset,
# #                       response_var = pars$response_var[rwind], 
# #                       nsim = pars$nsim[rwind],
# #                       noyear = pars$noyear[rwind], 
# #                       nosite.yr = pars$nosite.yr[rwind], 
# #                       samfreq = pars$samfreq[rwind], 
# #                       effect.size = pars$effect.size[rwind], 
# #                       days = pars$days[rwind],
# #                       save_loc = NULL)
# 
# 
# 
# # cols2vectors <- function(param_df, rownums, envir = .GlobalEnv) {
# #   for(i in 1:ncol(param_df)) {
# #     assign(colnames(param_df)[i], c(param_df[rownums, i]), envir = envir)
# #   }
# # }
# 
# cols2vectors(pars, 1508)
# 
# 
# 
# ## submit job
# #SBATCH --job-name="My test job"
# #SBATCH --time=00:01:00
# #SBATCH --mem=1M
# #SBATCH --account=mygws
# #SBATCH --partition=debug
# #SBATCH --qos=debug
