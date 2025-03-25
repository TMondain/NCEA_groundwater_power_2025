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

source("scripts/functions/water_level_power_analysis.R")

pars <- expand.grid(response_var = c("water_level", "perc_chg_ind", "cent_5_value", "cent_95_value"),
                    sample_column = "sampled_20",
                    nsim = 100,
                    nosite.yr = seq(100, 600, by = 100),
                    site_column = "sampling_point",
                    noyear = c(5, 10, 20, 25),
                    data_proportions = c(0.2, 0.5, 0.8, 1),
                    effect.size = c(0.001, 0.002, 0.01),
                    samfreq = 1,
                    save_loc = "data/simulations/power/water_level")

dim(pars)
pars <- (dplyr::arrange(pars, response_var))
# View(pars)
colnames(pars)

dataset_dip <- rep(c("data/processed/dip_data_mn_5yrperc_change.csv", 
                     "data/processed/dip_data_5_95quantile_summary.csv"), 
                   each = dim(pars)[1]/2)

dataset_telem <- rep(c("data/processed/site_data_mn_5yrperc_change.csv", 
                       "data/processed/site_data_5_95quantile_summary.csv"), 
                     each = dim(pars)[1]/2)

yearly_samfreq <- rep(c(12, 1), each = dim(pars)[1]/2)

model_pars_mnth = rep(paste(c("month", "year", "sampling_point"), collapse = ";"), dim(pars)[1]/2)
model_pars_annual = rep(paste(c("year", "sampling_point"), collapse = ";"), dim(pars)[1]/2)

random_effect_mnth = rep(paste(c("month", "sampling_point"), collapse = ";"), dim(pars)[1]/2)
random_effect_annual = rep("sampling_point", dim(pars)[1]/2)

pars <- cbind(dataset_dip,
              dataset_telem,
              pars,
              yearly_samfreq,
              yearly_samfreq_column = "month",
              model_pars = c(model_pars_mnth, model_pars_annual),
              random_effect = c(random_effect_mnth, random_effect_annual))

dim(pars)
# View(pars)
# colnames(pars)

# convert these to as.character
pars$sample_column <- as.character(pars$sample_column)
pars$site_column <- as.character(pars$site_column)
pars$response_var <- as.character(pars$response_var)
pars$save_loc <- as.character(pars$save_loc)


### testing

rwind = 1


cols2vectors <- function(param_df, rownums, envir = .GlobalEnv) {
  for(i in 1:ncol(param_df)) {
    assign(colnames(param_df)[i], c(param_df[rownums, i]), envir = envir)
  }
}

cols2vectors(pars, rwind)
