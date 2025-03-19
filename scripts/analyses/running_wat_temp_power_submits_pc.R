# running water temp power submits pc

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

scenarios <- read.csv("data/scenarios/WTmp_Scenarios.csv")


pars <- scenarios %>% 
  mutate(response_var = "response",
         nosite.yr = nosite,
         prop_cont = 0,
         nsim = 100,
         dataset_dip = "data/raw/GW_temp_data.csv",
         dataset_telem = "data/raw/GW_temp_data.csv",
         model_pars = paste(c("month", "year", "sampling_point"), collapse = ";"),
         random_effect = paste(c("month", "sampling_point"), collapse = ";"),
         effect.size = eff,
         samfreq = freq,
         save_loc = "data/simulations/power/water_temp")

pars$nosite <- NULL
pars$prop_tele <- NULL
pars$freq <- NULL
pars$eff <- NULL

head(pars)

colnames(pars)
dim(pars)
# str(pars)

# convert these to as.character
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
