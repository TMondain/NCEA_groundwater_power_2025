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

source("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/scripts/functions/water_level_power_analysis.R")

scenarios <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/scenarios/WTmp_scenarios.csv")


pars <- scenarios %>% 
  mutate(response_var = "response",
         nosite.yr = nosite,
         prop_cont = 0,
         nsim = 100,
         dataset_dip = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/GW_temp_data.csv",
         dataset_telem = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/GW_temp_data.csv",
         model_pars = paste(c("month", "year", "sampling_point"), collapse = ";"),
         random_effect = paste(c("month", "sampling_point"), collapse = ";"),
         effect.size = eff,
         samfreq = freq,
         save_loc = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/water_temp")

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


# View(pars)

# submit
sjob <- slurm_apply(water_level_power_analysis, 
                    pars, 
                    jobname = "ncea_pwr_temp",
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

rwind = 1


cols2vectors <- function(param_df, rownums, envir = .GlobalEnv) {
  for(i in 1:ncol(param_df)) {
    assign(colnames(param_df)[i], c(param_df[rownums, i]), envir = envir)
  }
}

cols2vectors(pars, rwind)

