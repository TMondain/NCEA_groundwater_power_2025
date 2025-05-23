# running water qual NEW power submits pc

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

source("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/scripts/functions/run_power_analysis.R")
source("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/scripts/functions/power_subfunctions.R")

wqdf <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/WaterQual_data_type.csv")

scenarios <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/scenarios/N_scenarios_reg_NEW.csv")


pars <- scenarios %>% 
  mutate(nosite.yr = nosite,
         site_column = "sampling_point",
         # data_proportions = 0,
         nsim = 100,
         datasets = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/WaterQual_data_type.csv",
         # dataset_telem = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/WaterQual_data_type.csv",
         model_pars = paste(c("year", "sampling_point"), collapse = ";"),
         random_effect = paste(c("sampling_point"), collapse = ";"),
         effect.size = eff,
         yearly_samfreq = days,
         samfreq = freq,
         save_loc = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/water_qual_reg_NEW")

pars$nosite <- NULL
pars$prox <- NULL
pars$prop_tele <- NULL
pars$freq <- NULL
pars$eff <- NULL
pars$days <- NULL

response_var = grep("_value", colnames(wqdf), value = TRUE) #c("gw0117_value", "gw0172_value")

wqdf$type <- NULL
sample_column = grep("type", colnames(wqdf), value = TRUE)


c.names <- colnames(pars)

# expand.grid for dat frames
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
pars <- expand.grid.df(response_var, 
                       sample_column, 
                       pars)
colnames(pars) <- c("response_var", "sample_column", c.names)

head(pars)

colnames(pars)
dim(pars)
# str(pars)

# convert these to as.character
pars$response_var <- as.character(pars$response_var)
pars$save_loc <- as.character(pars$save_loc)



# View(pars)

# submit
sjob <- slurm_apply(run_power_analysis, 
                    pars, 
                    jobname = "NEW_qual_ncea_pwr_qual_reg",
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
                    global_objects = c(c(lsf.str())),
                    sh_template = "jasmin_submit_sh.txt")






### testing

rwind = 904


cols2vectors <- function(param_df, rownums, envir = .GlobalEnv) {
  for(i in 1:ncol(param_df)) {
    assign(colnames(param_df)[i], c(param_df[rownums, i]), envir = envir)
  }
}

cols2vectors(pars, rwind)

