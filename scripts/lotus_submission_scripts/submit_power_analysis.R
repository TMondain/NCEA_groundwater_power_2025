# submit power analysis to lotus 

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
########################################################################
# Reading inputs
########################################################################

source("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/scripts/functions/power_analysis.R")

dataset <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/water_quality_data_cleaned.csv")

scenarios <- read.csv("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/scenarios/N_scenarios.csv")

colnames(scenarios) <- c("X", "nosite.yr", "noyear", "effect.size", "days", "samfreq", "prox"  )

# number of simulations
nsim = 100

# response variable
response_var = grep("_value", colnames(dataset), value = TRUE)

# create pars dataframe
pars <- data.frame(data_location = "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/raw/water_quality_data_cleaned.csv",
                   response_var = rep(response_var, each = nrow(scenarios)),
                   nsim = 100,
                   do.call("rbind", replicate(length(response_var), 
                                              scenarios, simplify = FALSE))
)

pars$save_loc <- "/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/"
pars$X <- NULL
pars$prox <- NULL

dim(pars)


# submit
sjob <- slurm_apply(power_analysis, 
                    pars, 
                    jobname = "ncea_pwr",
                    nodes = nrow(pars),
                    cpus_per_node = 1, 
                    submit = TRUE,
                    slurm_options = list(account = "ceh_generic",
                                         partition = "standard",
                                         qos = "short",
                                         time = "3:59:59",
                                         mem = "10000",
                                         output = "pwr_%a.out",
                                         error = "pwr_%a.err"),
                    sh_template = "jasmin_submit_sh.txt")









cols.num <- grep("_value", colnames(dataset), value = TRUE)
dataset[cols.num] <- sapply(dataset[cols.num],as.numeric)

sapply(dataset[cols.num],function(x) range(x, na.rm = TRUE))
sapply(dataset[cols.num],function(x) min(x, na.rm = TRUE))
sapply(dataset[cols.num],function(x) sum(x == 0, na.rm = TRUE))


# # to store results
# fpower0 <- fpower_LR0 <- fpower_LR0_anova <-rep(NA,length(scenarios[,1]))

rwind <- 1508

# pwr <- power_analysis(data_location = dataset,
#                       response_var = pars$response_var[rwind], 
#                       nsim = pars$nsim[rwind],
#                       noyear = pars$noyear[rwind], 
#                       nosite.yr = pars$nosite.yr[rwind], 
#                       samfreq = pars$samfreq[rwind], 
#                       effect.size = pars$effect.size[rwind], 
#                       days = pars$days[rwind],
#                       save_loc = NULL)



# cols2vectors <- function(param_df, rownums, envir = .GlobalEnv) {
#   for(i in 1:ncol(param_df)) {
#     assign(colnames(param_df)[i], c(param_df[rownums, i]), envir = envir)
#   }
# }

cols2vectors(pars, 1508)



## submit job
#SBATCH --job-name="My test job"
#SBATCH --time=00:01:00
#SBATCH --mem=1M
#SBATCH --account=mygws
#SBATCH --partition=debug
#SBATCH --qos=debug
