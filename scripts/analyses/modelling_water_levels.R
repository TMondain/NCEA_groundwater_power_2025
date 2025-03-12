
rm(list = ls())

require(readxl)
require(extraDistr)
require(EnvStats)
require(glmmTMB)
require(crayon)
require(gdata)
require(truncnorm)
require(ggplot2)
require(truncdist)




dip = read.csv("data/processed/dip_data_mn_5yrperc_change.csv")
head(dip)

telem = read.csv("data/processed/site_data_mn_5yrperc_change.csv")
head(telem)

sample_column_name = "sampled_20"

model_pars = c("month", "year", "sampling_point")

days = 12 # number of sampling occasions per year, 1 if annual, 12 if monthly

response_var = "water_level"

effect.size = 0.01


#### sample --------------------------------------------------------------------

### change this
dipsamp <- dip[dip[,sample_column_name] == 1,]

telemsamp <- telem[telem[,sample_column_name] == 1,]




#### model ---------------------------------------------------------------------
#### make this into a function?
get_model_pars <- function(dat, # the dataset
                           response, # response variable
                           model_covs) { # the model covariates
  
  # create formula
  f <- formula(paste0(response, "~1+", paste("(1|",model_covs,")", collapse = "+", sep = "")))
  
  # remove NAs and 0s
  dat <- na.omit(dat[,c(model_covs, response)])
  dat <- dat[dat[, response] > 0,]
  
  # run the model
  message("! Running initial model")
  mod <- glmmTMB(f,data=dat,family=Gamma(link = "log"))
  
  # extract model parameters
  ## intercept mean and standard error
  intval <- data.frame(summary(mod)$coefficients$cond)$Estimate #0.878
  intsd <- data.frame(summary(mod)$coefficients$cond)$Std..Error #0.026
  
  # extract model standard deviation for the covariates
  mod_paramvals <- lapply(model_covs, function(x) 
    attr(summary(mod)$varcor$cond[[x]], "stddev", exact = TRUE))
  
  # combine model parameters to a vector
  modpars <- do.call(c, mod_paramvals)
  names(modpars) <- NULL
  
  # join the intercept and the modelpars together
  modpars <- c(intval, intsd, modpars)
  
  # add the names
  names(modpars) <- c("intval", "intsd", model_covs)
  
  return(modpars)
  
}

# run the function
model_para_vals <- lapply(list(dipsamp, telemsamp), function(x)
  get_model_pars(dat = x, response = response_var, model_covs = model_pars))

# # create formula
# f <- formula(paste0(response_var, "~1+", paste("(1|",model_pars,")", collapse = "+", sep = "")))
# 
# # run the model
# message("! Running initial model")
# 
# # remove NAs and 0s
# dat <- na.omit(dat[,c(model_pars, response_var)])
# dat <- dat[dat[, response_var] > 0,]
# head(dat)
# 
# modparam <- glmmTMB(f,data=dat,family=Gamma(link = "log"))
# 
# # extract model parameters
# ## intercept mean and standard error
# intval = data.frame(summary(modparam)$coefficients$cond)$Estimate #0.878
# intsd = data.frame(summary(modparam)$coefficients$cond)$Std..Error #0.026
# 
# # extract model standard deviation for the covariates
# mod_paramvals <- lapply(model_pars, function(x) 
#   attr(summary(modparam)$varcor$cond[[x]], "stddev", exact = TRUE))
# 
# data.frame(intval, intsd)
# 
# data.frame(model_pars, do.call(rbind, mod_paramvals))
# 
# # check that it works - all match
# mthsd = attr(summary(modparam)$varcor$cond$month, "stddev", exact = TRUE)
# stsd = attr(summary(modparam)$varcor$cond$sampling_point, "stddev", exact = TRUE) #1.58
# yrsd = attr(summary(modparam)$varcor$cond$year, "stddev", exact = TRUE) #0.031




# create the template dataframe ------------------------------------------------
# - this is the same for every simulation 

create_sim_df <- function(samfreq = 1, nosite.yr, noyear, days) {
  
  if (noyear >= samfreq) nosite = nosite.yr * samfreq else {
    samfreq <- noyear
    nosite = nosite.yr * samfreq 
  } 
  thin.id <- paste(rep(1:nosite,floor(noyear/samfreq)),
                   rep(1:noyear,each=floor(nosite/samfreq)),sep="_")
  
  data.temp <- expand.grid(site=1:nosite, year=1:noyear)
  data.temp$site.yr <- paste(data.temp$site, data.temp$year, sep="_")
  data0 <- data.temp[is.element(data.temp$site.yr,thin.id),]
  
  expanded_data <- data.frame()
  #loop over each site-year combination
  for (zz in 1:nrow(data0)) {
    site <- dat$site[zz]
    year <- dat$year[zz]
    site_yr <- dat$site.yr[zz]
    
    # #the number of days for this site-year
    # days <- scenarios$days[ss]
    
    #unique days from the available days in a year
    unique_days <- 1:days
    
    #loop over each day for the current site-year
    for (day in unique_days) {
      
      #samples collected on the same date (within the same season and year) share a common parameter
      #samples collected during the same season share a common seasonal parameter
      expanded_data <- rbind(expanded_data, 
                             data.frame(site = site,
                                        year = year,
                                        site_yr = site_yr,
                                        day = day,
                                        date=paste(year, day, sep="_")))
    }
  }
  
  return(expanded_data)
  
}

expanded_data <- create_sim_df(samfreq = 1, nosite.yr = 100, noyear = 5, days = 12)

# add month
expanded_data$month <- expanded_data$day 

# View the first few rows of the expanded data
head(expanded_data)
table(expanded_data[,c("site","year")])
length(unique(expanded_data$site))




#### simulation ----------------------------------------------------------------


########################################################################
# Starting simulations for this scenario
########################################################################

template_dat = expanded_data
model_params = model_para_vals[[1]]
nsim = nsim 
nosite = 100

#### function starts here
simulate_power <- function(template_dat){}

#results will be stored here
pval0 <- LR0 <- LR0_anova <- sign.ts <- rep(NA,nsim)
#results will be stored here

#effect.size 
#note: here I tested whether there is a difference when simulating positive or negative effect sizes,  
#but there is no difference in the results. So, only simulating the negative effect
#effect.size is always negative
tslope <- effect.size
#tslope <- -(effect.size)#positive



message("! Starting simulations")

#loop over simulations
for (ii in 1:nsim){
  
  if(ii %% 10 == 0)
    cat(paste0("iteration: ", ii, "\n"))
  
  #number of unique sites (this number depends on sampling frequency 
  #(annual freq. will have the smallest number of unique sites))
  
  
  #### need to alter the proportion - should this be done outside of the function?
  # or should I add a proportion call to this
  
  
  ########################################################################
  # simulating
  ########################################################################
  
  ### need to work out how to incorporate days into the simulation of data
  # simulate the intercept (mean value)
  sim.int <- rtnorm(1, 
                    mean=model_params["intval"], 
                    sd=model_params["intsd"])
  
  ## go through each of the model_pars and add variation to the intercept
  for(i in 1:length(model_pars)) {
    n_samples <- length(unique(template_dat[,model_pars[i]]))
    sim.int <- sim.int + 
      rnorm(nosite, 
            mean=0, 
            sd=model_params[model_pars[i]])
    
  }
  
  
  
  #adding site variability
  int.df <- data.frame(int = sim.int + 
                         rnorm(nosite, 
                               mean=0, 
                               sd=model_params["sampling_point"]), 
                       site = unique(expanded_data$site))
  
  template_dat$int <- int.df$int[match(template_dat$site, int.df$site)]
  
  #adding monthly variability 
  month_levels <- unique(days)
  
  #adding year variability
  year_levels <- unique(expanded_data$year) 
  year_effects <- rnorm(length(year_levels), 
                        mean = 0, 
                        sd = yrsd)
  
  year_df <- data.frame(year = year_levels, year_effect = year_effects)
  template_dat <- merge(template_dat, year_df, by = "year", all.x = TRUE)
  template_dat$int <- template_dat$int + expanded_data$year_effect
  
  
  ########################################################################
  # sim values
  ########################################################################
  
  #sigma2
  g.s2 <- summary(modparam)$sigma^2 #0.121
  
  #expected mean (mu)
  expanded_data$mu <- exp(expanded_data$int + tslope * expanded_data$year)
  
  #Gamma parameters
  g.shape <- 1/g.s2
  g.scale <- expanded_data$mu/g.shape
  
  # Simulating values
  expanded_data$g.values <- sapply(1:nrow(expanded_data), function(i) {
    rtrunc(1, spec = "gamma", a = 1e-6, shape = g.shape, scale = g.scale[i])
  })
  
  data1 <- expanded_data[,c("g.values","site","year")]
  
  ########################################################################
  #Power analysis
  ########################################################################
  data1$site <- as.factor(data1$site)
  #comparing models
  mod0 <- glmmTMB(g.values~year+(1|site),data=data1,family=Gamma(link = "log"))
  
  #extracting coefficients and their p-values 
  coef.mod0 <- coef(summary(mod0))$cond
  pval0[ii] <- coef.mod0[rownames(coef.mod0)=="year",colnames(coef.mod0)=="Pr(>|z|)"]
  
  #checking if the estimated Year effect is in the same direction (positive/negative) 
  #as the simulated one
  sign.ts[ii] <- sign(tslope)==sign(coef.mod0[rownames(coef.mod0)=="year",
                                              colnames(coef.mod0)=="Estimate"])
  
  
  rm(mod0)#to ensure that convergence issues do not result in the same results being repeated
  # rm(mod_comp0)
  
}


message("! finished simulations")

#estimated power using p-values
fpower0 <- length(which(pval0<0.05 & !is.na(pval0) & sign.ts))*100/length(sort(pval0))

outs <- data.frame(response_var, nsim, nosite.yr, noyear, effect.size, 
                   days, samfreq, fpower0)









