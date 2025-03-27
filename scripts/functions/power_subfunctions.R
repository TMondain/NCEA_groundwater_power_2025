

# Get model parameter estimates
get_model_pars <- function(dat, # the dataset
                           response, # response variable
                           model_covs) { # the model covariates
  
  # create formula
  f <- formula(paste0(response, "~1+", paste("(1|",model_covs,")", collapse = "+", sep = "")))
  
  # remove NAs and 0s and infs
  dat <- na.omit(dat[,c(model_covs, response)])
  dat <- dat[dat[, response] > 0,]
  dat <- dat[is.finite(dat[,response]),]
  
  dat[model_covs] <- lapply(dat[model_covs], factor) 
  
  # run the model
  message("! Running initial model")
  mod <- glmmTMB(f,data=dat,family=Gamma(link = "log"))
  
  # extract model parameters
  ## intercept mean and standard error
  intval <- data.frame(summary(mod)$coefficients$cond)$Estimate 
  intsd <- data.frame(summary(mod)$coefficients$cond)$Std..Error
  
  # extract model standard deviation for the covariates
  mod_paramvals <- lapply(model_covs, function(x) 
    attr(summary(mod)$varcor$cond[[x]], "stddev", exact = TRUE))
  
  # combine model parameters to a vector
  modpars <- do.call(c, mod_paramvals)
  names(modpars) <- NULL
  
  # get sigma^2
  sigma2 <- summary(mod)$sigma^2
  
  # join the intercept and the modelpars together
  modpars <- c(intval, intsd, modpars, sigma2)
  
  # add the names
  names(modpars) <- c("intval", "intsd", model_covs, "sigma2")
  
  return(modpars)
  
}




# create simulation dataframe
create_sim_df <- function(samfreq = 1, # between-year sample frequency
                          nosite.yr, 
                          noyear,
                          site_column = "site", # `character` name of the site column
                          yearly_samfreq, # `numeric` within year sampling frequency
                          yearly_samfreq_column = "days") { # name of yearly samfreq model parameter
  
  if (noyear >= samfreq) nosite = nosite.yr * samfreq else {
    samfreq <- noyear
    nosite = nosite.yr * samfreq 
  } 
  
  thin.id <- paste(rep(1:nosite,floor(noyear/samfreq)),
                   rep(1:noyear,each=floor(nosite/samfreq)),sep="_")
  
  data.temp <- expand.grid(site=1:nosite, year=1:noyear)
  colnames(data.temp) <- c(site_column, "year")
  data.temp$site.yr <- paste(data.temp[, site_column], data.temp$year, sep="_")
  data0 <- data.temp[is.element(data.temp$site.yr,thin.id),]
  
  expanded_data <- data.frame()
  #loop over each site-year combination
  for (zz in 1:nrow(data0)) {
    site <- data0[,site_column][zz]
    year <- data0$year[zz]
    site_yr <- data0$site.yr[zz]
    
    #unique days from the available days in a year
    unique_days <- 1:yearly_samfreq
    
    #loop over each day for the current site-year
    for (day in unique_days) {
      
      #samples collected on the same date (within the same season and year) share a common parameter
      #samples collected during the same season share a common seasonal parameter
      day_dat <- data.frame(site = site,
                            year = year,
                            site_yr = site_yr,
                            day = day,
                            date=paste(year, day, sep="_"))
      colnames(day_dat) <- c(site_column, "year", "site_yr", yearly_samfreq_column , "date")
      
      expanded_data <- rbind(expanded_data, 
                             day_dat)
    }
  }
  
  return(expanded_data)
  
}



# alter proportion of data
alter_dat_proportions <- function(dats_list,
                                  data_proportions,
                                  template_data,
                                  site_column,
                                  nosite.yr) {
  
  if(length(dats_list) == length(data_proportions)) {
    
    nsites <- data_proportions*nosite.yr
    
  } else if(length(dats_list) != length(data_proportions) & 
            length(dats_list) == 2 &
            length(data_proportions) == 1) {
    
    # calculate number of sites to sample
    nsite_dat2 <- round(nosite.yr*data_proportions)
    nsite_dat1 <- nosite.yr-nsite_dat2
    nsites <- c(nsite_dat1, nsite_dat2)
    
  } else {
    stop(paste("! 'data_proportions' must be same length as number of datasets, or has length '1'"))
  }
  
  expanded_datlist <- lapply(nsites, function(x){
    
    smpsits <- sample(unique(template_data[,site_column]), size = x)
    template_data[template_data[,site_column] %in% smpsits,]
    
  })
  
  return(expanded_datlist)
  
}



#### simulate data according to templates
simulate_data <- function(template_dat,
                          model_params,
                          tslope,
                          nsim = 100, 
                          model_pars){
  
  
  # #effect.size 
  # #note: here I tested whether there is a difference when simulating positive or negative effect sizes,  
  # #but there is no difference in the results. So, only simulating the negative effect
  # tslope <- effect.size
  
  data_out <- list()
  
  #loop over simulations
  for (ii in 1:nsim){
    
    if(ii %% 10 == 0)
      cat(paste0("iteration: ", ii, "\n"))
    
    #number of unique sites (this number depends on sampling frequency 
    #(annual freq. will have the smallest number of unique sites))
    
    ########################################################################
    # simulating
    ########################################################################
    
    # simulate the intercept (mean value)
    sim.int <- rtnorm(1, 
                      mean=model_params["intval"], 
                      sd=model_params["intsd"])
    
    ### add variation of each model parameter to the intercept
    
    # get variation associated with each of the model pars
    par_variab <- lapply(1:length(model_pars), function(i) {
      
      unique_levels <- unique(template_dat[,model_pars[i]])
      
      n_samples <- length(unique_levels)
      
      int_with_var <- sim.int + 
        rnorm(n_samples, 
              mean=0, 
              sd=model_params[model_pars[i]])
      
      dat_out <- data.frame(unique_levels, int_with_var)  
      colnames(dat_out) <- c(model_pars[i], "int_with_var")
      dat_out
      
    })
    
    # expand the variation for each of the model pars to the full template_dat size 
    variab_expanded <- lapply(par_variab, function(x) {
      
      x[,"int_with_var"][match(template_dat[, colnames(x)[1]], x[,1])]
      
    })
    
    
    # add the list elements together to get the variability associated with all
    # of the model pars combined 
    simint_variability <- Reduce("+", variab_expanded)
    
    template_dat$int <- simint_variability
    
    
    ########################################################################
    # sim values
    ########################################################################
    
    #sigma2
    g.s2 <- model_params["sigma2"] #0.121
    
    #expected mean (mu)
    template_dat$mu <- exp(template_dat$int + tslope * template_dat$year)
    
    #Gamma parameters
    g.shape <- 1/g.s2
    g.scale <- template_dat$mu/g.shape
    
    # Simulating values
    template_dat$g.values <- sapply(1:nrow(template_dat), function(i) {
      rtrunc(1, spec = "gamma", a = 1e-6, shape = g.shape, scale = g.scale[i])
    })
    
    data_out[[ii]] <- template_dat[,c("g.values", model_pars)]
    
  }
  
  return(data_out)
  
}



# run the power analysis
run_power <- function(simulated_data, # a list of dataframes, one for each simulation
                      nsim, 
                      random_effect, 
                      fixed_effect, 
                      effect.size) {
  
  #results will be stored here
  #figure out how to extract parameters for multiple fixed effects
  pvalyr0 <- sign.ts <- rep(NA,nsim)
  
  fpower0 <- rep(NA,nsim)
  
  #results will be stored here
  for(ii in 1:nsim) {
    
    if(ii %% 10 == 0)
      cat(paste0("iteration: ", ii, "\n"))
    
    for(rf in 1:length(random_effect)){
      simulated_data[[ii]][,random_effect[rf]] <- as.factor(simulated_data[[ii]][,random_effect[rf]])
    }
    
    
    f <- formula(paste0("g.values~", paste(fixed_effect, collapse = "+"), "+",  
                        paste("(1|",random_effect,")", collapse = "+", sep = "")))
    
    #comparing models
    mod0 <- glmmTMB(f,data=simulated_data[[ii]],family=Gamma(link = "log"))
    
    #extracting coefficients and their p-values 
    coef.mod0 <- coef(summary(mod0))$cond
    
    
    ## this is only written for one fixed effect!!!!!
    pvalyr0[[ii]] <- coef.mod0[rownames(coef.mod0)=="year",colnames(coef.mod0)=="Pr(>|z|)"]
    
    sign.ts[[ii]] <- sign(effect.size)==sign(coef.mod0[rownames(coef.mod0)=="year",
                                                       colnames(coef.mod0)=="Estimate"])
    
  }
  
  fpower0 <- length(which(pvalyr0<0.05 & !is.na(pvalyr0) & sign.ts))*100/length(sort(pvalyr0))
  
  return(fpower0)
  
}
