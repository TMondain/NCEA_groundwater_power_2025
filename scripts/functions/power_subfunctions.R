

#' Extract Model Parameter Estimates for Power Analysis
#'
#' Fits a GLMM with Gamma family to estimate parameters from a dataset and extract variance components.
#'
#' @param dat A data frame containing the input data.
#' @param response A string specifying the response variable name.
#' @param model_covs A character vector of covariate names to be modelled. These are fit as random effects.
#'
#' @return A named vector of estimated parameters: intercept value, standard error, random effect standard deviations, and residual variance.
#' @export
#'
#' @import glmmTMB
#'
#' @examples
#' # get_model_pars(dat = your_data, response = "response", model_covs = c("site", "year"))
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




#' Create Simulated Sampling Data Frame
#'
#' Generates a simulated data frame representing a sampling design across sites, years, and days.
#'
#' @param samfreq Integer. Between-year sample frequency. Defaults to 1. 
#' @param nosite.yr Integer. Number of sites sampled per year.
#' @param noyear Integer. Number of years in the simulation.
#' @param site_column Character. Name of the site column.
#' @param yearly_samfreq Integer. Within-year sampling frequency.
#' @param yearly_samfreq_column Character. Column name for the within-year sampling variable (e.g. "days" or "months"). Defaults to `"days"`.
#'
#' @return A data frame representing simulated site-year-day sampling.
#' @export
#'
#' @examples
#' # create_sim_df(samfreq = 2, nosite.yr = 5, noyear = 10, yearly_samfreq = 30)
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



#' Alter Proportions of Sampled Data
#'
#' Adjusts the proportion of data used from multiple datasets based on target sampling proportions.
#'
#' @param dats_list A list of datasets to subsample from.
#' @param data_proportions Numeric vector of proportions to sample from each dataset. Must be same length as dats_list.
#' @param template_data Data frame providing the full set of possible sites. Normally the output of the `create_sim_df` function.
#' @param site_column Character. Name of the site column.
#' @param nosite.yr Integer. Number of sites per year.
#'
#' @return A list of subsampled data frames based on the specified proportions.
#' @export
#'
#' @examples
#' # alter_dat_proportions(list(dat1, dat2), c(0.7, 0.3), full_data, "site", 10)
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



#' Simulate Data from Template and Model Parameters
#'
#' Uses a sampling template and parameter values to generate simulated data under a Gamma GLMM framework. Currently only adds trends to `year` parameter. 
#'
#' @param template_dat Data frame template for simulation. Either the output of the `create_sim_df` function or `alter_dat_proportions`
#' @param model_params Named vector of parameter values from `get_model_pars`.
#' @param fixed_effect Character vector specifying the fixed effects. Must be same length as tslope.
#' @param tslope Numeric. The effect size to simulate. Adds trend to each of the fixed effects.
#' @param nsim Integer. Number of simulations to run. Defaults to 100.
#' @param model_pars Character vector of model covariates.
#'
#' @return A list of simulated data frames, each including simulated response values.
#' @export
#'
#' @examples
#' # simulate_data(template, params, tslope = -0.2, nsim = 100, model_pars = c("site", "year"))
simulate_data <- function(template_dat,
                          model_params,
                          fixed_effect,
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
    # if doing multiple fixed effects, would each effect be additive??
    # i.e. would this work?
    # fixed_effect <- model_pars[!model_pars %in% random_effect]
    
    # if(length(tslope) == 1) {
    #   # NEED TO CHANGE THIS SO THAT you can specify a slope for any of the fixed effects and
    #   # for all of them.
    #   # I.e. if you want to specify trend for month but not year...
    #   
    #   message("!! Only one effect size specified,")
    #   
    # }
    
    if(length(tslope) != length(fixed_effect)) {
      stop(paste("!! length of `effect_size` does not equal number of fixed effects"))
    }
    
    # new:
    trended_dat <- lapply(1:length(fixed_effect), function(x) {
      tslope[x]*template_dat[,fixed_effect[x]]
    })
    trended_dat <- Reduce("+", trended_dat)
    template_dat$mu <- exp(template_dat$int + trended_dat)
    
    #     # original:
    # template_dat$mu <- exp(template_dat$int + tslope * template_dat$year)
    
    #Gamma parameters
    g.shape <- 1/g.s2
    g.scale <- template_dat$mu/g.shape
    
    # Simulating values
    template_dat$g.values <- sapply(1:nrow(template_dat), function(i) {
      rtrunc(1, spec = "gamma", a = 1e-100, shape = g.shape, scale = g.scale[i])
    })
    
    data_out[[ii]] <- template_dat[,c("g.values", model_pars)]
    
  }
  
  return(data_out)
  
}



#' Run Power Analysis on Simulated Data
#'
#' Fits GLMMs to a list of simulated datasets and calculates statistical power for detecting fixed effects.
#'
#' @param simulated_data A list of data frames from `simulate_data`.
#' @param nsim Integer. Number of simulations.
#' @param random_effect Character vector of random effect variable names.
#' @param fixed_effect Character vector of fixed effect variable names.
#' @param effect.size Numeric vector of true effect sizes corresponding to the fixed effects.
#'
#' @return A data frame with estimated statistical power (%) for each fixed effect.
#' @export
#'
#' @examples
#' # run_power(simulated_data, nsim = 100, random_effect = c("site", "year"), fixed_effect = "year", effect.size = -0.2)
run_power <- function(simulated_data, # a list of dataframes, one for each simulation
                      nsim, 
                      random_effect, 
                      fixed_effect, 
                      effect.size) {
  
  #results will be stored here
  #figure out how to extract parameters for multiple fixed effects
  pvalyr0 <- sign.ts <- rep(NA,nsim)
  
  is_signif <- list()
  
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
    
    # get pvalues for >1 fixed effect
    p_vals <- lapply(fixed_effect, function(x) {
      coef.mod0[rownames(coef.mod0)==x,colnames(coef.mod0)=="Pr(>|z|)"]
    })
    
    # get sign for >1 fixed effect
    signs <- lapply(fixed_effect, function(x) {
      sign(effect.size)==sign(coef.mod0[rownames(coef.mod0)==x,
                                        colnames(coef.mod0)=="Estimate"])
    })
    
    #is signif?
    is_signif[[ii]] <- lapply(1:length(fixed_effect), function(x) {
      p_vals[[x]]<0.05 & !is.na(p_vals[[x]]) & signs[[x]]
    })
    
  }
  
  # fixed_effect
  
  # calculate power for many fixed effects
  power_outs_fe <- lapply(1:length(fixed_effect), function(x) {
    sum(unlist(lapply(is_signif, function(i) i[[x]]))) * 100/nsim
  })
  
  fpower <- do.call(cbind.data.frame, power_outs_fe)  
  colnames(fpower) <- paste0(fixed_effect, "_fpower")
  
  return(fpower)
  
}
