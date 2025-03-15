
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


# dataset_dip = read.csv("data/processed/dip_data_mn_5yrperc_change.csv")
# 
# dataset_telem = read.csv("data/processed/site_data_mn_5yrperc_change.csv")
# 
# sample_column = "sampled_20"
# 
# model_pars = c("month", "year", "sampling_point")
# 
# random_effect = c("month", "sampling_point")
# 
# days = 12 # number of sampling occasions per year, 1 if annual, 12 if monthly
# 
# response_var = "water_level"
# 
# effect.size = 0.01
# 
# nsim = 100
# 
# samfreq = 1
# 
# nosite.yr = 100 # number of sites sampled per year across all datasets
# 
# noyear = 5
# 
# prop_cont = 0.2
# 
# save_loc = NULL

water_power_analysis <- function(
    dataset_dip,
    dataset_telem,
    sample_column,
    model_pars,
    random_effect,
    days,# number of sampling occasions per year, 1 if annual, 12 if monthly
    response_var,
    effect.size,
    nsim,
    samfreq,
    nosite.yr, # number of sites sampled per year across all datasets
    noyear,
    prop_cont,
    save_loc) {
  
  #### sample ------------------------------------------------------------------
  
  ### change this
  if(!is.null(sample_column)){
  dipsamp <- dataset_dip[dataset_dip[,sample_column] == 1,]
  
  telemsamp <- dataset_telem[dataset_telem[,sample_column] == 1,]
  }else{
    dipsamp <- dataset_dip
    telemsamp <- dataset_telem
  }
  
  # combine into single list?
  dats_list <- list(dipsamp, telemsamp)
  
  
  #### initial model -----------------------------------------------------------
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
    
    # get sigma^2
    sigma2 <- summary(mod)$sigma^2
    
    # join the intercept and the modelpars together
    modpars <- c(intval, intsd, modpars, sigma2)
    
    # add the names
    names(modpars) <- c("intval", "intsd", model_covs, "sigma2")
    
    return(modpars)
    
  }
  
  # run the function
  model_para_vals <- lapply(dats_list, function(x)
    get_model_pars(dat = x, response = response_var, model_covs = model_pars))
  
  
  
  
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
      site <- data0$site[zz]
      year <- data0$year[zz]
      site_yr <- data0$site.yr[zz]
      
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
  
  expanded_data <- create_sim_df(samfreq = samfreq, 
                                 nosite.yr = nosite.yr, 
                                 noyear = noyear, 
                                 days = days)
  
  if("month" %in% model_pars)
    # add month
    expanded_data$month <- expanded_data$day 
  
  if("sampling_point" %in% model_pars)
    expanded_data$sampling_point <- expanded_data$site
  
  # View the first few rows of the expanded data
  head(expanded_data)
  table(expanded_data[,c("site","year")])
  length(unique(expanded_data$site))
  
  
  
  #### alter number of sites from each dataset -----------------------------------
  head(expanded_data)
  
  ## could write this to work for any number of datasets
  
  if(length(dats_list)>1) {
    
    # calculate number of sites to sample
    nsite_dat2 <- round(nosite.yr*prop_cont)
    nsite_dat1 <- nosite.yr-nsite_dat2
    
    nsites <- c(nsite_dat1, nsite_dat2)
    
    expanded_datlist <- lapply(nsites, function(x){
      
      smpsits <- sample(unique(expanded_data$sampling_point), size = x)
      expanded_data[expanded_data$sampling_point %in% smpsits,]
      
    })
  }
  
  # I think this worked
  lapply(expanded_datlist, dim)
  
  
  
  
  #### simulation ----------------------------------------------------------------
  
  
  ########################################################################
  # Starting simulations for this scenario
  ########################################################################
  
  # template_dat = expanded_data
  # model_params = model_para_vals[[1]]
  # nsim = nsim
  # nosite = 100
  # model_pars = model_pars
  
  #### function starts here
  simulate_data <- function(template_dat,
                            model_params,
                            nsim = 100, 
                            model_pars){
    
    
    #effect.size 
    #note: here I tested whether there is a difference when simulating positive or negative effect sizes,  
    #but there is no difference in the results. So, only simulating the negative effect
    #effect.size is always negative
    tslope <- effect.size
    #tslope <- -(effect.size)#positive
    
    
    data_out <- list()
   
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
      
      # ## go through each of the model_pars and add variation to the intercept
      # for(i in 1:length(model_pars)) {
      #   n_samples <- length(unique(template_dat[,model_pars[i]]))
      #   sim.int <- sim.int + 
      #     rnorm(n_samples, 
      #           mean=0, 
      #           sd=model_params[model_pars[i]])
      #   
      # }
      
      
      ### add variation to intercept
      par_variab <- lapply(1:length(model_pars), function(i) {
        
        unique_levels <- unique(template_dat[,model_pars[i]])
        
        n_samples <- length(unique_levels)
        
        int_with_var <- sim.int + 
          rnorm(n_samples, 
                mean=0, 
                sd=model_params[model_pars[i]])
        
        dat_out <- cbind(unique_levels, int_with_var)  
        colnames(dat_out) <- c(model_pars[i], "int_with_var")
        
        dat_out
        
      })
      
      
      variab_expanded <- lapply(par_variab, function(x) {
        
        x[,"int_with_var"][match(template_dat[, colnames(x)[1]], x[,1])]
        
      })
      
      
      # add the list elements together
      simint_variability <- Reduce("+", variab_expanded)
      
      template_dat$int <- simint_variability
      
      
      # #adding site variability
      # int.df <- data.frame(int = sim.int + 
      #                        rnorm(nosite, 
      #                              mean=0, 
      #                              sd=model_params["sampling_point"]), 
      #                      site = unique(expanded_data$site))
      # 
      # template_dat$int <- int.df$int[match(template_dat$site, int.df$site)]
      # 
      # #adding monthly variability 
      # month_levels <- unique(days)
      # 
      # #adding year variability
      # year_levels <- unique(expanded_data$year) 
      # year_effects <- rnorm(length(year_levels), 
      #                       mean = 0, 
      #                       sd = yrsd)
      # 
      # year_df <- data.frame(year = year_levels, year_effect = year_effects)
      # template_dat <- merge(template_dat, year_df, by = "year", all.x = TRUE)
      # template_dat$int <- template_dat$int + expanded_data$year_effect
      
      
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
  message("! Starting simulations")
  
  simdat_list <- lapply(1:length(model_para_vals), function(i) 
    simdat <- simulate_data(template_dat = expanded_datlist[[i]],
                            model_params = model_para_vals[[i]],
                            nsim = nsim,
                            model_pars = model_pars)
  )
  
  message("! finished simulations")
  
  #### combine the data ----------------------------------------------------------
  
  # this loops through the number of simulations and combines the simulations from
  # the two datasets (works for many datasets though)
  comb_dat <- lapply(1:nsim, function(x) {
    do.call(rbind,lapply(1:length(simdat_list), function(i) {
      simdat_list[[i]][[x]]
    }))
  })
  
  
  ########################################################################
  #Power analysis
  ########################################################################
  
  # simulated_data = comb_dat
  # 
  # fixed_effect = model_pars[-which(model_pars %in% random_effect)]
  # 
  # effect.size = effect.size
  
  run_power <- function(simulated_data, 
                        nsim, 
                        # model_pars, 
                        random_effect, 
                        fixed_effect, 
                        effect.size) {
    
    #results will be stored here
    pvalyr0 <- sign.ts <- rep(NA,nsim) # pvalmnth0 <- 
    
    fpower0 <- rep(NA,nsim)
    
    #results will be stored here
    for(ii in 1:nsim) {
      
      for(rf in 1:length(random_effect)){
        simulated_data[[ii]][,random_effect[rf]] <- as.factor(simulated_data[[ii]][,random_effect[rf]])
      }
      
      
      f <- formula(paste0("g.values~", paste(fixed_effect, collapse = "+"), "+",  
                          paste("(1|",random_effect,")", collapse = "+", sep = "")))
      
      #comparing models
      mod0 <- glmmTMB(f,data=simulated_data[[ii]],family=Gamma(link = "log"))
      
      #extracting coefficients and their p-values 
      coef.mod0 <- coef(summary(mod0))$cond
      
      # get pvalue
      # pvalyr0[ii] <- 
      ## this is only writted for one fixed effect!!!!!
      pvalyr0[[ii]] <- coef.mod0[rownames(coef.mod0)=="year",colnames(coef.mod0)=="Pr(>|z|)"]
      
      # lapply(1:length(fixed_effect), function(fe){
      
      # coef.mod0[rownames(coef.mod0)==fixed_effect[fe],colnames(coef.mod0)=="Pr(>|z|)"]
      #checking if the estimated Year effect is in the same direction (positive/negative) 
      #as the simulated one
      
      # })
      
      sign.ts[[ii]] <- sign(effect.size)==sign(coef.mod0[rownames(coef.mod0)=="year",
                                                         colnames(coef.mod0)=="Estimate"])
      # lapply(1:length(fixed_effect), function(fe) {
      
      # sign(effect.size)==sign(coef.mod0[rownames(coef.mod0)==fixed_effect[fe],
      #                                   colnames(coef.mod0)=="Estimate"])
      
      # })
      
      
      
      
      # #estimated power using p-values
      # need to write this for multiple fixed effects
      # lapply(1:length(fixed_effect), function(fe) {
      # 
      #   length(which(pvalyr0[[fe]]<0.05 & !is.na(pvalyr0[[fe]]) & sign.ts[[fe]]))#*100/length(sort(pvalyr0[[fe]]))
      # 
      # })
      
      # fpower0 <- length(which(pval0<0.05 & !is.na(pval0) & sign.ts))*100/length(sort(pval0))
      
      
      
      # pvalmnth0[ii] <- coef.mod0[rownames(coef.mod0)=="month",colnames(coef.mod0)=="Pr(>|z|)"]
      # #checking if the estimated Year effect is in the same direction (positive/negative) 
      # #as the simulated one
      # ## I don't think this is necessary for month as we're not interested in detecting
      # ## monthly trends (haven't simulated any), only yearly.
      # sign.ts[ii] <- sign(effect.size)==sign(coef.mod0[rownames(coef.mod0)=="month",
      #                                                  colnames(coef.mod0)=="Estimate"])
      
    }
    
    fpower0 <- length(which(pvalyr0<0.05 & !is.na(pvalyr0) & sign.ts))*100/length(sort(pvalyr0))
    
    return(fpower0)
    
  }
  
  
  
  
  fixed_effect = model_pars[-which(model_pars %in% random_effect)]
  
  message("! Starting modelling")
  
  fpower0 <- run_power(simulated_data = comb_dat, 
                       nsim = nsim, 
                       # model_pars = model_pars, 
                       random_effect = random_effect, 
                       fixed_effect = fixed_effect, 
                       effect.size = effect.size)
  
  message("! finished modelling")
  
  outs <- data.frame(response_var, nsim, nosite.yr, noyear, effect.size, 
                     days, samfreq, fpower0)
  
  
  if(!is.null(save_loc)){
    
    dir.create(save_loc, recursive = TRUE)
    write.csv(outs, 
              file = paste0(save_loc, "/", 
                            paste(response_var, nsim, nosite.yr, noyear, effect.size, 
                                  days, samfreq, collapse = "_"), ".csv"))
    
  }
  
  return(outs)
  
}


## run function:

outs <- water_power_analysis(
    dataset_dip = read.csv("data/processed/dip_data_mn_5yrperc_change.csv"),
    dataset_telem = read.csv("data/processed/site_data_mn_5yrperc_change.csv"),
    sample_column = "sampled_20",
    model_pars = c("month", "year", "sampling_point"),
    random_effect = c("month", "sampling_point"),
    days = 12,# number of sampling occasions per year, 1 if annual, 12 if monthly
    response_var = "water_level",
    effect.size = 0.01,
    nsim = 100,
    samfreq = 1,
    nosite.yr = 100, # number of sites sampled per year across all datasets
    noyear = 5,
    prop_cont = 0.2,
    save_loc = NULL)


# data1$site <- as.factor(data1$site)
# #comparing models
# mod0 <- glmmTMB(g.values~month+year+(1|sampling_point),data=data1,family=Gamma(link = "log"))
# 
# #extracting coefficients and their p-values 
# coef.mod0 <- coef(summary(mod0))$cond
# pvalyr0[ii] <- coef.mod0[rownames(coef.mod0)=="year",colnames(coef.mod0)=="Pr(>|z|)"]
# pvalmnth0[ii] <- coef.mod0[rownames(coef.mod0)=="month",colnames(coef.mod0)=="Pr(>|z|)"]
# 
# #checking if the estimated Year effect is in the same direction (positive/negative) 
# #as the simulated one
# sign.ts[ii] <- sign(tslope)==sign(coef.mod0[rownames(coef.mod0)=="year",
#                                             colnames(coef.mod0)=="Estimate"])
# 
# 
# rm(mod0)#to ensure that convergence issues do not result in the same results being repeated
# # rm(mod_comp0)




# message("! finished simulations")
# 
# #estimated power using p-values
# fpower0 <- length(which(pval0<0.05 & !is.na(pval0) & sign.ts))*100/length(sort(pval0))
# 
# outs <- data.frame(response_var, nsim, nosite.yr, noyear, effect.size, 
#                    days, samfreq, fpower0)









