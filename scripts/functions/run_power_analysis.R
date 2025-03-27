
require(readxl)
require(extraDistr)
require(EnvStats)
require(glmmTMB)
require(crayon)
require(gdata)
require(truncnorm)
require(ggplot2)
require(truncdist)


run_power_analysis <- function(
    datasets, # one or more datasets
    # dataset_dip,
    # dataset_telem,
    sample_column = NULL,
    site_column,
    model_pars,
    random_effect,
    yearly_samfreq,# number of sampling occasions per year, 1 if annual, 12 if monthly
    yearly_samfreq_column = "days",
    response_var,
    effect.size,
    nsim,
    samfreq,
    nosite.yr, # number of sites sampled per year across all datasets
    noyear,
    data_proportions = NULL,
    save_loc) {
  
  if(any(!random_effect %in% model_pars))
    stop("! All 'random_effect' must be found in 'model_pars'")
  
  
  if(inherits(datasets, "character")) {
    message("! reading datasets")
    dats_list <- lapply(strsplit(datasets, ";")[[1]], read.csv)
  } else if(inherits(datasets, "list")) {
    dats_list <- datasets
  } else if(inherits(datasets, "data.frame")) {
    dats_list <- list(datasets)
  } else {
    stop("! 'datasets' must be of class 'character', 'list' or 'data.frame'")
  }
  
  
  # sort out parameters
  if(any(grepl(";", model_pars)))
    model_pars <- strsplit(model_pars, ";")[[1]]
  
  if(any(grepl(";", random_effect)))
    random_effect <- strsplit(random_effect, ";")[[1]]
  
  if(any(grepl(";", data_proportions)))
    data_proportions <- as.numeric(strsplit(data_proportions, ";")[[1]])
  
  # convert these to as.character
  if(!is.null(sample_column))
    sample_column <- as.character(sample_column)
  
  if(!is.null(save_loc))
    save_loc <- as.character(save_loc)
  
  response_var <- as.character(response_var)
  
  
  #### sample ------------------------------------------------------------------
  
  # loop through datasets and sample
  if(!is.null(sample_column)) {
    dats_list <- lapply(dats_list, function(x) {
      x[x[,sample_column] == 1,]
    })
  }
  
  
  #### initial model -----------------------------------------------------------
  model_para_vals <- lapply(dats_list, function(x)
    get_model_pars(dat = x, response = response_var, model_covs = model_pars))
  
  
  # create the template dataframe ------------------------------------------------
  expanded_data <- create_sim_df(samfreq = samfreq, 
                                 nosite.yr = nosite.yr, 
                                 site_column = site_column,
                                 noyear = noyear, 
                                 yearly_samfreq = yearly_samfreq,
                                 yearly_samfreq_column = yearly_samfreq_column)
  
  
  # if("month" %in% model_pars)
  #   # add month
  #   expanded_data$month <- expanded_data$day 
  # 
  # if("sampling_point" %in% model_pars)
  #   expanded_data$sampling_point <- expanded_data$site
  
  # View the first few rows of the expanded data
  head(expanded_data)
  length(unique(expanded_data$site))
  
  
  
  #### alter number of sites from each dataset -----------------------------------
  head(expanded_data)
  
  if(!is.null(data_proportions)){
    message("! altering dataset proportions")
    
    # ## could write this to work for any number of datasets
    # if(length(dats_list)>1) {
    #   
    #   # calculate number of sites to sample
    #   nsite_dat2 <- round(nosite.yr*data_proportions)
    #   nsite_dat1 <- nosite.yr-nsite_dat2
    #   
    #   nsites <- c(nsite_dat1, nsite_dat2)
    #   
    #   expanded_datlist <- lapply(nsites, function(x){
    #     
    #     smpsits <- sample(unique(expanded_data[,site_column]), size = x)
    #     expanded_data[expanded_data[,site_column] %in% smpsits,]
    #     
    #   })
    # }
    
    expanded_datlist <- alter_dat_proportions(dats_list = dats_list, 
                                              data_proportions = data_proportions,
                                              template_data = expanded_data,
                                              site_column = site_column,
                                              nosite.yr = nosite.yr)
    
    # I think this worked
    lapply(expanded_datlist, dim)
    
    # remove datasets with no data
    for(x in 1:length(expanded_datlist)) {
      
      if(dim(expanded_datlist[[x]])[1] == 0) {
        model_para_vals[[x]] <- NULL
        expanded_datlist[[x]] <- NULL
      } 
      
    }
  } else {
    
    expanded_datlist <- lapply(1:length(dats_list), function(x) expanded_data)
    
  }
  
  #### simulation ----------------------------------------------------------------
  
  message("! Starting data simulations")
  
  
  simdat_list <- lapply(1:length(model_para_vals), function(i) 
    simdat <- simulate_data(template_dat = expanded_datlist[[i]],
                            model_params = model_para_vals[[i]],
                            tslope = effect.size,
                            nsim = nsim,
                            model_pars = model_pars)
  )
  
  message("! finished data simulations")
  
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
  
  fixed_effect = model_pars[-which(model_pars %in% random_effect)]
  
  message("! Starting power analysis")
  
  fpower0 <- run_power(simulated_data = comb_dat, 
                       nsim = nsim, 
                       random_effect = random_effect, 
                       fixed_effect = fixed_effect, 
                       effect.size = effect.size)
  
  message("! finished power analysis")
  
  outs <- data.frame(response_var,
                     sample_column = ifelse(!is.null(sample_column), sample_column, NA),
                     model_pars = paste(model_pars, collapse = "_"),
                     random_effect = paste(model_pars, collapse = "_"),
                     nsim, nosite.yr, noyear, effect.size, 
                     yearly_samfreq, samfreq, data_proportions = paste(data_proportions, collapse = "_"),  
                     fpower0)
  
  
  if(!is.null(save_loc)){
    
    message("! saving")  
    dir.create(save_loc, recursive = TRUE)
    write.csv(outs, 
              file = paste0(save_loc, "/", 
                            paste(response_var, paste(fixed_effect, collapse = "_"),
                                  ifelse(!is.null(sample_column), sample_column, ""), 
                                  nsim, nosite.yr, noyear, effect.size, 
                                  yearly_samfreq, samfreq, paste(data_proportions, collapse = "_"), sep = "_"), ".csv"))
    
  }
  
  return(outs)
  
}

