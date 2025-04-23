
#' Run a Power Analysis Using Simulated Data from Multiple Datasets
#'
#' Simulates ecological datasets based on user-defined parameters and performs a power analysis using GLMMs. Returns the proportion of significant results for each fixed effect.
#'
#' @param datasets A character string (semicolon-separated file paths), list of data frames, or a single data frame. These datasets provide the basis for parameter extraction and simulation.
#' @param sample_column Character. Name of a column used to subset datasets before simulation (optional; default is `NULL`).
#' @param site_column Character. Name of the column identifying unique sites.
#' @param model_pars Character vector or semicolon-separated string. Variables to include as predictors in the model.
#' @param random_effect Character vector or semicolon-separated string. Subset of `model_pars` that are to be treated as random effects.
#' @param yearly_samfreq Integer. Number of sampling occasions per year (e.g., 1 for annual, 12 for monthly; no default).
#' @param yearly_samfreq_column Character. Name of the column representing sampling frequency (default is `"days"`).
#' @param response_var Character. Name of the response variable to be modeled.
#' @param effect.size Numeric vector. True effect sizes for fixed effects (must match length of `fixed_effect` !! not yet coded !!; no default).
#' @param nsim Integer. Number of simulations to perform.
#' @param samfreq Integer. Number of total samples per site over the full study period.
#' @param nosite.yr Integer. Total number of unique sites sampled per year across all datasets.
#' @param noyear Integer. Number of years in the study period.
#' @param data_proportions Numeric vector or semicolon-separated string. Proportions of total sites assigned to each dataset (optional; default is `NULL`).
#' @param save_loc Character. Directory path to save the output CSV file (optional; default is `NULL`).
#'
#' @return A data frame summarizing power analysis results, including input parameters and the proportion of simulations where fixed effects were significant and correctly signed.
#'
#' @details
#' This function reads or accepts datasets, extracts model parameters using `get_model_pars`, builds a simulation template using `create_sim_df`, simulates datasets using `simulate_data`, and then runs a power analysis using `run_power`.
#'
#' The analysis assumes a Gamma-distributed response and uses the `glmmTMB` package for model fitting.
#'
#' @import readxl extraDistr EnvStats glmmTMB crayon gdata truncnorm ggplot2 truncdist
#'
#' @export

run_power_analysis <- function(
    datasets, # one or more datasets
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
  
  
  require(extraDistr)
  require(EnvStats)
  require(glmmTMB)
  require(crayon)
  require(gdata)
  require(truncnorm)
  require(ggplot2)
  require(truncdist)
  
  
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
  
  
  if(any(!random_effect %in% model_pars))
    stop("! All 'random_effect' must be found in 'model_pars'")
  
  
  #### sample ------------------------------------------------------------------
  
  # loop through datasets and sample
  if(!is.null(sample_column)) {
    dats_list <- lapply(dats_list, function(x) {
      x[which(x[,sample_column] == 1),]
    })
  }
  
  # check to make sure there are non-NA and non-inf values
  if(any(unlist(lapply(1:length(dats_list), function(x) 
    !any(is.finite(na.omit(dats_list[[x]][,response_var])))))))
    stop("! There are no useable (non NA and non-inf) values in dataset number ", which(unlist(lapply(1:length(dats_list), function(x) 
      !any(is.finite(na.omit(dats_list[[x]][,response_var])))))),
      "\nCheck that the sample column '", sample_column, "' has useable data associated with it")
  
  
  #### initial model -----------------------------------------------------------
  model_para_vals <- lapply(dats_list, function(x)
    get_model_pars(dat = x, response = response_var, model_covs = model_pars))
  
  # check that all worked
  isna <- lapply(model_para_vals, is.na)
  
  if(any(unlist(lapply(isna, any))))
    stop("! The model for dataset ", which(unlist(lapply(isna, any))), " has no value estimated for one of the parameters (convergence issue?). 
         \n Stopping because cannot simulate based on model that hasn't estimated all parameters.")
  
  
  # create the template dataframe ------------------------------------------------
  expanded_data <- create_sim_df(samfreq = samfreq, 
                                 nosite.yr = nosite.yr, 
                                 site_column = site_column,
                                 noyear = noyear, 
                                 yearly_samfreq = yearly_samfreq,
                                 yearly_samfreq_column = yearly_samfreq_column)
  
  # View the first few rows of the expanded data
  head(expanded_data)
  length(unique(expanded_data$site))
  
  
  
  #### alter number of sites from each dataset -----------------------------------
  head(expanded_data)
  
  if(!is.null(data_proportions)){
    message("! altering dataset proportions")
    
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

