# NCEA_groundwater_power_2025




### Introduce repository

The scripts and functions in this repository were created to run power analyses for groundwater monitoring using simulated data. The scripts in this repository consist of a single function which then calls multiple others to do conduct a power analysis. The main function `run_power_analysis` works by modelling current data, extracting the model parameters, simulating data according to an effect size and then modelling those simulated data. From this, we extract the number of times (across all simulations) that the model term of interest was significant and use this as an estimate of power to detect that effect size. The modelling is done using a _gamma_ distribution in the `glmmTMB` package. The function has a number of different options, allowing (some) flexibility in modelling scenarios.

There was initial interest in understanding the relative benefit of telemetered versus dip site data. This function can take multiple datasets as inputs, and the user can specify the proportion of each dataset that is used for modelling. The effect of using different proportions of different data types on the power to detect changes can therefore be assessed.

The function also has the ability to look at trends in two different (continuous) fixed effects. This means that the user can alter the changes over time of two different fixed effects, one for each of the fixed effects included.


### running the function

```{r source_scripts}

source("scripts/functions/run_power_analysis.R")
source("scripts/functions/power_subfunctions.R")

```

The steps behind the function

1. Reads or processes the input datasets.

2. Filters rows (if sample_column is provided).

3. Extracts model parameters from real data using `get_model_pars`.

4. Creates a template data frame with simulated temporal and site structure using .

5. Splits site sampling across datasets based on data_proportions.

6. Simulates response data under the assumed model and effect sizes.

7. Fits GLMMs to each simulation and calculates how often the true fixed effects are detected.

8. Returns (and optionally saves) a summary data frame of the power analysis results.


#### Parameters explained

---
title: "Parameter Description for `run_power_analysis` Function"
output: html_document
---

### Parameter Descriptions for `run_power_analysis`

`datasets`  
Accepts:
- A semicolon-separated character string of file paths (e.g., "data1.csv;data2.csv")
- A list of data frames
- A single data frame  
These are the source datasets used to extract model parameters and generate simulations.

`sample_column`  
(Optional) A character string naming a column used to subset the datasets. Should be a a column of `c(0,1)`, where 1s are the rows to keep. For example, if only rows where `sample_column == 1` should be used, specify it here.

`site_column`  
A character string naming the column that identifies unique sampling sites.

`model_pars`  
A character vector or semicolon-separated string listing all variables (covariates) to include in the model.

`fixed_effect`  
A character vector or semicolon-separated string specifying which of the model parameters should be treated as *fixed effects*. Must be a subset of `model_pars`. One of `fixed_effect` or `random_effect` is required.

`random_effect`  
Same as above, but for *random effects*. Also must be a subset of `model_pars`.  
One of `fixed_effect` or `random_effect` is required.

`yearly_samfreq`  
`Integer` Number of times each site is sampled per year (e.g., 12 for monthly, 1 for annual).

`yearly_samfreq_column`  
`Character` Name of the column that reflects the sampling frequency (default is "days").

`response_var`  
`Character` The name of the response variable in the dataset.

`effect.size`  
A numeric vector (or semicolon-separated string) representing the true effect sizes for the fixed effects. Must match the length and order of `fixed_effect`.

`nsim`  
`Integer` Number of simulations to run.

`samfreq`  
`Integer` Total number of samples per site across the entire study period.

`nosite.yr`  
`Integer` Number of unique sites sampled each year.

`noyear`  
`Integer` Number of years in the study.

`data_proportions`  
(Optional) A numeric vector (or semicolon-separated string) indicating what proportion of sites to simulate from each input dataset. Should sum to 1 and be the same length as the number of datasets.

`save_loc`  
(Optional) `Character` Directory where the output CSV should be saved. Creates the directory recursively if the location doesn't exist. If `NULL`, the results are not saved to disk.


#### examples

one dataset

```{r}

pwr <- run_power_analysis(
  datasets = "data/processed/dip_data_mn_5yrperc_change.csv", # one or more datasets
  sample_column = "sampled_20",
  site_column = "sampling_point",
  model_pars = c("month", "year", "sampling_point"), ## change this to station_number
  random_effect = c("month", "sampling_point"), ## try with single random effect
  yearly_samfreq = 12,# number of sampling occasions per year, 1 if annual, 12 if monthly
  yearly_samfreq_column = "month",
  response_var = "water_level",
  effect.size = 0.1,
  nsim = 10,
  samfreq = 1,
  nosite.yr = 100, # number of sites sampled per year across all datasets
  noyear = 5,
  data_proportions = NULL, # try with NULL - don't think it would work
  save_loc = NULL)

```

two datasets

```{r}

pwr <- run_power_analysis(
  datasets = paste("data/processed/dip_data_mn_5yrperc_change.csv", 
                   "data/processed/site_data_mn_5yrperc_change.csv", sep = ";"), # one or more datasets
  sample_column = "sampled_20",
  site_column = "sampling_point",
  model_pars = c("month", "year", "sampling_point"), # all of the model parameters
  random_effect = c("month", "sampling_point"), # which of model_pars are random effects
  yearly_samfreq = 12,# number of sampling occasions per year, 1 if annual, 12 if monthly
  yearly_samfreq_column = "month",
  response_var = "perc_chg_ind",
  effect.size = 0.05,
  nsim = 10,
  samfreq = 1,
  nosite.yr = 100, # number of sites sampled per year across all datasets
  noyear = 5,
  data_proportions = c(0.8, 0.2), # doesn't have to add up to 1
  save_loc = NULL)

```


two fixed effects and two effect sizes


### detailed explanation

different datasets

-different proportions

different effect sizes for each fixed effect


explain how the function works
(discuss functions that could be useful? or later...?)

model - glmmtmb gamma link
-extract model params
create data template
alter proportions - outputs template for each dataset
simulate data for each of the datasets - combine
run power


parameter specifications

"rules" about fixed effects
coded sequentially i.e. increasing values
present in parameter values 







sub-functions that are useful


simulate data

