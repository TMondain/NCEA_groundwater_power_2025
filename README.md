# NCEA_groundwater_power_2025

## Introduce repository

The scripts and functions in this repository were created to run power analyses for groundwater monitoring using simulated data. The scripts in this repository consist of a single function which then calls multiple others to do conduct a power analysis. The main function `run_power_analysis` works by modelling current data, extracting the model parameters, simulating data according to an effect size and then modelling those simulated data. From this, we extract the number of times (across all simulations) that the model term of interest was significant and use this as an estimate of power to detect that effect size. The modelling is done using a _gamma_ distribution in the `glmmTMB` package. The function has a number of different options, allowing (some) flexibility in modelling scenarios.

There was initial interest in understanding the relative benefit of telemetered versus dip site data. This function can take multiple datasets as inputs, and the user can specify the proportion of each dataset that is used for modelling. The effect of using different proportions of different data types on the power to detect changes can therefore be assessed.

The function also has the ability to look at trends in two different (continuous) fixed effects. This means that the user can alter the changes over time of two different fixed effects, one for each of the fixed effects included.

To start, source the scripts. See the package walkthrough script for more information.

```{r source_scripts}

source("scripts/functions/run_power_analysis.R")
source("scripts/functions/power_subfunctions.R")

```

### Repository structure

All of the scripts necessary to run the code are contained in `scripts/`. For the project, all of the scenarios were run on the JASMIN HPC. This was done using the package `rslurm`, and the scripts used to submit these scripts are located in `scripts/lotus_submission_scripts/`. These scripts were run on JASMIN.

Data should be put in subfolders in a `data/` folder. Raw data in `scripts/raw/` and scenarios in `scripts/scenarios/`.