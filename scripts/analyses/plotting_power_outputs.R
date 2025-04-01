rm(list = ls())

## power plots

library(tidyverse)


#### water quality -------------------------------------------------------------

# red combined power file
pwr <- read.csv("outputs/power_datasets/water_quality_combined_power_outputs.csv")
head(pwr, 15)

unique(pwr$response_var)

pwr <- pwr %>%
  mutate(resp_var_clean = gsub("_", " ", response_var))


dir.create("outputs/plots/water_quality")

rvar <- unique(pwr$resp_var_clean)

for(i in 1:length(rvar)) {
  
  pl_nyr_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(noyear, fpower0, colour = factor(samfreq), linetype = factor(days))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~nosite.yr, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of years") +
    ylab("Power (% significant results)") +
    scale_colour_manual(values =  viridis::viridis(5)[c(1:4)],
                        name = "Repeat sampling cycle") +
    scale_linetype(name = "Within-year\nrepeat sampling")
  
  
  pl_nsite_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(nosite.yr, fpower0, colour = factor(samfreq), linetype =  factor(days))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~noyear, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of sites per year") +
    ylab("Power (% significant results)") +
    scale_colour_manual(values =  viridis::viridis(5)[c(1:4)],
                        name = "Repeat sampling cycle") +
    scale_linetype(name = "Within-year\nrepeat sampling")  
  
  
  ggsave(pl_nyr_x, 
         filename = paste0("outputs/plots/water_quality/", rvar[i], "number_years_x-axis.png"),
         width = 14, height = 6)
  
  ggsave(pl_nsite_x, 
         filename = paste0("outputs/plots/water_quality/", rvar[i], "number_site_per_year_x-axis.png"),
         width = 8, height = 6)
  
}



#### Water level data ----------------------------------------------------------

# red combined power file
pwr <- read.csv("outputs/power_datasets/water_level_combined_power_outputs.csv")
head(pwr, 15)

unique(pwr$response_var)

pwr <- pwr %>%
  mutate(resp_var_clean = 
           ifelse(response_var == "cent_5_value", "5th centile",
                  ifelse(response_var == "cent_95_value", "95th centile",
                         ifelse(response_var == "water_level", "Mean water level", 
                                ifelse(response_var == "perc_chg_ind", "Change relative to 5yr baseline (%)","")))))


dir.create("outputs/plots/")

rvar <- unique(pwr$resp_var_clean)

for(i in 1:length(rvar)) {
  
  pl_nyr_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(noyear, fpower0, colour = factor(prop_cont))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~nosite.yr, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of years") +
    ylab("Power (% significant results)") +
    scale_colour_manual(values =  viridis::viridis(5)[c(1:4)],
                        name = "Proportion continuous\nmonitoring")  
  
  
  pl_nsite_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(nosite.yr, fpower0, colour = factor(prop_cont))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~noyear, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of sites per year") +
    ylab("Power (% significant results)") +
    scale_colour_manual(values =  viridis::viridis(5)[c(1:4)],
                        name = "Proportion continuous\nmonitoring")  
  
  
  ggsave(pl_nyr_x, 
         filename = paste0("outputs/plots/water_level/", rvar[i], "number_years_x-axis.png"),
         width = 14, height = 6)
  
  ggsave(pl_nsite_x, 
         filename = paste0("outputs/plots/water_level/", rvar[i], "number_site_per_year_x-axis.png"),
         width = 8, height = 6)
  
}



#### water temperature ---------------------------------------------------------

# read combined power file
pwr <- read.csv("outputs/power_datasets/water_temp_combined_power_outputs.csv")
head(pwr, 15)

unique(pwr$response_var)

pwr <- pwr %>%
  mutate(resp_var_clean = gsub("_", " ", response_var))


dir.create("outputs/plots/water_temp")

rvar <- unique(pwr$resp_var_clean)

for(i in 1:length(rvar)) {
  
  pl_nyr_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(noyear, fpower0, colour = factor(effect.size))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(~nosite.yr,
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of years") +
    ylab("Power (% significant results)") +
    scale_colour_manual(values =  viridis::viridis(6)[c(1,3,4)],
                        name = "Effect size") 

  
  ggsave(pl_nyr_x, 
         filename = paste0("outputs/plots/water_temp/", rvar[i], "number_years_x-axis.png"),
         width = 14, height = 6)
  
}





#### water quality regional ----------------------------------------------------

# red combined power file
pwr <- read.csv("outputs/power_datasets/water_qual_reg_combined_power_outputs.csv")
head(pwr, 15)

unique(pwr$response_var)

pwr <- pwr %>%
  mutate(resp_var_clean = gsub("_", " ", response_var))


dir.create("outputs/plots/water_quality_reg")

rvar <- unique(pwr$resp_var_clean)

for(i in 1:length(rvar)) {
  
  pl_nyr_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(noyear, fpower0, colour = factor(nosite.yr), linetype = factor(samfreq))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~sample_column, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of years") +
    ylab("Power (% significant results)") +
    scale_linetype(name = "Repeat sampling cycle") +
    scale_colour_manual(values =  viridis::viridis(10),
                        name = "Number sites per year")
  

  pl_nsite_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
          aes(nosite.yr, fpower0, colour = factor(noyear), linetype = factor(samfreq))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~sample_column, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of sites per year") +
    ylab("Power (% significant results)") +
    scale_linetype(name = "Repeat sampling cycle") +
    scale_colour_manual(values =  viridis::viridis(4)[c(1:3)],
                        name = "Number of years")
  
  
  ggsave(pl_nyr_x, 
         filename = paste0("outputs/plots/water_quality_reg/", rvar[i], "number_years_x-axis.png"),
         width = 14, height = 6)
  
  ggsave(pl_nsite_x, 
         filename = paste0("outputs/plots/water_quality_reg/", rvar[i], "number_site_per_year_x-axis.png"),
         width = 14, height = 6)
  
}



#### Water level regional data -------------------------------------------------

# red combined power file
pwr <- read.csv("outputs/power_datasets/water_level_regional_combined_power_outputs.csv")
head(pwr, 15)

unique(pwr$response_var)

pwr <- pwr %>%
  mutate(resp_var_clean = 
           ifelse(response_var == "cent_5_value", "5th centile",
                  ifelse(response_var == "cent_95_value", "95th centile",
                         ifelse(response_var == "water_level", "Mean water level", 
                                ifelse(response_var == "perc_chg_ind", "Change relative to 5yr baseline (%)","")))))


dir.create("outputs/plots/")

rvar <- unique(pwr$resp_var_clean)

for(i in 1:length(rvar)) {
  
  pl_nyr_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(noyear, year_fpower, colour = factor(nosite.yr))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~sample_column, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of years") +
    ylab("Power (% significant results)") +
    scale_colour_manual(values =  viridis::viridis(6)[c(1:5)],
                        name = "Number of sites\nper year")  
  
  
  pl_nsite_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(nosite.yr, year_fpower, colour = factor(noyear))) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~sample_column, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of sites per year") +
    ylab("Power (% significant results)") +
    scale_colour_manual(values =  viridis::viridis(5)[c(1:4)],
                        name = "Number of years")  
  
  
  ggsave(pl_nyr_x, 
         filename = paste0("outputs/plots/water_level_regional/", rvar[i], "number_years_x-axis.png"),
         width = 14, height = 6)
  
  ggsave(pl_nsite_x, 
         filename = paste0("outputs/plots/water_level_regional/", rvar[i], "number_site_per_year_x-axis.png"),
         width = 14, height = 6)
  
}




#### water quality regional NEW ------------------------------------------------

# red combined power file
pwr <- read.csv("outputs/power_datasets/water_qual_reg_NEW_combined_power_outputs.csv")
head(pwr, 15)

unique(pwr$response_var)

pwr <- pwr %>%
  mutate(resp_var_clean = gsub("_", " ", response_var))


dir.create("outputs/plots/water_quality_reg")

rvar <- unique(pwr$resp_var_clean)

for(i in 1:length(rvar)) {
  
  pl_nyr_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(noyear, year_fpower, colour = factor(nosite.yr))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~sample_column, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of years") +
    ylab("Power (% significant results)") +
    scale_linetype(name = "Repeat sampling cycle") +
    scale_colour_manual(values =  viridis::viridis(4)[1:3],
                        name = "Number sites per year")
  
  
  pl_nsite_x <- 
    ggplot(pwr[pwr$resp_var_clean == rvar[i],], 
           aes(nosite.yr, year_fpower, colour = factor(noyear))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~sample_column, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i])) +
    xlab("Number of sites per year") +
    ylab("Power (% significant results)") +
    scale_linetype(name = "Repeat sampling cycle") +
    scale_colour_manual(values =  viridis::viridis(4)[c(1:3)],
                        name = "Number of years")
  
  
  ggsave(pl_nyr_x, 
         filename = paste0("outputs/plots/water_quality_reg_NEW/", rvar[i], "number_years_x-axis.png"),
         width = 14, height = 6)
  
  ggsave(pl_nsite_x, 
         filename = paste0("outputs/plots/water_quality_reg_NEW/", rvar[i], "number_site_per_year_x-axis.png"),
         width = 14, height = 6)
  
}




