rm(list = ls())

## power plots

library(tidyverse)


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
           aes(noyear, fpower0, colour = factor(prop_cont))) +#, linetype = factor(days))) +
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
           aes(nosite.yr, fpower0, colour = factor(prop_cont))) + #, linetype =  factor(days))) +
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

