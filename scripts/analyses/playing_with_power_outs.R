

## power plots

library(tidyverse)


### need to check which scenarios didn't RUN!!

pwr <- read.csv("outputs/power_datasets/combined_power_outputs.csv")
head(pwr, 15)

gw1 <- pwr[pwr$response_var == "gw0117_value",]
# View(gw1)


head(gw1)
ggplot(gw1, aes(nosite.yr, fpower0, colour = factor(days), linetype = factor(samfreq))) +
  geom_line() +
  geom_hline(yintercept = 70, linetype = "dashed") +
  geom_hline(yintercept = 80, linetype = "dashed") +
  facet_wrap(effect.size~noyear)


ggplot(gw1, aes(noyear, fpower0, colour = factor(days), linetype = factor(samfreq))) +
  geom_line() +
  geom_hline(yintercept = 70, linetype = "dashed") +
  geom_hline(yintercept = 80, linetype = "dashed") +
  facet_wrap(effect.size~nosite.yr)

dir.create("outputs/plots/")

rvar <- unique(pwr$response_var)

for(i in 1:length(rvar)) {
  
  pl_nyr_x <- 
    ggplot(pwr[pwr$response_var == rvar[i],], 
           aes(noyear, fpower0, colour = factor(samfreq), linetype = factor(days))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~nosite.yr, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i]), "number years x-axis") +
    scale_colour_manual(values =  viridis::viridis(5)[c(1:4)])  
  
  
  pl_nsite_x <- 
    ggplot(pwr[pwr$response_var == rvar[i],], 
           aes(nosite.yr, fpower0, colour = factor(samfreq), linetype =  factor(days))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 70, linetype = "dashed") +
    geom_hline(yintercept = 80, linetype = "dashed") +
    facet_grid(effect.size~noyear, 
               labeller = "label_both") +
    ggtitle(paste(rvar[i], "number site per year x-axis")) +
    scale_colour_manual(values =  viridis::viridis(5)[c(1:4)])  
  
  
  ggsave(pl_nyr_x, 
         filename = paste0("outputs/plots/", rvar[i], "number_years_x-axis.png"),
         width = 14, height = 6)
  
  ggsave(pl_nsite_x, 
         filename = paste0("outputs/plots/", rvar[i], "number_site_per_year_x-axis.png"),
         width = 8, height = 6)
  
}




print("B:LLAAAAAAAAAAAAAAAAHHHH")


unique(pwr$nosite.yr)
unique(pwr$noyear)
unique(pwr$effect.size)
unique(pwr$days)
unique(pwr$samfreq)

unique(paste(pwr$noyear, pwr$effect.size, sep = "_"))


ggplot(pwr, aes(factor(nosite.yr), factor(noyear), fill = fpower0)) +
  geom_tile() +
  facet_wrap(effect.size~samfreq)


x <- c("a","b","c")
y <- c(1,2,3)


paste(x, y, sep = "_")
