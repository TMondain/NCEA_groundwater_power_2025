
########################################################################
# Libraries
########################################################################
library(readxl)
library(extraDistr)
library(EnvStats)
library(glmmTMB)
library(crayon)
library(gdata)
library(truncnorm)
library(ggplot2)
library(truncdist)
########################################################################
# Reading inputs
########################################################################


dat <- read_xlsx("data/raw/water_quality_data_cleaned.xlsx")
dat$gw0117_value <- as.numeric(dat$gw0117_value)

scenarios <- read.csv("data/scenarios/N_scenarios.csv")


# to store results
fpower0 <- fpower_LR0 <- fpower_LR0_anova <-rep(NA,length(scenarios[,1]))

# number of simulations
nsim = 20

# run the model
modparam <- glmmTMB(gw0117_value~1+(1|year)+(1|sampling_point),data=dat,family=Gamma(link = "log"))


########################################################################
#loop over the number of scenarios
########################################################################
#for (ss in 1:length(scenarios[,1])){
for (ss in 1:12){
  
  print(ss)
  
  #number of years
  noyear <- scenarios$noyear[ss]
  
  #number of sites per year
  nosite.yr <- scenarios$nosite[ss] 
  
  #frequency of sampling the same unit across years
  samfreq <- scenarios$freq[ss]
  
  effect.size <- scenarios$eff[ss]
  
  ########################################################################
  # Starting simulations for this scenario
  ########################################################################
  nsim = nsim # select a number of simulations
  
  #results will be stored here
  pval0 <- LR0 <- LR0_anova <-sign.ts <- rep(NA,nsim)
  #results will be stored here
  
  #effect.size 
  #note: here I tested whether there is a difference when simulating positive or negative effect sizes,  
  #but there is no difference in the results. So, only simulating the negative effect
  #effect.size is always negative
  tslope <- effect.size
  #tslope <- -(effect.size)#positive
  
  #loop over simulations
  for (ii in 1:nsim){
    
    ########################################################################
    # Data structure 
    ########################################################################
    
    if (is.numeric(samfreq)) {
      if (noyear >= samfreq) nosite = nosite.yr * samfreq else {
        samfreq <- noyear
        nosite = nosite.yr * samfreq 
      } 
      thin.id <- paste(rep(1:nosite,floor(noyear/samfreq)),
                       rep(1:noyear,each=floor(nosite/samfreq)),sep="_")
      
      data.temp <- expand.grid(site=1:nosite, year=1:noyear)
      data.temp$site.yr <- paste(data.temp$site, data.temp$year, sep="_")
      data0 <- data.temp[is.element(data.temp$site.yr,thin.id),]
    }
    
    
    expanded_data <- data.frame()
    #loop over each site-year combination
    for (zz in 1:nrow(data0)) {
      site <- data0$site[zz]
      year <- data0$year[zz]
      site_yr <- data0$site.yr[zz]
      
      #the number of days for this site-year
      days <- scenarios$days[ss]
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
    
    # View the first few rows of the expanded data
    head(expanded_data)
    table(expanded_data[,c("site","year")])
    length(unique(data0$site))
    
    #number of unique sites (this number depends on sampling frequency 
    #(annual freq. will have the smallest number of unique sites))
    nosite <- length(unique(expanded_data$site))
    
    ########################################################################
    # generating parameters
    ########################################################################
    intval = data.frame(summary(modparam)$coefficients$cond)$Estimate #0.878
    intsd = data.frame(summary(modparam)$coefficients$cond)$Std..Error #0.026
    stsd = attr(summary(modparam)$varcor$cond$sampling_point, "stddev", exact = TRUE) #1.58
    yrsd = attr(summary(modparam)$varcor$cond$year, "stddev", exact = TRUE) #0.031
    
    # summary(modparam)$coefficients$cond
    # summary(modparam)$varcor$cond$year
    # attr(summary(modparam)$varcor$cond$sampling_point, "stddev", exact = TRUE)
    
    sim.int <- rtnorm(1, mean=intval, 
                      sd=intsd)
    
    #adding site variability
    int.df <- data.frame(int = sim.int + rtnorm(nosite, mean=0, 
                                                sd=stsd), 
                         site = unique(expanded_data$site))
    
    expanded_data$int <- int.df$int[match(expanded_data$site, int.df$site)]
    
    #adding year variability
    year_levels <- unique(expanded_data$year) 
    year_effects <- rnorm(length(year_levels), 
                          mean = 0, 
                          sd = yrsd)
    
    year_df <- data.frame(year = year_levels, year_effect = year_effects)
    expanded_data <- merge(expanded_data, year_df, by = "year", all.x = TRUE)
    expanded_data$int <- expanded_data$int + expanded_data$year_effect
    
    
    ########################################################################
    # sim values
    ########################################################################
    
    #sigma2
    g.s2 <- summary(modparam)$sigma^2 #0.121
    
    #expected mean (mu)
    expanded_data$mu <- exp(expanded_data$int + tslope * expanded_data$year)
    
    #Gamma parameters
    g.shape <- 1/g.s2
    g.scale <- expanded_data$mu/g.shape
    
    # Simulating values
    expanded_data$g.values <- sapply(1:nrow(expanded_data), function(i) {
      rtrunc(1, spec = "gamma", a = 1e-6, shape = g.shape, scale = g.scale[i])
    })
    
    data1 <- expanded_data[,c("g.values","site","year")]
    
    ########################################################################
    #Power analysis
    ########################################################################
    data1$site <- as.factor(data1$site)
    #comparing models
    mod0 <- glmmTMB(g.values~year+(1|site),data=data1,family=Gamma(link = "log"))
    
    #extracting coefficients and their p-values 
    coef.mod0 <- coef(summary(mod0))$cond
    pval0[ii] <- coef.mod0[rownames(coef.mod0)=="year",colnames(coef.mod0)=="Pr(>|z|)"]
    
    #checking if the estimated Year effect is in the same direction (positive/negative) 
    #as the simulated one
    sign.ts[ii] <- sign(tslope)==sign(coef.mod0[rownames(coef.mod0)=="year",
                                                colnames(coef.mod0)=="Estimate"])
    
    
    rm(mod0)#to ensure that convergence issues do not result in the same results being repeated
    # rm(mod_comp0)
    
  }
  #estimated power using p-values
  fpower0[ss] <- length(which(pval0<0.05 & !is.na(pval0) & sign.ts))*100/length(sort(pval0))
  
  
}

save.image("C://RTemp//GWDTE_power_N.RData")




