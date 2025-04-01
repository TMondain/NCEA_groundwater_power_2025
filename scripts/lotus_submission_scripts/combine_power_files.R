
# Script to combine the power output files

response_type =  basename(
  list.dirs("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/", 
            recursive = FALSE)
)

for(i in response_type){
  print(i)
  pwr<-do.call(rbind, lapply(list.files(paste0("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/", i),
                                        full.names=TRUE), read.csv))
  
  pwr$data_type <- i
  
  dir.create(paste0("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/outputs/power_datasets/"), recursive = TRUE)
  
  write.csv(pwr, file = paste0("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/outputs/power_datasets/", i, "_combined_power_outputs.csv"))
}