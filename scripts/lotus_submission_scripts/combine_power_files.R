
# Script to combine the power output files

response_type = "water_qual_reg"

pwr<-do.call(rbind, lapply(list.files(paste0("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/data/simulations/power/", response_type),
                                      full.names=TRUE), read.csv))

pwr$data_type <- response_type

dir.create(paste0("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/outputs/power_datasets/"), recursive = TRUE)

write.csv(pwr, file = paste0("/gws/nopw/j04/ceh_generic/thoval/ncea/groundwater_power/outputs/power_datasets/", response_type, "_combined_power_outputs.csv"))
