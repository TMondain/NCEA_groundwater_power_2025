
# Script to combine the power output files


pwr<-do.call(rbind, lapply(list.files("data/simulations/power/water_level/",
                                      full.names=TRUE), read.csv))

dir.create("outputs/power_datasets/", recursive = TRUE)

write.csv(pwr, file = "outputs/power_datasets/combined_power_outputs.csv")
