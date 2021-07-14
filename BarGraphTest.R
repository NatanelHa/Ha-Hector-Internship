##Importing Libraries 
library(hector)
library(ggplot2)
library(dplyr)

#Creating Multiple Cores
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core45 <- newcore(rcp45, suppresslogging  = TRUE)

rcp26 <- system.file("input", "hector_rcp26.ini", package = "hector")
core26 <- newcore(rcp26, suppresslogging  = TRUE)

rcp85 <- system.file("input", "hector_rcp85.ini", package = "hector")
core85 <- newcore(rcp85, suppresslogging  = TRUE)

#Run Cores
run(core26)
run(core45)
run(core85)

#Retrieve Results
result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), VEG_C())
results26 <- fetchvars(core26, 2000:2200, result_vars, scenario = "RCP 2.6")
results45 <- fetchvars(core45, 2000:2200, result_vars, scenario = "RCP 4.5")
results85 <- fetchvars(core85, 2000:2200, result_vars, scenario = "RCP 8.5")

results <- rbind(results26, results45, results85)


results %>%
  filter(year %% 100 == 0) ->
  results

ggplot(results)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Soil", "Vegetation"))+
  facet_wrap(~scenario)+
  guides(fill = guide_legend(title = "Carbon Pools"))
  

#Shutdown Core
shutdown(core)
