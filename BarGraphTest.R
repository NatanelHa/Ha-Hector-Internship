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
results26 <- fetchvars(core26, 2000:2300, result_vars, scenario = "RCP 2.6")
results45 <- fetchvars(core45, 2000:2300, result_vars, scenario = "RCP 4.5")
results85 <- fetchvars(core85, 2000:2300, result_vars, scenario = "RCP 8.5")

results <- rbind(results26, results45, results85)


#Plotting as an area curve
ggplot(results)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Soil", "Vegetation"))+
  geom_col(width = 1)+
  facet_wrap(~scenario)+
  guides(fill = guide_legend(title = "Carbon Pools"))

#Filtering for centuries
results %>%
  filter(year %% 100 == 0 & year > 2000)->
  results

#Plotting as a bar chart
ggplot(results)+
  aes(x = scenario, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Soil", "Vegetation"))+
  facet_wrap(~year)+
  guides(fill = guide_legend(title = "Carbon Pools"))

#Retrieve Results
result_vars <- c(GLOBAL_TEMP(), OCEAN_AIR_TEMP(), OCEAN_SURFACE_TEMP(), LAND_AIR_TEMP())
results26 <- fetchvars(core26, 2000:2300, result_vars, scenario = "RCP 2.6")
results45 <- fetchvars(core45, 2000:2300, result_vars, scenario = "RCP 4.5")
results85 <- fetchvars(core85, 2000:2300, result_vars, scenario = "RCP 8.5")

results <- rbind(results26, results45, results85)


#Plotting as an area curve

#Filtering for centuries
results %>%
  filter(year %% 100 == 0 & year > 2000)->
  results

#Plotting as a bar chart, by scenario
ggplot(results)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity",
           position = "dodge2")+
  ylab("Temperature (Degrees Celsius)") +
  scale_fill_discrete(labels = c("Global", "Land", "Ocean Air", "Ocean Surface"))+
  facet_wrap(~scenario)+
  guides(fill = guide_legend(title = "Average Temperature"))

#Plotting as bar chart, by year
ggplot(results)+
  aes(x = scenario, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity",
           position = "dodge2")+
  ylab("Temperature (Degrees Celsius)") +
  scale_fill_discrete(labels = c("Global", "Land", "Ocean Air", "Ocean Surface"))+
  facet_wrap(~year)+
  guides(fill = guide_legend(title = "Average Temperature"))


#Shutdown Core
shutdown(core26)
shutdown(core45)
shutdown(core85)
