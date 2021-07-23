#Install Local Hector
#devtools::install("/Users/Natanel Ha/Documents/GitHub/hector")

#Importing Libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

## Basic Run
#Configuring INI File
ini_file <- system.file("input/hector_rcp45.ini", package = "hector") 

#Initialize a Hector Instance
core <- newcore(ini_file)
core 

#Run the Core
run(core) 

#Retrieve Results
results <- fetchvars(core, 2000:2300)
head(results) 


#Plotting Results
ggplot(results) +
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("Ca" = "CO2 Concentration (ppmv CO2)",
                                              "Ftot" = "Total Radiative Forcing (W/m2)",
                                              "FCO2" = "CO2 Forcing (W/m2)",
                                              "Tgav" = "Global Mean Temperature (degrees C)")))+
  ylab(NULL)

#Getting Results
result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), VEG_C(), DETRITUS_C(), OCEAN_C())
results <- fetchvars(core, 2000:2300, result_vars, scenario = "RCP 4.5")

#Plotting results
##Line Graph
lineGraph <- ggplot(results)+
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atmos_c" = "Atmospheric",
                                              "veg_c" = "Vegetation",
                                              "soil_c" = "Soil",
                                              "detritus_c" = "Detritus",
                                              "ocean_c" = "Ocean")))+
  ylab("Carbon (Pg C)")+
  xlab("Year")+
  theme_bw()

##Area Graph
areaGraph <- ggplot(results)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")+
  theme_bw()

##Bar Graph Filtering
results %>%
  filter(year %% 100 == 0)->
  resultsCentury

#Collected in One Bar
barPlot <- ggplot(resultsCentury)+
  aes(x = as.character(year), y = value, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")+
  theme_bw()

#Split into separate Facets
splitBarPlot <- ggplot(resultsCentury)+
  aes(x = as.character(year), y = value)+
  geom_bar(stat = "identity")+
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atmos_c" = "Atmospheric",
                                              "veg_c" = "Vegetation",
                                              "soil_c" = "Soil",
                                              "detritus_c" = "Detritus",
                                              "ocean_c" = "Ocean")))+
  ylab("Carbon (Pg C)") +
  xlab("Year")+
  theme_bw()

##Looking at fluxes
result_vars <- c(OCEAN_CFLUX(), LAND_CFLUX(), FFI_EMISSIONS())
resultsflux <- fetchvars(core, 2000:2300, result_vars, scenario = "RCP 4.5")

#Calculating atmosphere flux
resultsflux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  resultsflux

##Plotting Fluxes
#Not Faceted
fluxLine <- ggplot(resultsflux)+
  aes(x = year, y = value, color = variable) +
  geom_line() +
  scale_color_discrete(labels = c("Land", "Ocean", "Atmosphere"))+
  guides(color = guide_legend(title = "Carbon Flux(Pg C/yr)"))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")+
  theme_bw()

#Faceted
fluxFacetLine <- ggplot(resultsflux)+
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y",
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")+
  theme_bw()

##Bar Graph Fluxes
resultsflux %>%
  filter(year %% 100 == 0)->
  resultsfluxCentury

fluxBar <- ggplot(resultsfluxCentury)+
  aes(x = as.character(year), y = value)+
  geom_bar(stat = "identity")+
  facet_wrap(~variable, 
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)") +
  xlab("Year")+
  theme_bw()

#RCP 4.5 Plots
lineGraph
areaGraph
barPlot
splitBarPlot
fluxLine
fluxFacetLine
fluxBar 