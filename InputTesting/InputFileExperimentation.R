#Importing Libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

#Set Theme
theme_set(theme_bw())

## Basic Run
#Configuring INI File
ini_file_norm <- system.file("input", "hector_rcp45.ini", package = "hector")
ini_file_3pump <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/InputTesting/myInputPump3.ini"
ini_file_7pump <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/InputTesting/myInputPump7.ini"

#Initialize a Hector Instance
coreNorm <- newcore(ini_file_norm)
core3pump <- newcore(ini_file_3pump)
core7pump <- newcore(ini_file_7pump)

#Run the Cores
run(coreNorm) 
run(core3pump)
run(core7pump)

#Retrieve Results
resultsNorm <- fetchvars(coreNorm, 2000:2100, scenario = "Normal")
results3 <- fetchvars(core3pump, 2000:2100, scenario = "Pump 3x")
results7 <- fetchvars(core7pump, 2000:2100, scenario = "Pump 7x")

#Retrieve fluxes Results 
result_vars <- c(OCEAN_CFLUX(), LAND_CFLUX(), FFI_EMISSIONS())
resultsNormFlux <- fetchvars(coreNorm, 2000:2100, result_vars, scenario = "Normal")
results3Flux <- fetchvars(core3pump, 2000:2100, result_vars,  scenario = "Pump 3x")
results7Flux <- fetchvars(core7pump, 2000:2100, result_vars,  scenario = "Pump 7x")

##Calculating atmosphere flux
resultsNormFlux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  resultsNormFlux

results3Flux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  results3Flux

results7Flux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  results7Flux


#Combining
results <- rbind(resultsNorm, results3, results7)
resultsFlux <- rbind(resultsNormFlux, results3Flux, results7Flux)

#Line graph
##Line Graph
lineGraphCompare <- ggplot(results)+
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("Ca" = "CO2 Concentration (ppmv CO2)",
                                              "Ftot" = "Total Radiative Forcing (W/m2)",
                                              "FCO2" = "CO2 Forcing (W/m2)",
                                              "Tgav" = "Global Mean Temperature (degrees C)")))+
  ylab(NULL)+
  guides(color = guide_legend(title = "Scenario"))+
  xlab("Year")
  

##Plotting Fluxes
#Faceted
fluxFacetLineCompare <- ggplot(resultsFlux)+
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y",
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")
  

##Bar Graph Fluxes
resultsFlux %>%
  filter(year %% 100 == 0)->
  resultsfluxCentury

fluxBarCompare <- ggplot(resultsfluxCentury)+
  aes(x = as.character(year), y = value, fill = scenario)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~variable, 
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)") +
  xlab("Year")


#Comparing RCPs
lineGraphCompare
fluxFacetLineCompare
fluxBarCompare