#Importing Libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

#Set Theme
theme_set(theme_bw())

## Basic Run
#Configuring INI File
ini_file_norm <- system.file("input", "hector_rcp26.ini", package = "hector")
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_jay <- paste(path, "JayScenario1.ini", sep="")

#Initialize a Hector Instance for each file
coreNorm <- newcore(ini_file_norm)
coreJay1 <- newcore(ini_file_jay)

#Run the Cores
run(coreNorm) 
run(coreJay1)

#Retrieve Results
resultsNorm <- fetchvars(coreNorm, 2005:2100, scenario = "Normal")
resultsJay1 <- fetchvars(coreJay1, 2005:2100, scenario = "Jay1")

#Retrieve fluxes Results 
result_vars <- c(OCEAN_CFLUX(), LAND_CFLUX(), FFI_EMISSIONS())
resultsNormFlux <- fetchvars(coreNorm, 2005:2100, result_vars, scenario = "Normal")
resultsJay1Flux <- fetchvars(coreJay1, 2005:2100, result_vars,  scenario = "Jay1")

#Combining into one dataset 
results <- rbind(resultsNorm, resultsJay1)
resultsFlux <- rbind(resultsNormFlux, resultsJay1Flux)

#Calculating Atmospheric flux 
resultsFlux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  resultsFlux

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

#Comparing RCPs
lineGraphCompare
fluxFacetLineCompare