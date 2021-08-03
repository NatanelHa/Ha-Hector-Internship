#Importing Libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

#Set Theme
theme_set(theme_bw())

## Basic Run
#Configuring INI File
ini_file_norm <- system.file("input", "hector_rcp26.ini", package = "hector")
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_jay1 <- paste(path, "JayScenario1.ini", sep="")
#ini_file_jay2 <- paste(path, "JayScenario2.ini", sep="")
#ini_file_jay3 <- paste(path, "JayScenario3.ini", sep="")
#ini_file_jay4 <- paste(path, "JayScenario4.ini", sep="")
#ini_file_jay5 <- paste(path, "JayScenario5.ini", sep="")
ini_file_jay6 <- paste(path, "JayScenario6.ini", sep="")

#Initialize a Hector Instance for each file
coreNorm <- newcore(ini_file_norm)
coreJay1 <- newcore(ini_file_jay1)
#coreJay2 <- newcore(ini_file_jay2)
#coreJay3 <- newcore(ini_file_jay3)
#coreJay4 <- newcore(ini_file_jay4)
#coreJay5 <- newcore(ini_file_jay5)
coreJay6 <- newcore(ini_file_jay6)

#Run the Cores
run(coreNorm) 
run(coreJay1)
#run(coreJay2)
#run(coreJay3)
#run(coreJay4)
#run(coreJay5)
run(coreJay6)

#Retrieve Results
resultsNorm <- fetchvars(coreNorm, 2005:2100, scenario = "Normal")
resultsJay1 <- fetchvars(coreJay1, 2005:2100, scenario = "Jay")
#resultsJay2 <- fetchvars(coreJay2, 2005:2100, scenario = "Jay No Luc")
#resultsJay3 <- fetchvars(coreJay3, 2005:2100, scenario = "Jay CCS")
#resultsJay4 <- fetchvars(coreJay4, 2005:2100, scenario = "Jay CCS No Luc")
#resultsJay5 <- fetchvars(coreJay5, 2005:2100, scenario = "Jay CCS Pos Luc")
resultsJay6 <- fetchvars(coreJay6, 2005:2100, scenario = "Jay Unsplit")


#Retrieve fluxes Results 
result_vars <- c(OCEAN_CFLUX(), LAND_CFLUX(), FFI_EMISSIONS(), DACCS_UPTAKE())
resultsNormFlux <- fetchvars(coreNorm, 2005:2100, result_vars, scenario = "Normal")
resultsJay1Flux <- fetchvars(coreJay1, 2005:2100, result_vars, scenario = "Jay")
#resultsJay2Flux <- fetchvars(coreJay2, 2005:2100, result_vars, scenario = "Jay No Luc")
#resultsJay3Flux <- fetchvars(coreJay3, 2005:2100, result_vars, scenario = "Jay CCS")
#resultsJay4Flux <- fetchvars(coreJay4, 2005:2100, result_vars, scenario = "Jay CCS No Luc")
#resultsJay5Flux <- fetchvars(coreJay5, 2005:2100, result_vars, scenario = "Jay CCS Pos Luc")
resultsJay6Flux <- fetchvars(coreJay6, 2005:2100, result_vars, scenario = "Jay Unsplit")

#Combining into one dataset 
results <- rbind(resultsNorm, resultsJay1, resultsJay6)
resultsFlux <- rbind(resultsNormFlux, resultsJay1Flux, resultsJay6Flux)

#Calculating Atmospheric flux 
resultsFlux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - daccs_uptake - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions, -daccs_uptake) %>%
  pivot_longer(3:5, names_to = "variable")->
  resultsFlux

#Label data
data_label <- results                            
data_label$label <- NA
data_label$label[which(data_label$year == max(data_label$year))] <- 
  data_label$scenario[which(data_label$year == max(data_label$year))]

##Line Graph
lineGraphCompare <- ggplot(data_label)+
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("Ca" = "CO2 Concentration (ppmv CO2)",
                                              "Ftot" = "Total Radiative Forcing (W/m2)",
                                              "FCO2" = "CO2 Forcing (W/m2)",
                                              "Tgav" = "Global Mean Temperature (degrees C)")))+
  ylab(NULL)+
  guides(color = guide_legend(title = "Scenario"))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  #scale_color_viridis_d()+
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
  #scale_color_viridis_d()+
  xlab("Year")

#Comparing RCPs
lineGraphCompare
fluxFacetLineCompare


