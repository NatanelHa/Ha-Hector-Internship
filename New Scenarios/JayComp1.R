ini_file_SSP1 <- paste(path, "jay_SSP1.ini", sep="")
ini_file_SSP2 <- paste(path, "jay_SSP2.ini", sep="")
ini_file_SSP4 <- paste(path, "jay_SSP4.ini", sep="")
ini_file_SSP5 <- paste(path, "jay_SSP5.ini", sep="")

#Initialize a Hector Instance for each file
coreNorm <- newcore(ini_file_norm)
coreSSP1 <- newcore(ini_file_SSP1)
coreSSP2 <- newcore(ini_file_SSP2)
coreSSP4 <- newcore(ini_file_SSP4)
coreSSP5 <- newcore(ini_file_SSP5)

#Run the Cores
run(coreNorm) 
run(coreSSP1)
run(coreSSP2)
run(coreSSP4)
run(coreSSP5)

#Retrieve Results
resultsNorm <- fetchvars(coreNorm, 2005:2100, scenario = "Hector 2.6")
resultsSSP1 <- fetchvars(coreSSP1, 2005:2100, scenario = "SSP1")
resultsSSP2 <- fetchvars(coreSSP2, 2005:2100, scenario = "SSP2")
resultsSSP4 <- fetchvars(coreSSP4, 2005:2100, scenario = "SSP4")
resultsSSP5 <- fetchvars(coreSSP5, 2005:2100, scenario = "SSP5")

#Retrieve fluxes Results 
result_vars <- c(OCEAN_CFLUX(), LAND_CFLUX(), FFI_EMISSIONS(), DACCS_UPTAKE())
resultsNormFlux <- fetchvars(coreNorm, 2005:2100, result_vars, scenario = "Hector 2.6")
resultsSSP1Flux <- fetchvars(coreSSP1, 2005:2100, result_vars, scenario = "SSP1")
resultsSSP2Flux <- fetchvars(coreSSP2, 2005:2100, result_vars, scenario = "SSP2")
resultsSSP4Flux <- fetchvars(coreSSP4, 2005:2100, result_vars, scenario = "SSP4")
resultsSSP5Flux <- fetchvars(coreSSP5, 2005:2100, result_vars, scenario = "SSP5")


#Combining into one dataset 
results <- rbind(resultsNorm, resultsSSP1, resultsSSP2,
                 resultsSSP4, resultsSSP5)
resultsFlux <- rbind(resultsNormFlux, resultsSSP1Flux, resultsSSP2Flux, 
                     resultsSSP4Flux, resultsSSP5Flux)

#Calculating Atmospheric flux 
resultsFlux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - daccs_uptake - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions, -daccs_uptake) %>%
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
  scale_color_viridis_d()+
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
  scale_color_viridis_d()+
  xlab("Year")

lineGraphCompare
fluxFacetLineCompare