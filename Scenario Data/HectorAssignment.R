#Importing Libraries 
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

#Creating rcp85 Core
rcp85 <- system.file("input", "hector_rcp85.ini", package = "hector")
core85 <- newcore(rcp85, suppresslogging  = TRUE)

#Run Core
run(core85)

#Getting Results
result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), VEG_C(), DETRITUS_C(), OCEAN_C())
results85 <- fetchvars(core85, 2000:2100, result_vars, scenario = "RCP 8.5")

#Plotting results
##Line Graph
lineGraph85 <- ggplot(results85)+
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atmos_c" = "Atmospheric",
                                              "veg_c" = "Vegetation",
                                              "soil_c" = "Soil",
                                              "detritus_c" = "Detritus",
                                              "ocean_c" = "Ocean")))+
  ylab("Carbon (Pg C)")+
  theme_bw()+
  xlab("Year")

##Area Graph
areaGraph85 <- ggplot(results85)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")+
  theme_bw()

##Bar Graph Filtering
results85 %>%
  filter(year %% 100 == 0)->
  resultsCentury85

#Collected in One Bar
barPlot85 <- ggplot(resultsCentury85)+
  aes(x = as.character(year), y = value, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")+
  theme_bw()

#Split into separate Facets
splitBarPlot85 <- ggplot(resultsCentury85)+
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
results85flux <- fetchvars(core85, 2000:2100, result_vars, scenario = "RCP 8.5")

#Calculating atmosphere flux
results85flux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  results85flux
  
##Plotting Fluxes
#Not Faceted
fluxLine85 <- ggplot(results85flux)+
  aes(x = year, y = value, color = variable) +
  geom_line() +
  scale_color_discrete(labels = c("Land", "Ocean", "Atmosphere"))+
  guides(color = guide_legend(title = "Carbon Flux(Pg C/yr)"))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")+
  theme_bw()

#Faceted
fluxFacetLine85 <- ggplot(results85flux)+
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
results85flux %>%
  filter(year %% 100 == 0)->
  results85fluxCentury

fluxBar85 <- ggplot(results85fluxCentury)+
  aes(x = as.character(year), y = value)+
  geom_bar(stat = "identity")+
  facet_wrap(~variable, scales = "free_y",
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)") +
  xlab("Year")+
  theme_bw()

#RCP 8.5 Plots
lineGraph85
areaGraph85
barPlot85
splitBarPlot85
fluxLine85
fluxFacetLine85
fluxBar85

##Runnin RCP 4.5
#Creating rcp85 Core
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core45 <- newcore(rcp45, suppresslogging  = TRUE)

#Run Core
run(core45)

#Getting Results
result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), VEG_C(), DETRITUS_C(), OCEAN_C())
results45 <- fetchvars(core45, 2000:2100, result_vars, scenario = "RCP 4.5")

#Plotting results
##Line Graph
lineGraph45 <- ggplot(results45)+
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
areaGraph45 <- ggplot(results45)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")+
  theme_bw()

##Bar Graph Filtering
results45 %>%
  filter(year %% 100 == 0)->
  resultsCentury45

#Collected in One Bar
barPlot45 <- ggplot(resultsCentury45)+
  aes(x = as.character(year), y = value, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")+
  theme_bw()

#Split into separate Facets
splitBarPlot45 <- ggplot(resultsCentury45)+
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
results45flux <- fetchvars(core45, 2000:2100, result_vars, scenario = "RCP 4.5")

#Calculating atmosphere flux
results45flux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  results45flux

##Plotting Fluxes
#Not Faceted
fluxLine45 <- ggplot(results45flux)+
  aes(x = year, y = value, color = variable) +
  geom_line() +
  scale_color_discrete(labels = c("Land", "Ocean", "Atmosphere"))+
  guides(color = guide_legend(title = "Carbon Flux(Pg C/yr)"))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")+
  theme_bw()

#Faceted
fluxFacetLine45 <- ggplot(results45flux)+
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
results45flux %>%
  filter(year %% 100 == 0)->
  results45fluxCentury

fluxBar45 <- ggplot(results45fluxCentury)+
  aes(x = as.character(year), y = value)+
  geom_bar(stat = "identity")+
  facet_wrap(~variable, scales = "free_y",
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)") +
  xlab("Year")+
  theme_bw()

#RCP 4.5 Plots
lineGraph45
areaGraph45
barPlot45
splitBarPlot45
fluxLine45
fluxFacetLine45
fluxBar45 

##Comparing two RCPs
#Combining
resultsC <- rbind(results85, results45)
resultsFlux <- rbind(results85flux, results45flux)

#Line graph
##Line Graph
lineGraphCompare <- ggplot(resultsC)+
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atmos_c" = "Atmospheric",
                                              "veg_c" = "Vegetation",
                                              "soil_c" = "Soil",
                                              "detritus_c" = "Detritus",
                                              "ocean_c" = "Ocean")))+
  ylab("Carbon (Pg C)")+
  guides(color = guide_legend(title = "Scenario"))+
  xlab("Year")+
  theme_bw()

##Area Graph
areaGraphCompare <- ggplot(resultsC)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  facet_wrap(~scenario)+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")+
  theme_bw()

##Bar Graph Filtering
resultsC %>%
  filter(year %% 100 == 0)->
  resultsCenturyCompare

#Collected in One Bar
barPlotCompare <- ggplot(resultsCenturyCompare)+
  aes(x = scenario, y = value, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  facet_wrap(~year)+
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab(NULL)+
  theme_bw()

#Split into separate Facets
splitBarPlotCompare <- ggplot(resultsCenturyCompare)+
  aes(x = as.character(year), y = value, fill = scenario)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atmos_c" = "Atmospheric",
                                              "veg_c" = "Vegetation",
                                              "soil_c" = "Soil",
                                              "detritus_c" = "Detritus",
                                              "ocean_c" = "Ocean")))+
  ylab("Carbon (Pg C)") +
  xlab("Year")+
  theme_bw()

##Plotting Fluxes
#Not Faceted
fluxLineCompare <- ggplot(resultsFlux)+
  aes(x = year, y = value, color = variable, linetype = scenario) +
  geom_line() +
  scale_color_discrete(labels = c("Land", "Ocean", "Atmosphere"))+
  guides(color = guide_legend(title = "Carbon Flux(Pg C/yr)"))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")+
  theme_bw()

#Faceted
fluxFacetLineCompare <- ggplot(resultsFlux)+
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y",
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")+
  theme_bw()

##Bar Graph Fluxes
resultsFlux %>%
  filter(year %% 100 == 0)->
  resultsfluxCentury

fluxBarCompare <- ggplot(resultsfluxCentury)+
  aes(x = as.character(year), y = value, fill = scenario)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~variable, scales = "free_y",
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                              "atm_ocean_flux" = "Ocean Net Flux",
                                              "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)") +
  theme_bw()+
  xlab("Year")

#Comparing RCPs
lineGraphCompare
areaGraphCompare
barPlotCompare
splitBarPlotCompare
fluxLineCompare
fluxFacetLineCompare
fluxBarCompare

## Finding lowest change that gives below 0.5 Pg/c Atmosphere Flux
##Finding out what the current values of beta and Q10_RH
beta <- fetchvars(core85, NA, BETA())
beta 
q10rh <- fetchvars(core85, NA, Q10_RH())
q10rh 

##Sensitivity analysis 
#Function to run Hector with a parameter
run_with_param <- function(core, parameter, value) {
  old_value <- fetchvars(core, NA, parameter)
  unit <- as.character(old_value[["units"]])
  setvar(core, NA, parameter, value, unit)
  reset(core)
  run(core)
  result_vars <- c(OCEAN_CFLUX(), LAND_CFLUX(), FFI_EMISSIONS())
  result <- fetchvars(core, 2000:2100, result_vars, scenario = "RCP 8.5")
  result[["parameter_value"]] <- value
  result
}

#Run Hector with a range of parameter values
run_with_param_range <- function(core, parameter, values) {
  mapped <- Map(function(x) run_with_param(core, parameter, x), values)
  Reduce(rbind, mapped)
}

#RCP 85
#Sensitivity analysis for Beta
sensitivity_beta <- run_with_param_range(core85, BETA(), seq(0.36, 5.5, 0.12))

#Calculating Atmosphere Flux
sensitivity_beta %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions,-atm_ocean_flux,-atm_land_flux) ->
  sensitivity_beta

#Plotting the range of different values 
ggplot(sensitivity_beta) +
  aes(x = year, y = atmosphere_flux, color = parameter_value, group = parameter_value) +
  geom_line() +
  ylab("Atmosphheric flux (Pg C/yr)")+
  guides(color = guide_colorbar(title = expression(beta))) +
  scale_color_viridis_c() 

#Reseting Beta
setvar(core85, NA, BETA(), 0.36, "(unitless)") 

#Sensitivity for q10rh
sensitivity_q10rh <- run_with_param_range(core85, Q10_RH(), seq(0.05, 2, 0.05))

#Calculating Atmosphere Flux
sensitivity_q10rh %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions,-atm_ocean_flux,-atm_land_flux) ->
  sensitivity_q10rh

#Plotting the range of different values 
ggplot(sensitivity_q10rh) +
  aes(x = year, y = atmosphere_flux, color = parameter_value, group = parameter_value) +
  geom_line() +
  ylab("Atmosphheric flux (Pg C/yr)")+
  guides(color = guide_colorbar(title = "Q10_RH")) +
  scale_color_viridis_c()

#Resetting q10rh
setvar(core85, NA, Q10_RH(), 2, "(unitless)")

#RCP 4.5
#Sensitivity analysis for Beta
sensitivity_beta <- run_with_param_range(core45, BETA(), seq(0.36, 5.5, 0.12))

#Calculating Atmosphere Flux
sensitivity_beta %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions,-atm_ocean_flux,-atm_land_flux) ->
  sensitivity_beta

#Plotting the range of different values 
ggplot(sensitivity_beta) +
  aes(x = year, y = atmosphere_flux, color = parameter_value, group = parameter_value) +
  geom_line() +
  ylab("Atmosphheric flux (Pg C/yr)")+
  guides(color = guide_colorbar(title = expression(beta))) +
  scale_color_viridis_c() 

#Reseting Beta
setvar(core45, NA, BETA(), 0.36, "(unitless)") 

#Sensitivity for q10rh
sensitivity_q10rh <- run_with_param_range(core45, Q10_RH(), seq(0.05, 2, 0.05))

#Calculating Atmosphere Flux
sensitivity_q10rh %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions,-atm_ocean_flux,-atm_land_flux) ->
  sensitivity_q10rh

#Plotting the range of different values 
ggplot(sensitivity_q10rh) +
  aes(x = year, y = atmosphere_flux, color = parameter_value, group = parameter_value) +
  geom_line() +
  ylab("Atmosphheric flux (Pg C/yr)")+
  guides(color = guide_colorbar(title = "Q10_RH")) +
  scale_color_viridis_c()

#Resetting q10rh
setvar(core45, NA, Q10_RH(), 2, "(unitless)")
