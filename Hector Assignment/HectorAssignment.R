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
  facet_wrap(~variable,
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
  facet_wrap(~variable, 
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
  facet_wrap(~variable, 
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
  result <- fetchvars(core, 2000:2100, result_vars)
  result[["parameter_value"]] <- value
  result
}

#Run Hector with a range of parameter values
run_with_param_range <- function(core, parameter, values) {
  mapped <- Map(function(x) run_with_param(core, parameter, x), values)
  Reduce(rbind, mapped)
}

#Perform sensitivity analysis 
sensitivity_analysis <- function(core, parameter, sequence){
  #Get Old value
  old_value <- fetchvars(core, NA, parameter)
  unit <- as.character(old_value[["units"]])
  
  #Sensitivity 
  sensitivity <- run_with_param_range(core, parameter, sequence)
  
  #Calculating Atmosphere Flux
  sensitivity %>%
    select(-units) %>%
    pivot_wider(names_from = variable) %>%
    mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
    select(-ffi_emissions,-atm_ocean_flux,-atm_land_flux) ->
    sensitivity
  
  #Create plot
  sensitivity_plot <<- ggplot(sensitivity) +
    aes(x = year, y = atmosphere_flux, color = parameter_value, group = parameter_value) +
    geom_line() +
    ylab("Atmosphheric flux (Pg C/yr)")+
    guides(color = guide_colorbar(title = parameter)) +
    scale_color_viridis_c() 
  
  #Reset var
  setvar(core, NA, parameter, first(old_value$value), unit) 
  
  #Finding cutoff
  sensitivity %>%
    filter(year==2100, atmosphere_flux<=0.5)->
    working
  
  working %>%
    mutate(distance = abs(parameter_value - first(old_value$value)))%>%
    arrange(distance)->
    working
  
  return(first(working$parameter_value))
}

find_values <- function(core, betaValues, q10rhValues){
  beta <- betaValues
  q10_rh <- c()
  for(val in betaValues){
    setvar(core, NA, BETA(), val, "(unitless)")
    q10_rh <- append(q10_rh, sensitivity_analysis(core, Q10_RH(), q10rhValues))
  }
  setvar(core, NA, BETA(), 0.36, "(unitless)")
  return(data.frame(beta, q10_rh))
}

#RCP 8.5
sensitivity_analysis(core85, BETA(), seq(0,20,0.5))
sensitivity_plot

sensitivity_analysis(core85, Q10_RH(), seq(0.05,2,0.05))
sensitivity_plot

find_values(core85, seq(14,19,0.5), seq(0.05,2,0.05))

#RCP 4.5
sensitivity_analysis(core45, BETA(), seq(0,2,0.05))
sensitivity_plot

sensitivity_analysis(core45, Q10_RH(), seq(1,2,0.025))
sensitivity_plot

find_values(core45, seq(0.36,0.54,0.02), seq(1.75,2,0.01))

#Shutting down Cores
shutdown(core85)
shutdown(core45)

###Extra Experimentation
#RCP 6.0
#Creating rcp60 Core
rcp60 <- system.file("input", "hector_rcp60.ini", package = "hector")
core60 <- newcore(rcp60, suppresslogging  = TRUE)

#Run Core
run(core60)

sensitivity_analysis(core60, BETA(), seq(0,10,0.5))
sensitivity_plot

sensitivity_analysis(core60, Q10_RH(), seq(0.05,2,0.05))
sensitivity_plot

find_values(core60, seq(4,6,0.2), seq(0.05,2,0.05))

#RCP 2.6
#Creating rcp26 Core
rcp26 <- system.file("input", "hector_rcp26.ini", package = "hector")
core26 <- newcore(rcp26, suppresslogging  = TRUE)

#Run Core
run(core26)

sensitivity_analysis(core26, BETA(), seq(0,1,0.05))
sensitivity_plot

sensitivity_analysis(core26, Q10_RH(), seq(1,4,0.25))
sensitivity_plot

##Results
result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), VEG_C(), DETRITUS_C(), OCEAN_C())
results60 <- fetchvars(core60, 2000:2100, result_vars, scenario = "RCP 6.0")
results26 <- fetchvars(core26, 2000:2100, result_vars, scenario = "RCP 2.6")

result_vars <- c(OCEAN_CFLUX(), LAND_CFLUX(), FFI_EMISSIONS())
results60flux <- fetchvars(core60, 2000:2100, result_vars, scenario = "RCP 6.0")
results26flux <- fetchvars(core26, 2000:2100, result_vars, scenario = "RCP 2.6")

##Calculating atmosphere flux
results60flux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  results60flux

results26flux %>%
  select(-units) %>%
  pivot_wider(names_from = variable) %>%
  mutate(atmosphere_flux = (ffi_emissions - atm_ocean_flux - atm_land_flux))%>%
  select(-ffi_emissions) %>%
  pivot_longer(3:5, names_to = "variable")->
  results26flux


##Comparing the four  RCPs
#Combining
resultsC <- rbind(results85, results45, results60, results26)
resultsFlux <- rbind(results85flux, results45flux, results60flux, results26flux)

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
  facet_wrap(~variable, 
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