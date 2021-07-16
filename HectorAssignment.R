#Importing Libraries 
library(hector)
library(ggplot2)
library(dplyr)

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
carbonPlot1 <- ggplot(results85)+
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atmos_c" = "Atmospheric",
                                              "veg_c" = "Vegetation",
                                              "soil_c" = "Soil",
                                              "detritus_c" = "Detritus",
                                              "ocean_c" = "Ocean")))+
  ylab("Carbon (Pg C)")+
  xlab("Year")

##Area Graph
carbonPlot2 <- ggplot(results85)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")

##Bar Graph
results85 %>%
  filter(year %% 100 == 0)->
  resultsCentury85

#Collected in One Bar
carbonPlot3 <- ggplot(resultsCentury85)+
  aes(x = as.character(year), y = value, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  xlab("Year")

#Split into separate Facets
carbonPlot4 <- ggplot(resultsCentury85)+
  aes(x = as.character(year), y = value)+
  geom_bar(stat = "identity")+
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atmos_c" = "Atmospheric",
                                              "veg_c" = "Vegetation",
                                              "soil_c" = "Soil",
                                              "detritus_c" = "Detritus",
                                              "ocean_c" = "Ocean")))+
  ylab("Carbon (Pg C)") +
  xlab("Year")

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
#Not Facetted
fluxPlot1 <- ggplot(results85flux)+
  aes(x = year, y = value, color = variable) +
  geom_line() +
  scale_color_discrete(labels = c("Land", "Ocean", "Atmosphere"))+
  guides(color = guide_legend(title = "Carbon Flux(Pg C/yr)"))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")

#Facetted
fluxPlot2 <- ggplot(results85flux)+
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y",
             labeller = labeller(variable = c("atm_land_flux" = "Land Net Flux",
                                             "atm_ocean_flux" = "Ocean Net Flux",
                                            "atmosphere_flux" = "Atmosphere Net Flux")))+
  ylab("Carbon Flux(Pg C/yr)")+
  xlab("Year")




  