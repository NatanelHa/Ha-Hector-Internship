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
  ylab("Carbon in Pool(Pg C)")

##Area Graph
carbonPlot2 <- ggplot(results85)+
  aes(x = year, y = value, group = scenario, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)") +
  scale_fill_discrete(labels = c("Atmosphere", "Detritus","Ocean", "Soil", "Vegetation"))+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))

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

#Split into sperate Factes
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

