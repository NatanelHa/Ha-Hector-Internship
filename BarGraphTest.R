##Importing Libraries 
library(hector)
library(ggplot2)
library(dplyr)

#Creating Multiple Cores
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core45 <- newcore(rcp45, suppresslogging  = TRUE)

#Run Cores
run(core45)

#Retrieve Results
result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), DETRITUS_C(), VEG_C())
results45 <- fetchvars(core45, 2000:2300, result_vars, scenario = "RCP 4.5")

results45 %>%
  filter(year %% 100 == 0) ->
  results45

ggplot(results45)+
  aes(x = year, y = value, fill = variable)+
  geom_bar(stat = "identity")+
  ylab("Carbon (Pg C)")


#Shutdown Core
shutdown(core)
