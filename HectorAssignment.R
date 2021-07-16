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



