#Importing libraries
library(hector)
library(ggplot2)

## Basic Run
#Configuring INI Files
ini_file26 <- system.file("input/hector_rcp26.ini", package = "hector") 
ini_file45 <- system.file("input/hector_rcp45.ini", package = "hector") 

#Initialize the Cores
core26 <- newcore(ini_file26)
core45 <- newcore(ini_file45)

#Run the Core
run(core26) 
run(core45) 

#Getting tracking
td26 <- get_tracking_data(core26)
td45 <- get_tracking_data(core45)

#Shutdown
shutdown(core26)
shutdown(core45)





