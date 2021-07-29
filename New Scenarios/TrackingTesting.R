#Importing libraries
library(hector)
library(ggplot2)
library(latex2exp)

## Basic Run
#Configuring INI File
ini_file <- system.file("input/hector_rcp26.ini", package = "hector") 

#Initialize a Hector Instance
core <- newcore(ini_file)
core 

#Run the Core
run(core) 

td <- get_tracking_data(core)
tracking <-read.csv(textConnection(td))

shutdown(core)