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

shutdown(core)
#trackingdata <- readr::read_csv("output/tracking_rcp45.csv")