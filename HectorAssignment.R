#Importing Libraries 
library(hector)
library(ggplot2)
library(dplyr)

#Creating rcp85 Core
rcp85 <- system.file("input", "hector_rcp85.ini", package = "hector")
core85 <- newcore(rcp85, suppresslogging  = TRUE)