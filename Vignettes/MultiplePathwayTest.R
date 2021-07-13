#Importing Libraries 
library(hector)
library(ggplot2)

#Creating Multiple Cores
rcp26 <- system.file("input", "hector_rcp26.ini", package = "hector")
core26 <- newcore(rcp26, suppresslogging  = TRUE)

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core45 <- newcore(rcp45, suppresslogging  = TRUE)

rcp60 <- system.file("input", "hector_rcp60.ini", package = "hector")
core60 <- newcore(rcp60, suppresslogging  = TRUE)

rcp85 <- system.file("input", "hector_rcp85.ini", package = "hector")
core85 <- newcore(rcp85, suppresslogging  = TRUE)
