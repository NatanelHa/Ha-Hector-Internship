#Configuring INI File
library(hector) 
ini_file <- system.file("input/hector_rcp45.ini", package = "hector") 

#Initialize a Hector Instance
core <- newcore(ini_file)
core 

#Run the Core
run(core) 

#Retrieve Results
results <- fetchvars(core, 2000:2300)
head(results) 

#Plotting Results
library(ggplot2)

ggplot(results) +
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

