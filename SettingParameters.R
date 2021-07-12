#Importing Hector Library
library(hector)

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

#Getting Beta value
BETA() #Only returns string

beta <- fetchvars(core, NA, BETA())
beta 

#Changing Beta
setvar(core, NA, BETA(), 0.40, "(unitless)") 

#Checking if it was changed
fetchvars(core, NA, BETA()) 

#Rerun Hector
core
reset(core)
run(core)

#Obtain Results
results_40 <- fetchvars(core, 2000:2300)
head(results_40) 

#Seeing How Results Change
results[["beta"]] <- 0.36
results_40[["beta"]] <- 0.40
compare_results <- rbind(results, results_40)

#Plotting the Change
ggplot(compare_results) +
  aes(x = year, y = value, color = factor(beta)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_legend(title = expression(beta)))
