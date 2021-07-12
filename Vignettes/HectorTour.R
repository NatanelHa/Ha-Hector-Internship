#Importing Libraries
library(hector)
library(ggplot2)

## Basic Run
#Configuring INI File
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
ggplot(results) +
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("Ca" = "CO2 Concentration",
                                          "Ftot" = "Total Radiative Forcing",
                                          "FCO2" = "CO2 Forcing",
                                          "Tgav" = "Global Mean Temperature")))

##Setting Parameters
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
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("Ca" = "CO2 Concentration",
                                              "Ftot" = "Total Radiative Forcing",
                                              "FCO2" = "CO2 Forcing",
                                              "Tgav" = "Global Mean Temperature"))) +
  guides(color = guide_legend(title = expression(beta)))

##Sensitivity analysis 
run_with_param <- function(core, parameter, value) {
  old_value <- fetchvars(core, NA, parameter)
  unit <- as.character(old_value[["units"]])
  setvar(core, NA, parameter, value, unit)
  reset(core)
  run(core)
  result <- fetchvars(core, 2000:2300)
  result[["parameter_value"]] <- value
  result
}

#Run Hector with a range of parameter values
run_with_param_range <- function(core, parameter, values) {
  mapped <- Map(function(x) run_with_param(core, parameter, x), values)
  Reduce(rbind, mapped)
}

#Specifically look at a range of Beta values
sensitivity_beta <- run_with_param_range(core, BETA(), seq(0, 1, 0.05))

#Plotting the range of different values 
ggplot(sensitivity_beta) +
  aes(x = year, y = value, color = parameter_value, group = parameter_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("Ca" = "CO2 Concentration",
                                              "Ftot" = "Total Radiative Forcing",
                                              "FCO2" = "CO2 Forcing",
                                              "Tgav" = "Global Mean Temperature"))) +
  guides(color = guide_colorbar(title = expression(beta))) +
  scale_color_viridis_c() 

#Shutting down
shutdown(core)
