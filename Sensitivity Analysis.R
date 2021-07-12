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

#Sensitivity analysis 
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

sensitivity_beta <- run_with_param_range(core, BETA(), seq(0, 1, 0.05))

ggplot(sensitivity_beta) +
  aes(x = year, y = value, color = parameter_value, group = parameter_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_colorbar(title = expression(beta))) +
  scale_color_viridis_c() 