#Importing Libraries 
library(hector)
library(ggplot2)
library(dplyr)

#Creating Multiple Cores
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(rcp45, suppresslogging  = TRUE)

run(core)

result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), LAND_CFLUX())
results <- fetchvars(core, 2000:2200, result_vars, scenario = "normal")


#Change value and rerun
setvar(core, NA, WARMINGFACTOR(), 0.50, "(unitless)") 
reset(core)
run(core)
results_half <- fetchvars(core, 2000:2200, result_vars, scenario = "half")

#Change value and rerun
setvar(core, NA, WARMINGFACTOR(), 2, "(unitless)") 
reset(core)
run(core)
results_double <- fetchvars(core, 2000:2200, result_vars, scenario = "double")

results[["Warming_Factor"]] <- 1
results_half[["Warming_Factor"]] <- 0.5
results_double[["Warming_Factor"]] <- 2

final_results <- rbind(results, results_half, results_double)


#Plotting the Change
ggplot(final_results) +
  aes(x = year, y = value, color = factor(Warming_Factor)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", 
             labeller = labeller(variable = c("atm_land_flux" = "Atmosphere Land Flux (Pg C/yr))",
                                              "atmos_c" = "Atmospheric Carbon (Pg C)",
                                              "soil_c" = "Soil Carbon (Pg C)")))+
  ylab(NULL)+
  guides(color = guide_legend(title = "Warming Factor"))
