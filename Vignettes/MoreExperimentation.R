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


##Multiple Biomes Experimentation

reset(core)
run(core, 2100)

result_vars <- c(ATMOSPHERIC_CO2(), RF_TOTAL(), GLOBAL_TEMP(),
                 VEG_C(), SOIL_C(), DETRITUS_C())

base_results <- fetchvars(core, 2000:2100, result_vars, scenario = "reference")

#Redoing run with high-latitude warming
reset(core)
split_biome(core, "global", c("Northern-Hemisphere", "Southern-Hemisphere"),
            fveg_c = c(0.6, 0.4),
            fdetritus_c = c(0.7, 0.3),
            fsoil_c = c(0.8, 0.2))
            
run(core, 2100)
hemisphere_split_results <- fetchvars(core, 2000:2100, result_vars, scenario = "Hemisphere_split")

#Plotting the results 
plot_data <- rbind(base_results, hemisphere_split_results)
plot_data$variable <- factor(plot_data$variable, result_vars)

ggplot(plot_data) +
  aes(x = year, y =value, color = scenario) +
  geom_line()+
  facet_wrap(vars(variable), scales = "free_y",
             labeller = labeller(variable = c("Ca" = "CO2 Concentration (ppmv CO2)",
                                              "Ftot" = "Total Radiative Forcing (W/m2)",
                                              "Tgav" = "Global Mean Temperature (degrees C)",
                                              "veg_c" = "Vegetation Carbon (Pg C)",
                                              "soil_c" = "Soil Carbon (Pg C)",
                                              "detritus_c" = "Detritus Carbon (Pg C)")))+
  theme_bw()

#Examining the biome specific pools
hemisphere_details <- fetchvars(core, 2000:2100, 
                             c(VEG_C("Northern-Hemisphere"), VEG_C("Southern-Hemisphere"),
                               DETRITUS_C("Northern-Hemisphere"), DETRITUS_C("Southern-Hemisphere"),
                               SOIL_C("Northern-Hemisphere"), SOIL_C("Southern-Hemisphere")),
                             scenario = "Hemisphere_split")
head(hemisphere_details)

#Splitting variable column into components
variable_split <- strsplit(hemisphere_details$variable,".", fixed = TRUE)
hemisphere_details$biome <- factor(vapply(variable_split, "[[", character(1),1),
                                c("Northern-Hemisphere", "Southern-Hemisphere"))
hemisphere_details$variable <- vapply(variable_split, "[[", character(1), 2)

#Plotting low and high latitude
ggplot(hemisphere_details)+
  aes(x = year, y=value, color=biome)+
  geom_line()+
  facet_wrap(vars(variable), scales = "free_y",
             labeller = labeller(variable = c("veg_c" = "Vegetation Carbon (Pg C)",
                                              "soil_c" = "Soil Carbon (Pg C)",
                                              "detritus_c" = "Detritus Carbon (Pg C)")))+
  theme_bw()




