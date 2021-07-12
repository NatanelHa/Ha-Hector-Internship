#Importing libraries
library(hector)
library(ggplot2)

#Running a reference case
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(rcp45, suppresslogging  = TRUE)
invisible(run(core, 2100))
result_vars <- c(ATMOSPHERIC_CO2(), RF_TOTAL(), GLOBAL_TEMP(),
                 VEG_C(), SOIL_C(), DETRITUS_C())
reference_results <- fetchvars(core, 2000:2100, result_vars, scenario = "reference")

#Redoing run with high-latitude warming
invisible(reset(core))
split_biome(core, "global", c("low-latitude", "high-latitude"),
            fveg_c = c(0.9, 0.1),
            fdetritus_c = c(0.9, 0.1),
            fsoil_c = c(0.6, 0.4),
            warmingfactor = c(1, 2.5))
invisible(run(core, 2100))
warming_results <- fetchvars(core, 2000:2100, result_vars, scenario = "warming")

#Plotting the results 
plot_data <- rbind(reference_results, warming_results)
plot_data$variable <- factor(plot_data$variable, result_vars)
ggplot(plot_data) +
  aes(x = year, y =value, color = scenario) +
  geom_line()+
  facet_wrap(vars(variable), scales = "free_y")+
  theme_bw()

#Examining the biome specific pools
warming_details <- fetchvars(core, 2000:2100, 
                             c(VEG_C("low-latitude"), VEG_C("high-latitude"),
                               DETRITUS_C("low-latitude"), DETRITUS_C("high-latitude"),
                               SOIL_C("low-latitude"), SOIL_C("high-latitude")),
                             scenario = "warming")
head(warming_details)

#Splitting variable columb into components
variable_split <- strsplit(warming_details$variable,".", fixed = TRUE)
warming_details$biome <- factor(vapply(variable_split, "[[", character(1),1),
                                c("low-latitude", "high-latitude"))
warming_details$variable <- vapply(variable_split, "[[", character(1), 2)

#Plotting low and high latitude
ggplot(warming_details)+
  aes(x = year, y=value, color=biome)+
  geom_line()+
  facet_wrap(vars(variable), scales = "free_y")+
  theme_bw()

#C3 and C4 Plants
#Creating newcore
core <- newcore(rcp45, suppresslogging = TRUE)

#Splitting into biomes
split_biome(core, "global", c("C3", "C4"),
            fveg_c = c(0.9, 0.1),
            beta = c(0.36, 0.18))

#Running and storing results
invisible(run(core, 2100))
c4_results <- fetchvars(core, 2000:2100, result_vars, scenario = "c3 vs c4")
c4_results_biome <- fetchvars(core, 2000:2100,
                              c(VEG_C("C3"), VEG_C("C4"),
                                DETRITUS_C("C3"), DETRITUS_C("C4"),
                                SOIL_C("C3"), SOIL_C("C4")),
                              scenario = "c3 vs c4")

#Splitting variable based on C3 vs C4
variable_split <- strsplit(c4_results_biome$variable, ".", fixed = TRUE)
c4_results_biome$biome <- vapply(variable_split, "[[",character(1),1)
c4_results_biome$variable <- vapply(variable_split, "[[", character(1), 2)

c4_results$biome <- "global"
reference_results$biome <- "global"

#Plotting data
plot_data <- rbind(reference_results, c4_results, c4_results_biome)
plot_data$variable <- factor(plot_data$variable, result_vars)
ggplot(plot_data)+
  aes(x = year, y = value, linetype = scenario, color = biome)+
  geom_line()+
  facet_wrap(vars(variable), scales = "free_y")+
  theme_bw()


  


