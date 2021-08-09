# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

# Function to get result
atmos_results <- function(ini_file, start, stop, scenario) {
  # Establish core
  core <- newcore(ini_file, suppresslogging = TRUE)
  
  # Run core
  run(core)
  
  # Get Results
  results <- get_tracking_data(core)
  
  # Calculating source anount
  results %>%
    filter(pool_name == "atmos_c") %>%
    filter(year >= start) %>%
    filter(year <= stop) %>%
    mutate(source_amount = source_fraction * pool_value) ->
    results

  results %>%
    select(-pool_value, -source_fraction) %>%
    pivot_wider(names_from = "source_name", values_from = "source_amount") %>%
    mutate(detritus_c_global = detritus_c_global - first(detritus_c_global))%>%
    mutate(veg_c_global = veg_c_global - first(veg_c_global))%>%
    mutate(soil_c_global = soil_c_global - first(soil_c_global))%>%
    mutate(earth_c = earth_c - first(earth_c))%>%
    mutate(atmos_c = atmos_c - first(atmos_c))%>%
    pivot_longer(4:12, names_to = "source_name", values_to = "source_amount") %>%
    mutate(title = scenario) ->
    results
  return(results)
}

atmos_results_split <- function(ini_file, start, stop, scenario) {
  # Establish core
  core <- newcore(ini_file, suppresslogging = TRUE)
  
  # Run core
  run(core)
  
  # Get Results
  results <- get_tracking_data(core)
  
  # Calculating source anount
  results %>%
    filter(pool_name == "atmos_c") %>%
    filter(year >= start) %>%
    filter(year <= stop) %>%
    mutate(source_amount = source_fraction * pool_value) ->
    results
  
  # Get ffi emissions
  ffi_emissions <- fetchvars(core, 2020:2100, FFI_EMISSIONS())
  ffi_emissions$value <- cumsum(ffi_emissions$value)
  
  ffi_emissions %>%
    select(value, year) %>%
    rename("fossil_fuels" = value) ->
    ffi_emissions
  
  results %>%
    right_join(ffi_emissions) %>%
    select(-pool_value, -source_fraction) %>%
    pivot_wider(names_from = "source_name", values_from = "source_amount") %>%
    mutate(earth_c = earth_c - fossil_fuels) %>%
    mutate(detritus_c_global = detritus_c_global - first(detritus_c_global))%>%
    mutate(veg_c_global = veg_c_global - first(veg_c_global))%>%
    mutate(soil_c_global = soil_c_global - first(soil_c_global))%>%
    mutate(earth_c = earth_c - first(earth_c))%>%
    mutate(atmos_c = atmos_c - first(atmos_c))%>%
    pivot_longer(4:13, names_to = "source_name", values_to = "source_amount") %>%
    mutate(title = scenario) ->
    results
  return(results)
}

path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

combined <- rbind(
  atmos_results(ini_file_26_SSP1, 2020, 2100, "SSP1 RCP 2.6"),
  atmos_results(ini_file_19_SSP1, 2020, 2100, "SSP1 RCP 1.9"),
  atmos_results(ini_file_26_SSP5, 2020, 2100, "SSP5 RCP 2.6"),
  atmos_results(ini_file_19_SSP5, 2020, 2100, "SSP5 RCP 1.9")
)

combined_split <- rbind(
  atmos_results_split(ini_file_26_SSP1, 2020, 2100, "SSP1 RCP 2.6"),
  atmos_results_split(ini_file_19_SSP1, 2020, 2100, "SSP1 RCP 1.9"),
  atmos_results_split(ini_file_26_SSP5, 2020, 2100, "SSP5 RCP 2.6"),
  atmos_results_split(ini_file_19_SSP5, 2020, 2100, "SSP5 RCP 1.9")
)

areaGraph <- ggplot(combined) +
  aes(x = year, y = source_amount, fill = source_name) +
  geom_area() +
  facet_wrap(~title) +
  scale_fill_manual(
    limits = c(
      "detritus_c_global",
      "veg_c_global",
      "soil_c_global",
      "earth_c",
      "atmos_c"
    ),
    labels = c(
      "Detritus",
      "Vegetation",
      "Soil",
      "Earth",
      "Atmosphere"
    ),
    values = c(
      "#DDCC77",
      "#999933",
      "#44AA99",
      "#117733",
      "#DDDDDD"
    )
  ) +
  guides(fill = guide_legend(title = "Carbon Pools")) +
  ylab("Source Amount (Pg C)") +
  ggtitle(paste("Atmospheric Pool")) +
  xlab("Year")
areaGraph

combined %>%
  filter(year > 2030) %>%
  filter(year %% 25 == 0) ->
  barData

barGraph <- ggplot(barData) +
  aes(x = title, y = source_amount, fill = source_name) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  scale_fill_manual(
    limits = c(
      "detritus_c_global",
      "veg_c_global",
      "soil_c_global",
      "earth_c",
      "atmos_c"
    ),
    labels = c(
      "Detritus",
      "Vegetation",
      "Soil",
      "Earth",
      "Atmosphere"
    ),
    values = c(
      "#DDCC77",
      "#999933",
      "#44AA99",
      "#117733",
      "#DDDDDD"
    )
  ) +
  guides(fill = guide_legend(title = "Carbon Pools")) +
  ylab("Source Amount (Pg C)") +
  ggtitle(paste("Atmospheric Pool")) +
  xlab("Scenario")
barGraph


areaGraph <- ggplot(combined_split) +
  aes(x = year, y = source_amount, fill = source_name) +
  geom_area() +
  facet_wrap(~title) +
  scale_fill_manual(
    limits = c(
      "detritus_c_global",
      "veg_c_global",
      "soil_c_global",
      "earth_c",
      "fossil_fuels",
      "atmos_c"
    ),
    labels = c(
      "Detritus",
      "Vegetation",
      "Soil",
      "Earth",
      "Fossil Fuels",
      "Atmosphere"
    ),
    values = c(
      "#DDCC77",
      "#999933",
      "#44AA99",
      "#117733",
      "#882255",
      "#DDDDDD"
    )
  ) +
  guides(fill = guide_legend(title = "Carbon Pools")) +
  ylab("Source Amount (Pg C)") +
  ggtitle(paste("Atmospheric Pool")) +
  xlab("Year")
areaGraph

combined_split %>%
  filter(year > 2030) %>%
  filter(year %% 25 == 0) ->
  barData

barGraph <- ggplot(barData) +
  aes(x = title, y = source_amount, fill = source_name) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  scale_fill_manual(
    limits = c(
      "detritus_c_global",
      "veg_c_global",
      "soil_c_global",
      "earth_c",
      "fossil_fuels",
      "atmos_c"
    ),
    labels = c(
      "Detritus",
      "Vegetation",
      "Soil",
      "Earth",
      "Fossil Fuels",
      "Atmosphere"
    ),
    values = c(
      "#DDCC77",
      "#999933",
      "#44AA99",
      "#117733",
      "#882255",
      "#DDDDDD"
    )
  ) +
  guides(fill = guide_legend(title = "Carbon Pools")) +
  ylab("Source Amount (Pg C)") +
  ggtitle(paste("Atmospheric Pool")) +
  xlab("Scenario")
barGraph
