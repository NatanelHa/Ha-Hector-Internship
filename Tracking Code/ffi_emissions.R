# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)
# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)

# Tracking results
tracking_results <- function(ini_file, start, stop, scenarioName) {
  # establish core
  core <- newcore(ini_file)
  
  # run core
  run(core)
  
  # Get Results
  results_with_diff <- get_tracking_data(core)
  results_with_diff
  
  # Filter out diff and year
  td <-
    results_with_diff %>%
    filter(pool_name != "Diff") %>%
    filter(year <= stop) %>%
    filter(year >= start) %>%
    mutate(source_amount = source_fraction * pool_value) %>%
    mutate(scenario = scenarioName)
  
  return(td)
}

earth_calc <- function(ini_file, start, stop, scenario) {
  results <- tracking_results(ini_file, start, stop, scenario) %>%
    filter(pool_name == "earth_c")
  
  # Finding earth_c values in start
  # The year negative emissions starts
  earth_start <-
    results %>%
    filter(source_name == "earth_c")
  
  earth_start_carbon <-
    first(earth_start$source_amount)
  
  earth_start_fraction <-
    first(earth_start$source_fraction)
  
  # Calculating difference from start
  results %>%
    mutate(source_amount = source_amount - earth_start_carbon * (source_name == "earth_c")) %>%
    mutate(source_fraction = source_fraction - earth_start_fraction * (source_name == "earth_c")) ->
    results
}

earth_plot_maker <- function(results, start, type, subtitleName, facet) {
  ylabel <- ""
  
  if (type == "fraction") {
    results %>%
      rename(yval = source_fraction) ->
      results
    ylabel <- "Source Fraction"
  } else {
    results %>%
      rename(yval = source_amount) ->
      results
    ylabel <- "Source Amount (Pg C)"
  }
  
  # Plotting in area graph of amount
  areaGraph <- ggplot(results) +
    aes(x = year, y = yval, fill = source_name) +
    geom_area() +
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
        "Earth In",
        "Earth Out",
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
    ylab(ylabel) +
    xlab("Year")
  
  if (facet == FALSE) {
    areaGraph <- areaGraph +
      ggtitle(paste("Earth Pool: Net Change from", start),
              subtitle = subtitleName
      )
  } else {
    areaGraph <- areaGraph +
      ggtitle(paste("Earth Pool: Net Change from", start)) +
      facet_wrap(~scenario)
  }
  
  return(areaGraph)
}

earth_plot <- function(ini_file, start, stop, type, scenario) {
  results <- earth_calc(ini_file, start, stop, scenario)
  plot <- earth_plot_maker(results, start, type, scenario, FALSE)
  return(plot)
}

# Function to get result
ffi_results <- function(ini_file, start, stop, scenario) {
  results <- earth_calc(ini_file, start, stop, scenario)
  
  core <- newcore(ini_file)
  run(core)
  
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
    mutate(earth_c = earth_c + fossil_fuels) %>%
    mutate(fossil_fuels = fossil_fuels * -1) %>%
    pivot_longer(5:14, names_to = "source_name", values_to = "source_amount") ->
  results
}

split_earth <- function(ini_file, start, stop, type, scenario) {
  results <- ffi_results(ini_file, start, stop, scenario)
  plot <- earth_plot_maker(results, start, type, scenario, FALSE)
  return(plot)
}

path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

split_earth(ini_file_26_SSP1, 2020, 2100, "amount", "RCP 2.6 SSP1")
split_earth(ini_file_19_SSP1, 2020, 2100, "amount", "RCP 1.9 SSP1")
split_earth(ini_file_26_SSP5, 2020, 2100, "amount", "RCP 2.6 SSP5")
split_earth(ini_file_19_SSP5, 2020, 2100, "amount", "RCP 1.9 SSP5")

total_results <-
  rbind(
    ffi_results(ini_file_26_SSP1, 2020, 2100, "SSP1 \nRCP 2.6"),
    ffi_results(ini_file_26_SSP5, 2020, 2100, "SSP5 \nRCP 2.6"),
    ffi_results(ini_file_19_SSP1, 2020, 2100, "SSP1 \nRCP 1.9"),
    ffi_results(ini_file_19_SSP5, 2020, 2100, "SSP5 \nRCP 1.9 ")
  )

earth_plot_maker(total_results, 2020, "amount", "NA", TRUE)
