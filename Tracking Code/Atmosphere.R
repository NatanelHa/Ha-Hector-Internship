# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

# Get tracking results
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

#Atmospheric Reslts
atmos_results <- function(ini_file, start, stop, scenario) {
  results <- tracking_results(ini_file, start, stop, scenario)
  
  results %>%
    filter(pool_name == "atmos_c") %>%
    select(-pool_value, -source_fraction) %>%
    pivot_wider(names_from = "source_name", values_from = "source_amount") %>%
    mutate(detritus_c_global = detritus_c_global - first(detritus_c_global))%>%
    mutate(veg_c_global = veg_c_global - first(veg_c_global))%>%
    mutate(soil_c_global = soil_c_global - first(soil_c_global))%>%
    mutate(earth_c = earth_c - first(earth_c))%>%
    mutate(atmos_c = atmos_c - first(atmos_c))%>%
    pivot_longer(5:13, names_to = "source_name", values_to = "source_amount") ->
    results
  return(results)
}

#Split Function
atmos_results_split <- function(ini_file, start, stop, scenario) {
  results <- tracking_results(ini_file, start, stop, scenario)
  
  core <- newcore(ini_file)
  run(core)
  
  ffi_emissions <- fetchvars(core, 2020:2100, FFI_EMISSIONS())
  ffi_emissions$value <- cumsum(ffi_emissions$value)
  
  ffi_emissions %>%
    select(value, year) %>%
    rename("fossil_fuels" = value) ->
    ffi_emissions
  
  results %>%
    filter(pool_name == "atmos_c") %>%
    right_join(ffi_emissions) %>%
    select(-pool_value, -source_fraction) %>%
    pivot_wider(names_from = "source_name", values_from = "source_amount") %>%
    mutate(earth_c = earth_c - fossil_fuels) %>%
    mutate(detritus_c_global = detritus_c_global - first(detritus_c_global))%>%
    mutate(veg_c_global = veg_c_global - first(veg_c_global))%>%
    mutate(soil_c_global = soil_c_global - first(soil_c_global))%>%
    mutate(earth_c = earth_c - first(earth_c))%>%
    mutate(atmos_c = atmos_c - first(atmos_c))%>%
    pivot_longer(5:14, names_to = "source_name", values_to = "source_amount") ->
    results
  return(results)
}

#Plot Maker
atmos_plot_maker <- function(results, start, type, subtitleName, facet) {
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
    ylab(ylabel) +
    xlab("Year")
  
  if (facet == FALSE) {
    areaGraph <- areaGraph +
      ggtitle(paste("Atmospheric Pool: Net Change from", start),
              subtitle = subtitleName
      )
  } else {
    areaGraph <- areaGraph +
      ggtitle(paste("Atmospheric Pool: Net Change from", start)) +
      facet_wrap(~scenario)
  }
  return(areaGraph)
}

#Unsplit
atmos_plot <- function(ini_file, start, stop, type, scenario) {
  results <- atmos_results(ini_file, start, stop, scenario)
  plot <- atmos_plot_maker(results, start, type, scenario, FALSE)
  return(plot)
}

#Split
atmos_plot_split <- function(ini_file, start, stop, scenario) {
  results <- atmos_results_split(ini_file, start, stop, scenario)
  plot <- atmos_plot_maker(results, start, "amount", scenario, FALSE)
  return(plot)
}

# Getting plots for all RCPs
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

#Unsplit
atmos_combined <- rbind(
  atmos_results(ini_file_26_SSP1, 2020, 2100, "SSP1 RCP 2.6"),
  atmos_results(ini_file_19_SSP1, 2020, 2100, "SSP1 RCP 1.9"),
  atmos_results(ini_file_26_SSP5, 2020, 2100, "SSP5 RCP 2.6"),
  atmos_results(ini_file_19_SSP5, 2020, 2100, "SSP5 RCP 1.9")
)

#Split
atmos_combined_split <- rbind(
  atmos_results_split(ini_file_26_SSP1, 2020, 2100, "SSP1 RCP 2.6"),
  atmos_results_split(ini_file_19_SSP1, 2020, 2100, "SSP1 RCP 1.9"),
  atmos_results_split(ini_file_26_SSP5, 2020, 2100, "SSP5 RCP 2.6"),
  atmos_results_split(ini_file_19_SSP5, 2020, 2100, "SSP5 RCP 1.9")
)

# RCP 2.6 SSP1
atmos_plot_split(ini_file_26_SSP1, 2020, 2100, "RCP 2.6 SSP1")

# RCP 2.6 SSP6
atmos_plot_split(ini_file_26_SSP5, 2020, 2100, "RCP 2.6 SSP5")


# RCP 1.9 SSP1
atmos_plot_split(ini_file_19_SSP1, 2020, 2100, "RCP 1.9 SSP1")

# RCP 1.9 SSP5
atmos_plot_split(ini_file_19_SSP5, 2020, 2100, "RCP 1.9 SSP5")

# Facetted
atmos_plot_maker(atmos_combined_split, 2020, "amount", "NA", TRUE)

# Stacked Bar Chart
atmos_combined_split %>%
  filter(year > 2030) %>%
  filter(year %% 25 == 0) ->
  barData

barGraph <- ggplot(barData) +
  aes(x = scenario, y = source_amount, fill = source_name) +
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
  ylab("Source Amount (Pg C)") +
  ggtitle(paste("Atmosphere Pool: Net Change from 2020")) +
  xlab("Scenario")
barGraph
