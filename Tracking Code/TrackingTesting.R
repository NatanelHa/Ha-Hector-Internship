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

# Simple tracking plot
basic_plot <- function(td, graph_type, pool, title) {
  # Changing Order
  td$pool_namef <- factor(td$pool_name, levels = c(
    "detritus_c_global",
    "veg_c_global",
    "soil_c_global",
    "earth_c",
    "atmos_c",
    "HL",
    "intermediate",
    "LL",
    "deep"
  ))

  # Set Theme
  theme_set(theme_minimal())

  # Filter for Pool
  if (pool != "all") {
    td %>%
      filter(pool_name == pool) ->
    td
  }

  ylabel <- ""

  # Graph of Fractional Portion
  if (graph_type == "fraction") {
    td %>%
      rename(yval = source_fraction) ->
    td
    ylabel <- "Source Fraction"
  } else {
    td %>%
      rename(yval = source_amount) ->
    td
    ylabel <- "Source Amount (Pg C)"
  }
  areaGraph <- ggplot(td) +
    aes(x = year, y = yval, fill = source_name) +
    geom_area() +
    facet_wrap(~pool_namef,
      scales = "free_y",
      labeller = labeller(pool_namef = c(
        "detritus_c_global" = "Detritus",
        "veg_c_global" = "Vegetation",
        "soil_c_global" = "Soil",
        "earth_c" = "Earth",
        "atmos_c" = "Atmosphere",
        "HL" = "High Level Ocean",
        "intermediate" = "Intermediate Ocean",
        "LL" = "Low Level Ocean",
        "deep" = "Deep Ocean"
      ))
    ) +
    scale_fill_manual(
      limits = c(
        "detritus_c_global",
        "veg_c_global",
        "soil_c_global",
        "earth_c",
        "atmos_c",
        "HL",
        "intermediate",
        "LL",
        "deep"
      ),
      labels = c(
        "Detritus",
        "Vegetation",
        "Soil",
        "Earth",
        "Atmosphere",
        "High Level Ocean",
        "Intermediate Ocean",
        "Low Level Ocean",
        "Deep Ocean"
      ),
      values = c(
        "#DDCC77",
        "#999933",
        "#44AA99",
        "#117733",
        "#DDDDDD",
        "#882255",
        "#AA4499",
        "#88CCEE",
        "#332288"
      )
    ) +
    guides(fill = guide_legend(title = "Carbon Pools")) +
    ylab(ylabel) +
    ggtitle(title) +
    xlab("Year")
  return(areaGraph)
}

# tracking plot function
tracking_plot <- function(ini_file, start, stop, graph_type, pool, scenario) {
  td <- tracking_results(ini_file, start, stop, scenario)
  graph <- basic_plot(td, graph_type, pool, scenario)
  return(graph)
}

# Getting plots for all RCPs
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

# RCP 2.6 SSP1
tracking_plot(
  ini_file_26_SSP1, 2005, 2100,
  "fraction", "all", "RCP 2.6 SSP1"
)
tracking_plot(
  ini_file_26_SSP1, 2005, 2100,
  "amount", "all", "RCP 2.6 SSP1"
)

# RCP 1.9 SSP1
tracking_plot(
  ini_file_19_SSP1, 2005, 2100,
  "fraction", "all", "RCP 1.9 SSP1"
)
tracking_plot(
  ini_file_19_SSP1, 2005, 2100,
  "amount", "all", "RCP 1.9 SSP1"
)

# RCP 2.6 SSP5
tracking_plot(
  ini_file_26_SSP5, 2005, 2100,
  "fraction", "all", "RCP 2.6 SSP5"
)
tracking_plot(
  ini_file_26_SSP5, 2005, 2100,
  "amount", "earth_c", "RCP 2.6 SSP5"
)

# RCP 1.9 SSP5
tracking_plot(
  ini_file_19_SSP5, 2005, 2100,
  "fraction", "all", "RCP 1.9 SSP5"
)
tracking_plot(
  ini_file_19_SSP5, 2005, 2100,
  "amount", "all", "RCP 1.9 SSP5"
)