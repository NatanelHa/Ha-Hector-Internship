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

# Tracking plot Function
difference_calc <- function(ini_file, start, stop, title) {
  td <- tracking_results(ini_file, start, stop, title)
  
  td %>%
    group_by(pool_name, source_name) %>%
    mutate(differ = source_amount - lag(source_amount)) ->
  td
  
  # Filtering out years and NA
  td %>%
    filter(year >= start) %>%
    filter(year <= stop) %>%
    filter(!is.na(differ)) ->
    td
}

difference_plot_maker <- function(td, pool, title, facet) {
  # Creating better labels
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

  # A plot of all pools
  if (pool != "all") {
    td %>%
      filter(pool_name == pool) ->
    td
    lineSize <- 1.75
  }
  else {
    lineSize <- 1
  }
  
  lineGraph <- ggplot(td) +
    aes(x = year, y = differ, color = source_name) +
    geom_line(size = lineSize) +
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
    scale_color_manual(
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
    guides(color = guide_legend(title = "Carbon Pools")) +
    ylab("Change from Previous Year (Pg C/Yr)") +
    xlab("Year")
  
  if (facet == FALSE) {
    lineGraph <- lineGraph +
      ggtitle(title)
  } else {
    lineGraph <- lineGraph  +
      facet_wrap(~scenario)
  }
  return(lineGraph)
}

difference_plot <- function(ini_file, start, stop, pool, title){
  td <- difference_calc(ini_file, start, stop, title)
  graph <- difference_plot_maker(td, pool, title, FALSE)
  return(graph)
}

# Running RCP 2.6 and RCP 4.5
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

# RCP 2.6 SSP1
difference_plot(ini_file_26_SSP1, 2005, 2100, "all", "RCP 2.6 SSP1")
difference_plot(ini_file_26_SSP1, 2005, 2100, "earth_c", "RCP 2.6 SSP1")

# RCP 1.9 SSP1
difference_plot(ini_file_19_SSP1, 2005, 2100, "all", "RCP 1.9 SSP1")
difference_plot(ini_file_19_SSP1, 2005, 2100, "earth_c", "RCP 1.9 SSP1")

# RCP 2.6 SSP5
difference_plot(ini_file_26_SSP5, 2005, 2100, "all", "RCP 2.6 SSP5")
difference_plot(ini_file_26_SSP5, 2005, 2100, "earth_c", "RCP 2.6 SSP5")

# RCP 1.9 SSP5
difference_plot(ini_file_19_SSP5, 2005, 2100, "all", "RCP 1.9 SSP5")
difference_plot(ini_file_19_SSP5, 2005, 2100, "earth_c", "RCP 1.9 SSP5")

total_results <-
  rbind(
    difference_calc(ini_file_26_SSP1, 2020, 2100, "SSP1 \nRCP 2.6"),
    difference_calc(ini_file_26_SSP5, 2020, 2100, "SSP5 \nRCP 2.6"),
    difference_calc(ini_file_19_SSP1, 2020, 2100, "SSP1 \nRCP 1.9"),
    difference_calc(ini_file_19_SSP5, 2020, 2100, "SSP5 \nRCP 1.9 ")
  )

difference_plot_maker(total_results, "earth_c", "NA", TRUE)
difference_plot_maker(total_results, "atmos_c", "NA", TRUE)

