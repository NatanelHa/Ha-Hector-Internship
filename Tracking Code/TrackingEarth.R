# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)

earth_plot <- function(ini_file, start, stop, type, scenario) {
  # Establish core
  core <- newcore(ini_file, suppresslogging = TRUE)

  # Run core
  run(core)

  # Get Results
  results <- get_tracking_data(core)

  # Calculating source anount
  results %>%
    filter(pool_name == "earth_c") %>%
    filter(year >= start) %>%
    filter(year <= stop) %>%
    mutate(source_amount = source_fraction * pool_value) ->
  results

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

  if (type == "amount") {
    # Plotting in area graph of amount
    areaGraph <- ggplot(results) +
      aes(x = year, y = source_amount, fill = source_name) +
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
      ylab("Source Amount (Pg C)") +
      ggtitle(paste("Earth Pool: Net Change from", start), 
              subtitle = scenario) +
      xlab("Year")
    return(areaGraph)
  } else {
    # Plotting in area graph of fraction
    areaGraph2 <- ggplot(results) +
      aes(x = year, y = source_fraction, fill = source_name) +
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
      ylab("Source Fraction") +
      ggtitle(paste("Earth Pool: Net Change from", start), 
              subtitle = scenario) +
      xlab("Year")
    return(areaGraph2)
  }
}


# Getting plots for all RCPs
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

# RCP 2.6 SSP1
earth_plot(
  ini_file_26_SSP1, 2020, 2100,
  "fraction",  "RCP 2.6 SSP1"
)
earth_plot(
  ini_file_26_SSP1, 2020, 2100,
  "amount",  "RCP 2.6 SSP1"
)

# RCP 1.9 SSP1
earth_plot(
  ini_file_19_SSP1, 2020, 2100,
  "fraction",  "RCP 1.9 SSP1"
)
earth_plot(
  ini_file_19_SSP1, 2020, 2100,
  "amount",  "RCP 1.9 SSP1"
)

# RCP 2.6 SSP5
earth_plot(
  ini_file_26_SSP5, 2020, 2100,
  "fraction",  "RCP 2.6 SSP5"
)
earth_plot(
  ini_file_26_SSP5, 2020, 2100,
  "amount",  "RCP 2.6 SSP5"
)

# RCP 1.9 SSP5
earth_plot(
  ini_file_19_SSP5, 2020, 2100,
  "fraction",  "RCP 1.9 SSP5"
)
earth_plot(
  ini_file_19_SSP5, 2020, 2100,
  "amount",  "RCP 1.9 SSP5"
)
