# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

# Tracking plot Function
difference_plot <- function(ini_file, start, stop, pool, title) {
  core <- newcore(ini_file, suppresslogging = TRUE)

  # run core
  run(core)

  # Get td
  td <- get_tracking_data(core)

  # Calculating Source amount and Diff
  td %>%
    filter(pool_name != "Diff") %>%
    mutate(source_amount = source_fraction * pool_value) %>%
    group_by(pool_name, source_name) %>%
    mutate(differ = source_amount - lag(source_amount)) ->
  td

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

  # Filtering out years and NA
  td %>%
    filter(year >= start) %>%
    filter(year <= stop) %>%
    filter(!is.na(differ)) ->
  td

  # A plot of all pools
  if (pool != "all") {
    td %>%
      filter(pool_name == pool) ->
    td
  }
  areaGraph <- ggplot(td) +
    aes(x = year, y = differ, fill = source_name) +
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
    ylab("Change from Previous Year (Pg C)") +
    ggtitle(title) +
    xlab("Year")

  areaGraph
}

# Running RCP 2.6 and RCP 4.5
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_SSP2 <- paste(path, "jay_SSP2.ini", sep = "")
ini_file_SSP4 <- paste(path, "jay_SSP4.ini", sep = "")
ini_file_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")

# SSP1
difference_plot(ini_file_SSP1, 2005, 2100, "all", "RCP 2.6 SSP1")
difference_plot(ini_file_SSP1, 2050, 2100, "earth_c", "RCP 2.6 SSP1")

# SSP2
difference_plot(ini_file_SSP2, 2005, 2100, "all", "RCP 2.6 SSP2")
difference_plot(ini_file_SSP2, 2050, 2100, "earth_c", "RCP 2.6 SSP2")

# SSP4
difference_plot(ini_file_SSP4, 2005, 2100, "all", "RCP 2.6 SSP4")
difference_plot(ini_file_SSP4, 2050, 2100, "earth_c", "RCP 2.6 SSP4")

# SSP5
difference_plot(ini_file_SSP5, 2005, 2100, "all", "RCP 2.6 SSP5")
difference_plot(ini_file_SSP5, 2050, 2100, "earth_c", "RCP 2.6 SSP5")
