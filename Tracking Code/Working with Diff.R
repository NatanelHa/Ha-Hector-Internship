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
    geom_bar(stat = "identity") +
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
    scale_fill_discrete(labels = c(
      "Detritus",
      "Vegetation",
      "Soil",
      "Earth",
      "Atmosphere",
      "High Level Ocean",
      "Intermediate Ocean",
      "Low Level Ocean",
      "Deep Ocean"
    )) +
    geom_col(width = 1) +
    guides(fill = guide_legend(title = "Carbon Pools")) +
    ylab("Change from Previous Year (Pg C)") +
    ggtitle(title) +
    xlab("Year")

  areaGraph
}

# Running RCP 2.6 and RCP 4.5
rcp26 <- system.file("input/hector_rcp26.ini", package = "hector")
difference_plot(rcp26, 2000, 2100, "all", "RCP 2.6")
difference_plot(rcp26, 2050, 2300, "earth_c", "RCP 2.6")
difference_plot(rcp26, 2050, 2150, "earth_c", "RCP 2.6")
