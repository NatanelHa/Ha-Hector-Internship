# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)

# tracking plot function
tracking_plot <- function(ini_file, start, stop, graph_type, pool, title) {
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
    mutate(source_amount = source_fraction * pool_value)

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

  if (pool != "all") {
    td %>%
      filter(pool_name == pool) ->
    td
  }

  # Graph of Fractional Portion
  if (graph_type == "fraction") {
    areaGraph <- ggplot(td) +
      aes(x = year, y = source_fraction, fill = source_name) +
      geom_bar(stat = "identity") +
      facet_wrap(~pool_namef,
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
      geom_col(width = 1) +
      guides(fill = guide_legend(title = "Carbon Pools")) +
      ylab("Source Fraction") +
      ggtitle(title) +
      xlab("Year")

    # Graph of the Amount of Carbon
  } else {
    areaGraph <- ggplot(td) +
      aes(x = year, y = source_amount, fill = source_name) +
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
      geom_col(width = 1) +
      guides(fill = guide_legend(title = "Carbon Pools")) +
      ylab("Source Amount (Pg C)") +
      ggtitle(title) +
      xlab("Year")
  }

  # Shutdown Core
  shutdown(core)

  areaGraph
}

# Getting plots for all RCPs
tracking_plot(
  system.file("input/hector_rcp26.ini", package = "hector"), 1750, 2300,
  "fraction", "all", "RCP 2.6"
)
tracking_plot(
  system.file("input/hector_rcp26.ini", package = "hector"), 1750, 2300,
  "amount", "all", "RCP 2.6"
)

tracking_plot(
  system.file("input/hector_rcp26.ini", package = "hector"), 2000, 2300,
  "fraction", "earth_c", "RCP 2.6"
)
tracking_plot(
  system.file("input/hector_rcp26.ini", package = "hector"), 2000, 2300,
  "amount", "earth_c", "RCP 2.6"
)

