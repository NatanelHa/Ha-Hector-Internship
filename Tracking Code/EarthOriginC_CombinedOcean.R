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

earth_origin_plot <- function(ini_file, start, stop, type, scenarioName) {
  # Filtering for earth origin c
  td <- tracking_results(ini_file, 2020, 2100, scenarioName)

  td %>%
    filter(source_name == "earth_c") %>%
    filter(pool_name == "earth_c" |
      pool_name == "soil_c_global" |
      pool_name == "deep" |
      pool_name == "HL" |
      pool_name == "intermediate" |
      pool_name == "LL") %>%
    select(-source_fraction, -pool_value) %>%
    pivot_wider(names_from = "pool_name", values_from = "source_amount") %>%
    mutate(ocean = deep + HL + intermediate + LL) %>%
    select(-deep, -HL, -intermediate, -LL) %>%
    pivot_longer(5:7, names_to = "pool_name", values_to = "source_amount") ->
  td

  title <- ""
  ytitle <- ""

  if (type == "total") {
    title <- "Total Carbon with Earth Pool Origin"
    ytitle <- "Source Amount (Pg C)"

    td <- td %>%
      rename(yval = source_amount)
  } else {
    title <- "Total Uptake Carbon with Earth Pool Origin"
    ytitle <- "Source Amount (Pg C)"

    core <- newcore(ini_file)
    run(core)

    ffi_emissions <- fetchvars(core, 2020:2100, FFI_EMISSIONS())
    ffi_emissions$value <- cumsum(ffi_emissions$value)

    ffi_emissions %>%
      select(value, year) %>%
      rename("fossil_fuels" = value) ->
    ffi_emissions

    td %>%
      right_join(ffi_emissions) %>%
      pivot_wider(names_from = "pool_name", values_from = "source_amount") %>%
      mutate(earth_c = earth_c - first(earth_c) + fossil_fuels) %>%
      mutate(ocean = ocean - first(ocean)) %>%
      mutate(soil_c_global = soil_c_global - first(soil_c_global)) %>%
      select(-fossil_fuels) %>%
      pivot_longer(5:7, names_to = "pool_name", values_to = "source_amount") ->
    td

    if (type == "rate") {
      td %>%
        group_by(pool_name, source_name) %>%
        mutate(differ = source_amount - lag(source_amount)) %>%
        filter(!is.na(differ)) ->
      td

      title <- "Rate of Uptake of Carbon with Earth Pool Origin"
      ytitle <- "Source Amount (Pg C/Yr)"
      td <- td %>%
        rename(yval = differ)
    } else {
      td <- td %>%
        rename(yval = source_amount)
    }
  }


  graph <- ggplot(td) +
    aes(x = year, y = yval, color = pool_name) +
    geom_line(size = 1.25) +
    scale_color_manual(
      limits = c(
        "earth_c",
        "soil_c_global",
        "ocean"
      ),
      labels = c(
        "Earth",
        "Soil",
        "Ocean"
      ),
      values = c(
        "#117733",
        "#44AA99",
        "#332288"
      )
    ) +
    guides(color = guide_legend(title = "Carbon Pools")) +
    ylab(ytitle) +
    ggtitle(title,
      subtitle = scenarioName
    ) +
    xlab("Year")
  return(graph)
}



path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

earth_origin_plot(ini_file_26_SSP1, 2020, 2100, "uptake", "RCP 2.6 SSP1")
earth_origin_plot(ini_file_26_SSP1, 2020, 2100, "rate", "RCP 2.6 SSP1")

earth_origin_plot(ini_file_19_SSP1, 2020, 2100, "uptake", "RCP 1.9 SSP1")
earth_origin_plot(ini_file_19_SSP1, 2020, 2100, "rate", "RCP 1.9 SSP1")

earth_origin_plot(ini_file_26_SSP5, 2020, 2100, "uptake", "RCP 2.6 SSP5")
earth_origin_plot(ini_file_26_SSP5, 2020, 2100, "rate", "RCP 2.6 SSP5")

earth_origin_plot(ini_file_19_SSP5, 2020, 2100, "uptake", "RCP 1.9 SSP5")
earth_origin_plot(ini_file_19_SSP5, 2020, 2100, "rate", "RCP 1.9 SSP5")
