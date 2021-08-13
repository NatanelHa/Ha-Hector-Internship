# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)
library(tidyr)

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
    mutate(source_fraction = source_fraction - earth_start_fraction * 
             (source_name =="earth_c")) ->
    results
  return(results)
}


ffi_results <- function(ini_file, start, stop, scenario) {
  results <- earth_calc(ini_file, start, stop, scenario)
  
  core <- newcore(ini_file)
  run(core)
  
  daccs <- fetchvars(core, 2020:2100, DACCS_UPTAKE())
  daccs$value <- cumsum(daccs$value)
  
  daccs %>%
    select(value, year) %>%
    rename("daccs_uptake" = value) ->
    daccs
  
  results %>%
    group_by(year, scenario) %>%
    mutate(sum = sum(source_amount)) ->
    results
  
  results %>%
    right_join(daccs)%>%
    select(-pool_value, -source_fraction) %>%
    pivot_wider(names_from = "source_name", values_from = "source_amount")%>%
    mutate(fossil_fuels = earth_c)%>%
    mutate(earth_c = daccs_uptake - sum + earth_c)%>%
    mutate(fossil_fuels = fossil_fuels - earth_c)%>%
    select(-sum, -daccs_uptake)%>%
    pivot_longer(5:14, names_to = "source_name", values_to = "source_amount")->
    results
}

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
    filter(pool_name == "deep" |
      pool_name == "soil_c_global") %>%
    select(-source_fraction, -pool_value) ->
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

    earthData <- ffi_results(ini_file, 2020, 2100, scenarioName)
    earthData %>%
      filter(source_name=="earth_c")%>%
      select(source_amount, year)%>%
      rename(earth_c = "source_amount")->
    earthData
    
    td %>%
      pivot_wider(names_from = "pool_name", values_from = "source_amount") %>%
      right_join(earthData)%>%
      mutate(deep = deep - first(deep)) %>%
      mutate(soil_c_global = soil_c_global - first(soil_c_global)) %>%
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
    }
    else {
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
        "deep"
      ),
      labels = c(
        "Earth",
        "Soil",
        "Deep Ocean"
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
            subtitle = scenarioName) +
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
