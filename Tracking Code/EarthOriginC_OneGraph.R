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

earth_origin_plot_total <- function(td, type) {
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
    
    core26_1 <- newcore(ini_file_26_SSP1)
    core26_5 <- newcore(ini_file_26_SSP5)
    core19_1 <- newcore(ini_file_19_SSP1)
    core19_5 <- newcore(ini_file_19_SSP5)
    run(core26_1)
    run(core26_5)
    run(core19_1)
    run(core19_5)
    
    ffi_emissions26_1 <- fetchvars(core26_1, 2020:2100, FFI_EMISSIONS(), 
                                   scenario = "SSP1 \nRCP 2.6")
    ffi_emissions26_1$value <- cumsum(ffi_emissions26_1$value)
    
    ffi_emissions26_5 <- fetchvars(core26_5, 2020:2100, FFI_EMISSIONS(), 
                                   scenario = "SSP5 \nRCP 2.6")
    ffi_emissions26_5$value <- cumsum(ffi_emissions26_5$value)
    
    ffi_emissions19_1 <- fetchvars(core19_1, 2020:2100, FFI_EMISSIONS(), 
                                   scenario = "SSP1 \nRCP 1.9")
    ffi_emissions19_1$value <- cumsum(ffi_emissions19_1$value)
    
    ffi_emissions19_5 <- fetchvars(core19_5, 2020:2100, FFI_EMISSIONS(), 
                                   scenario = "SSP5 \nRCP 1.9")
    ffi_emissions19_5$value <- cumsum(ffi_emissions19_5$value)
    
    ffi_emissions <- rbind (
      ffi_emissions26_1,
      ffi_emissions26_5,
      ffi_emissions19_1,
      ffi_emissions19_5
    )
    
    ffi_emissions %>%
      select(value, year, scenario) %>%
      rename("fossil_fuels" = value) ->
      ffi_emissions
    
    ffi_emissionsG1 <<- ffi_emissions
    
    td %>%
      right_join(ffi_emissions) ->
      td
    ffi_emissionsG2 <<- td
    
    td %>%
      pivot_wider(names_from = "pool_name", values_from = "source_amount") %>%
      group_by(scenario)%>%
      mutate(earth_c = earth_c - first(earth_c) + fossil_fuels) %>%
      mutate(ocean = ocean - first(ocean)) %>%
      mutate(soil_c_global = soil_c_global - first(soil_c_global)) %>%
      select(-fossil_fuels) %>%
      pivot_longer(5:7, names_to = "pool_name", values_to = "source_amount") ->
      td
    
    if (type == "rate") {
      td %>%
        group_by(pool_name, source_name, scenario) %>%
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
    aes(x = year, y = yval, color = pool_name, linetype = scenario) +
    geom_line(size = 0.8) +
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
    scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash"))+
    guides(color = guide_legend(title = "Carbon Pools")) +
    ylab(ytitle) +
    ggtitle(title) +
    xlab("Year")
  return(graph)
}



path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

tracking_data <- rbind(
  tracking_results(ini_file_26_SSP1, 2020, 2100, "SSP1 \nRCP 2.6"),
  tracking_results(ini_file_26_SSP5, 2020, 2100, "SSP5 \nRCP 2.6"),
  tracking_results(ini_file_19_SSP1, 2020, 2100, "SSP1 \nRCP 1.9"),
  tracking_results(ini_file_19_SSP5, 2020, 2100, "SSP5 \nRCP 1.9")
)

earth_origin_plot_total(tracking_data, "total")
earth_origin_plot_total(tracking_data, "uptake")
earth_origin_plot_total(tracking_data, "rate")

