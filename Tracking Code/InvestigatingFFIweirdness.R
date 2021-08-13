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

path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")


td <- tracking_results(ini_file_26_SSP1, 2020, 2100, "")
core <- newcore(ini_file_26_SSP1)
run(core)

emissions <- fetchvars(core, 2020:2100, c(FFI_EMISSIONS(), DACCS_UPTAKE()))

emissions %>%
  pivot_wider(names_from = "variable", values_from = "value") ->
emissions

emissions$ffi_emissions <- cumsum(emissions$ffi_emissions)
emissions$daccs_uptake <- cumsum(emissions$daccs_uptake)



td %>%
  filter(pool_name == "earth_c") ->
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

results %>%
  group_by(year, scenario) %>%
  mutate(sum = sum(source_amount)) ->
results

emissions %>%
  select(year, daccs_uptake)->
  emissions

results %>%
  right_join(emissions)%>%
  select(-pool_value, -source_fraction) %>%
  pivot_wider(names_from = "source_name", values_from = "source_amount")%>%
  mutate(fossil_fuels = earth_c)%>%
  mutate(earth_c = daccs_uptake - sum + earth_c)%>%
  mutate(fossil_fuels = fossil_fuels - earth_c)%>%
  select(-sum, -daccs_uptake)%>%
  pivot_longer(5:15, names_to = "source_name", values_to = "source_amount")->
  results

earth_plot_maker <- function(results, start, type, subtitleName, facet) {
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
    xlab("Year")
  
  if (facet == FALSE) {
    areaGraph <- areaGraph +
      ggtitle(paste("Earth Pool: Net Change from", start),
              subtitle = subtitleName
      )
  } else {
    areaGraph <- areaGraph +
      ggtitle(paste("Earth Pool: Net Change from", start)) +
      facet_wrap(~scenario)
  }
  
  return(areaGraph)
}

results %>%
  #filter(source_name != "earth_c") %>%
  filter(source_name != "ffi_emissions")->
  total_results_split

total_results_split %>%
  group_by(year, scenario)%>%
  mutate(sum = sum(source_amount))->
  uptake_fraction 

uptake_fraction %>%
  mutate(source_fraction = source_amount/sum)->
  uptake_fraction

earth_plot_maker(uptake_fraction, 2020, "fraction", "Fraction of Uptake", TRUE)