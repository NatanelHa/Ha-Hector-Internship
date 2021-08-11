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

#Getting Files
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

#Filtering for earth origin c
td <- tracking_results(ini_file_19_SSP5, 2020, 2100, "SSP5 RCP 1.9")

td %>%
  filter(source_name == "earth_c") %>%
  filter(pool_name == "earth_c" | 
           pool_name == "deep" |
           pool_name == "soil_c_global") %>%
  select(-source_fraction, -pool_value)  ->
  td

# Set Theme
theme_set(theme_minimal())

areaGraph <- ggplot(td) +
  aes(x = year, y = source_amount, color = pool_name) +
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
  ylab("Source Amount (Pg C)") +
  ggtitle("Total Carbon with Earth Pool Origin") +
  xlab("Year")
return(areaGraph)

core <- newcore(ini_file_19_SSP5)
run(core)

ffi_emissions <- fetchvars(core, 2020:2100, FFI_EMISSIONS())
ffi_emissions$value <- cumsum(ffi_emissions$value)

ffi_emissions %>%
  select(value, year) %>%
  rename("fossil_fuels" = value) ->
  ffi_emissions


td %>%
  right_join(ffi_emissions)%>%
  pivot_wider(names_from = "pool_name", values_from = "source_amount") %>%
  mutate(earth_c = earth_c - first(earth_c) + fossil_fuels)%>%
  mutate(deep = deep - first(deep))%>%
  mutate(soil_c_global = soil_c_global - first(soil_c_global)) %>%
  select(-fossil_fuels)%>%
  pivot_longer(5:7, names_to = "pool_name", values_to = "source_amount") ->
  td

areaGraph <- ggplot(td) +
  aes(x = year, y = source_amount, color = pool_name) +
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
  ylab("Source Amount (Pg C)") +
  ggtitle("Cumulative Uptake of Carbon with Earth Pool Origin") +
  xlab("Year")
return(areaGraph)

td %>%
  group_by(pool_name, source_name) %>%
  mutate(differ = source_amount - lag(source_amount)) %>%
  filter(!is.na(differ)) ->
  td

areaGraph <- ggplot(td) +
  aes(x = year, y = differ, color = pool_name) +
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
  ylab("Source Amount (Pg C/Yr)") +
  ggtitle("Rate of Uptake of Carbon with Earth Pool Origin") +
  xlab("Year")
return(areaGraph)
