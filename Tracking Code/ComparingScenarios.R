# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)

# Function to get results
get_results <- function(ini_file, start, stop, name) {
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
    mutate(unmod_source_amount = source_fraction * pool_value) ->
  results

  # Finding earth_c values in start
  # The year negative emissions starts
  earth_start <-
    results %>%
    filter(source_name == "earth_c")

  earth_start_carbon <-
    first(earth_start$unmod_source_amount)

  earth_start_fraction <-
    first(earth_start$source_fraction)

  # Calculating difference from start
  results %>%
    mutate(source_amount = unmod_source_amount - earth_start_carbon * (source_name == "earth_c")) %>%
    mutate(source_fraction = source_fraction - earth_start_fraction * (source_name == "earth_c")) %>%
    mutate(scenario = name) %>%
    filter(source_name == "earth_c" | source_name == "detritus_c_global" 
           | source_name == "atmos_c" |source_name == "soil_c_global" 
           | source_name == "veg_c_global")%>%
    group_by(pool_name, source_name) %>%
    mutate(differ = unmod_source_amount - lag(unmod_source_amount)) ->
  results
  return(results)
}

path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_26_SSP1 <- paste(path, "jay_SSP1.ini", sep = "")
ini_file_19_SSP1 <- paste(path, "jay_19_SSP1.ini", sep = "")
ini_file_26_SSP5 <- paste(path, "jay_SSP5.ini", sep = "")
ini_file_19_SSP5 <- paste(path, "jay_19_SSP5.ini", sep = "")

total_results <-
  rbind(
    get_results(ini_file_26_SSP1, 2020, 2100, "RCP 2.6 SSP1"),
    get_results(ini_file_26_SSP5, 2020, 2100, "RCP 2.6 SSP5"),
    get_results(ini_file_19_SSP1, 2020, 2100, "RCP 1.9 SSP1"),
    get_results(ini_file_19_SSP5, 2020, 2100, "RCP 1.9 SSP5")
  )

amount <- ggplot(total_results) +
  aes(x = year, y = source_amount, fill = source_name) +
  geom_area() +
  facet_wrap(~scenario) +
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
  ggtitle(paste("Earth Pool: Net Change from 2020")) +
  xlab("Year")
amount

# Plotting in area graph of fraction
frac <- ggplot(total_results) +
  aes(x = year, y = source_fraction, fill = source_name) +
  geom_area() +
  facet_wrap(~scenario) +
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
  ggtitle(paste("Earth Pool: Net Change from 2020")) +
  xlab("Year")
frac

total_results %>%
  filter(year == 2100) ->
total_results_2100

# As Amount
amountBar <- ggplot(total_results_2100) +
  aes(x = source_name, y = source_amount, fill = source_name) +
  geom_bar(stat = "identity") +
  facet_wrap(~scenario) +
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
  ggtitle(paste("Earth Pool: Net Change from 2020")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
amountBar

# Plotting in area graph of fraction
fracBar <- ggplot(total_results_2100) +
  aes(x = source_name, y = source_fraction, fill = source_name) +
  geom_bar(stat = "identity") +
  facet_wrap(~scenario) +
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
  ggtitle(paste("Earth Pool: Net Change from 2020")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
fracBar

#Difference
dif <- ggplot(total_results) +
  aes(x = year, y = differ, fill = source_name) +
  geom_area() +
  facet_wrap(~scenario)+
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
  ylab("Change from Previous Year (Pg C)") +
  ggtitle("Difference in Earth Pool") +
  xlab("Year")

dif
