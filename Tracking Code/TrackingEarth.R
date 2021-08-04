# Importing libraries
library(hector)
library(ggplot2)
library(dplyr)

path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/New Scenarios/"
ini_file_SSP1 <- paste(path, "jay_SSP1.ini", sep="")
ini_file_SSP2 <- paste(path, "jay_SSP2.ini", sep="")
ini_file_SSP4 <- paste(path, "jay_SSP4.ini", sep="")
ini_file_SSP5 <- paste(path, "jay_SSP5.ini", sep="")

# Establish core
rcp26 <- system.file("input/hector_rcp26.ini", package = "hector")
core <- newcore(ini_file_SSP2, suppresslogging = TRUE)

# Run core
run(core)

# Get Results
results <- get_tracking_data(core)

# Calculating source anount
results %>%
  filter(pool_name == "earth_c") %>%
  filter(year >= 2005) %>%
  filter(year <= 2100) %>%
  mutate(source_amount = source_fraction * pool_value) ->
results

# Finding earth_c values in 2073
# The year negative emissions starts
earth_2073 <-
  results %>%
  filter(source_name == "earth_c")

earth_2073_carbon <-
  first(earth_2073$source_amount)

earth_2073_fraction <-
  first(earth_2073$source_fraction)

# Calculating difference from 2073
results %>%
  mutate(source_amount = source_amount - earth_2073_carbon * (source_name == "earth_c")) %>%
  mutate(source_fraction = source_fraction - earth_2073_fraction * (source_name == "earth_c")) ->
results

# Plotting in area graph of amount
areaGraph <- ggplot(results) +
  aes(x = year, y = source_amount, fill = source_name) +
  geom_area() +
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
  ylab("Source Amount (Pg C)") +
  ggtitle("Earth Pool (Net Change from 2005)") +
  xlab("Year")
areaGraph

# Plotting in area graph of fraction
areaGraph2 <- ggplot(results) +
  aes(x = year, y = source_fraction, fill = source_name) +
  geom_area() +
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
  ylab("Source Fraction") +
  ggtitle("Earth Pool fraction (Net Change from 2005)") +
  xlab("Year")
areaGraph2



