#Importing Libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#Reading Datasets
collar_map <- readr::read_csv("collar_map.csv")
fluxes <- readr::read_csv("fluxes.csv")

#Clean up collar_map
collar_map %>%
  select(-LI8100A_Port)->
  collar_map

#Wide to Long Data
fluxes %>%
  pivot_longer(-Collar, names_to="Year") ->
  fluxes_long

#Filter Out NA values
fluxes_long %>%
  filter(!is.na(value))->
  fluxes_long

#Merge the two tables
fluxes_long %>%
  left_join(collar_map, by="Collar")->
  fluxes_merged

#Compute and Print Summary
fluxes_merged %>%
  group_by(Treatment) %>%
  summarise(mean = mean(value), N = n(), Std_dev = sd(value))->
  summary_treatment

summary_treatment %>%
  print()

