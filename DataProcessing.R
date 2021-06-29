#Importing Libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#Reading Datasets
collar_map <- readr::read_csv("collar_map.csv")
fluxes <- readr::read_csv("fluxes.csv")

#Convert Group to Character
collar_map$Group <- as.character(collar_map$Group)

#Wide to Long Data
collar_map %>%
  select(-LI8100A_Port) %>%
  pivot_longer(-Collar, names_to = "Variable") ->
  collar_map_long

fluxes %>%
  pivot_longer(-Collar, names_to="Year") ->
  fluxes_long

#Filter Out NA values
collar_map_long %>%
  filter(!is.na(value))->
  collar_map_long

fluxes_long %>%
  filter(!is.na(value))->
  fluxes_long

#Merge the two tables
fluxes_long %>%
  left_join(collar_map_long, by="Collar")->
  fluxes_merged
