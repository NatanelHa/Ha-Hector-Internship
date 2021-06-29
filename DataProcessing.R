#Importing Libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#Reading Datasets
collar_map <- readr::read_csv("collar_map.csv")
fluxes <- readr::read_csv("fluxes.csv")

#Clean up collar_map
collar_map %>%
  select(Treatment, Collar)->
  collar_map

#Wide to Long Data
fluxes %>%
  pivot_longer(-Collar, names_to="Date") ->
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

#Boxplot with overlaid dots
fluxes_merged %>%
  ggplot(aes(x = Treatment, y = value))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, aes(color = Date))+
  theme_minimal()

#Line Graph with error bars
fluxes_merged %>%
  group_by(Treatment, Date) %>%
  summarise(mean = mean(value), Std_dev = sd(value)) %>%
  ggplot(aes(x = Date, y = mean, color = Treatment, group = Treatment))+
  facet_wrap(~Treatment)+
  geom_line()+ 
  geom_errorbar(aes(ymax = mean + Std_dev, 
                     ymin = mean - Std_dev), 
                 width = 0.2)+
  theme_grey()

#Finding out which collars do not appear in fluxes
fluxes_long %>%
  anti_join(collar_map, by="Collar")

print("There are no collars listed in the metadata that do not appear in the data file")  
                 