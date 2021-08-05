# Importing Libraries
library(dplyr)
library(tidyr)
library(zoo)

# Reading in the csv
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/"
data <- readr::read_csv(paste(path,
  "GCAM scenario data/GCAM-DAC-SSP3pctHR.csv",
  sep = ""
))
# setting scenario
scenario <- "SSP5-1p9-3pctHR"

# Filtering data
data %>%
  filter(Region == "World") %>%
  filter(Variable == "Emissions|CO2|Energy and Industrial Processes" |
    Variable == "Emissions|CO2|AFOLU" |
    Variable == "Carbon Sequestration|Direct Air Capture") %>%
  filter(Scenario == scenario) %>%
  select(-Model, -Unit, -Scenario, -Region) ->
data

# Wide to long and column changing
data %>%
  pivot_longer(-Variable, names_to = "Years") ->
data

# Converting to Int
data$Years <- as.integer(data$Years)

# Filling in blank years
data %>%
  complete(Variable, Years = 2005:2100) %>%
  pivot_wider(names_from = Variable) %>%
  rename(ffi_emissions_with_negative = "Emissions|CO2|Energy and Industrial Processes") %>%
  rename(luc_emissions = "Emissions|CO2|AFOLU") -> #%>%
  #rename(daccs_uptake = "Carbon Sequestration|Direct Air Capture") ->
data

data$ffi_emissions_with_negative <- na.approx(data$ffi_emissions_with_negative, data$Years) / 3670

data$luc_emissions <- na.approx(data$luc_emissions, data$Years) / 3670

#data$daccs_uptake <- na.approx(data$daccs_uptake, data$Years) / 3670

data %>%
  mutate(ffi_emissions = ffi_emissions_with_negative) %>% # + daccs_uptake) %>%
  select(Years, ffi_emissions, luc_emissions) ->
data

write.csv(data, paste(path, "New Scenarios/", scenario, ".csv", sep = ""), row.names = FALSE)