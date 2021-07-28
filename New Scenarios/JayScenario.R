#Importing Libraries
library(dplyr)
library(tidyr)
library(zoo)

#Reading in the csv
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/"
data <- readr::read_csv(paste(path,
                              "GCAM scenario data/GCAM-DAC-SSP3pctHR.csv", 
                              sep =""))
#setting scenario
scenario <- "SSP1-1p9-DACCS-3pctHR"

#Filtering data
data %>%
  filter(Region=="World") %>%
  filter(Variable =="Emissions|CO2" | Variable =="Carbon Sequestration|Direct Air Capture") %>%
  filter(Scenario==scenario)%>%
  select(-Model, -Unit, -Scenario, -Region)->
  data

#Wide to long and column changing
data %>%
  pivot_longer(-Variable, names_to = "Years")->
  data

#Converting to Int
data$Years <- as.integer(data$Years)

#Filling in blank years
data %>%
  complete(Variable, Years = 2005:2100) %>%
  pivot_wider(names_from=Variable)%>%
  rename(emissions = "Emissions|CO2")%>%
  rename(antiemissions = "Carbon Sequestration|Direct Air Capture") ->
  data

data$emissions<-na.approx(data$emissions, data$Years)/3670

data$antiemissions<-na.approx(data$antiemissions, data$Years)/3670

write.csv(data, paste(path,"New Scenarios/",scenario,".csv", sep=""), row.names = FALSE)