#Importing Libraries
library(dplyr)
library(tidyr)

#Reading in the csv
path <- "/Users/Natanel Ha/Documents/GitHub/Ha-Hector-Internship/"
data <- readr::read_csv(paste(path,
                              "GCAM scenario data/GCAM-DAC-SSP3pctHR.csv", 
                              sep =""))

#Filtering data
data %>%
  filter(Region=="World") %>%
  filter(Variable =="Emissions|CO2" | Variable =="Carbon Sequestration|Direct Air Capture")->
  data