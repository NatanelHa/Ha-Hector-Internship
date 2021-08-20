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

#Calculating Airborne Fraction
AF_calc <- function(td, start, stop) {
    
  #Filtering for atmosphere
  atm_c <- td %>%
    filter(year == start | year == stop) %>%
    filter(pool_name == "atmos_c") %>%
    filter(source_name == "earth_c")

  #Calculating  atmosphere uptake of earth carbon
  atm_c <- last(atm_c$source_amount) - first(atm_c$source_amount)
  
  #Calculating all earth carbon emitted
  earth_c_loss <- td %>%
    filter(year == start | year == stop) %>%
    filter(pool_name == "earth_c") %>%
    filter(source_name == "earth_c")

  earth_c_loss <- first(earth_c_loss$source_amount) - last(earth_c_loss$source_amount)

  #Calculating Airborne Fraction
  AF <- atm_c / earth_c_loss
  
  return(AF)
}

rcp26 <- system.file("input", "hector_rcp26.ini", package = "hector")
td <- tracking_results(rcp26, 1750, 2050, "RCP 2.6")

AF_2000 <- AF_calc(td, 1750, 2000)
AF_2020 <- AF_calc(td, 1750, 2020)

#Comparing to values in Knorr 2009
names <- c("KNORR 1","KNORR 2","KNORR 3","KNORR 4","KNORR 5","KNORR 6","KNORR 7","HECTOR 2.6")
values <- c(0.45, 0.453, 0.518, 0.468, 0.468, 0.514, 0.449, AF_2000)
error <- c(0.022, 0.014, 0.064, 0.047, 0.051, 0.035, 0.014, 0)
df <- data.frame(names, values, error)

graph <- ggplot(df) +
  aes(x=names, y=values, fill=names) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=values-error, ymax=values+error), width=.2)+
  scale_fill_manual(values=c("#B92E34", "grey50", "grey50", "grey50", "grey50",
                             "grey50", "grey50", "grey50")) +
  theme(legend.position = "none")+
  ggtitle("Airborne Fraction Comparison")+
  ylab("Airborne Fraction in 2020")+
  xlab(NULL)

graph

