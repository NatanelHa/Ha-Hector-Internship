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

rcp26 <- system.file("input", "hector_rcp26.ini", package = "hector")
td <- tracking_results(rcp26, 1900, 2000, "RCP 2.6")

atm_c <- td %>%
  filter(year==1900 | year==2000)%>%
  filter(pool_name=="atmos_c")%>%
  filter(source_name=="earth_c")
  

atm_c <- last(atm_c$source_amount) - first(atm_c$source_amount)

earth_c_loss <- td %>%
  filter(year==1900 | year==2000)%>%
  filter(pool_name=="earth_c")%>%
  filter(source_name=="earth_c")

earth_c_loss <- first(earth_c_loss$source_amount)- last(earth_c_loss$source_amount)

AF <- atm_c/earth_c_loss

