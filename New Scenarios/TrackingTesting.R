#Importing libraries
library(hector)
library(ggplot2)

#tracking plot function
tracking_plot <-function(ini_file, title) {
  #establish core
  core <- newcore(ini_file)
  
  #run core
  run(core)
  
  #Get Results
  results_with_diff <- get_tracking_data(core)
  
  #Filter out diff
  td <-
    results_with_diff %>%
    filter(pool_name != "Diff")
  
  #Set Theme
  theme_set(theme_minimal())
  
  areaGraph <- ggplot(td)+
    aes(x = year, y = source_fraction, fill = source_name)+
    geom_bar(stat = "identity")+
    facet_wrap(~pool_name)+
    geom_col(width = 1)+
    guides(fill = guide_legend(title = "Carbon Pools"))+
    ylab("Source Fraction") +
    ggtitle(title)+
    xlab("Year")
  
  shutdown(core)
  
  areaGraph
}

#Getting plots for each rcp
tracking_plot(system.file("input/hector_rcp26.ini", package = "hector"), "RCP 2.6")
tracking_plot(system.file("input/hector_rcp45.ini", package = "hector"), "RCP 4.5") 
tracking_plot(system.file("input/hector_rcp60.ini", package = "hector"), "RCP 6.0")
tracking_plot(system.file("input/hector_rcp85.ini", package = "hector"), "RCP 8.5")


