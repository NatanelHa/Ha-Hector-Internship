#Importing libraries
library(hector)
library(ggplot2)
library(dplyr)

#tracking plot function
tracking_plot <-function(ini_file, graph_type, title) {
  #establish core
  core <- newcore(ini_file)
  
  #run core
  run(core)
  
  #Get Results
  results_with_diff <- get_tracking_data(core)
  
  #Filter out diff
  td <-
    results_with_diff %>%
    filter(pool_name != "Diff") %>%
    mutate(source_amount = source_fraction*pool_value)
  
  #Set Theme
  theme_set(theme_minimal())
  
  if (graph_type == "fraction"){
    areaGraph <- ggplot(td)+
      aes(x = year, y = source_fraction, fill = source_name)+
      geom_bar(stat = "identity")+
      facet_wrap(~pool_name, scales = "free_y")+
      geom_col(width = 1)+
      guides(fill = guide_legend(title = "Carbon Pools"))+
      ylab("Source Fraction") +
      ggtitle(title)+
      xlab("Year")
  }
  else {
    areaGraph<- ggplot(td)+
      aes(x = year, y = source_amount, fill = source_name)+
      geom_bar(stat = "identity")+
      facet_wrap(~pool_name, scales = "free_y")+
      geom_col(width = 1)+
      guides(fill = guide_legend(title = "Carbon Pools"))+
      ylab("Source Amount (Pg C)") +
      ggtitle(title)+
      xlab("Year")
  }
  
  #Shutdown Core
  shutdown(core)
  
  areaGraph
}

#Getting plots for each RCP
tracking_plot(system.file("input/hector_rcp26.ini", package = "hector"),
              "fraction", "RCP 2.6")
tracking_plot(system.file("input/hector_rcp26.ini", package = "hector"),
              "amount", "RCP 2.6")

tracking_plot(system.file("input/hector_rcp45.ini", package = "hector"), 
              "fraction", "RCP 4.5") 
tracking_plot(system.file("input/hector_rcp45.ini", package = "hector"), 
              "amount", "RCP 4.5") 

tracking_plot(system.file("input/hector_rcp60.ini", package = "hector"), 
              "fraction", "RCP 6.0")
tracking_plot(system.file("input/hector_rcp60.ini", package = "hector"), 
              "amount", "RCP 6.0")

tracking_plot(system.file("input/hector_rcp85.ini", package = "hector"), 
              "fraction", "RCP 8.5")
tracking_plot(system.file("input/hector_rcp85.ini", package = "hector"), 
              "amount", "RCP 8.5")


