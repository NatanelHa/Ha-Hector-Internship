#Importing libraries
library(hector)
library(ggplot2)

## Basic Run
#Configuring INI Files
ini_file26 <- system.file("input/hector_rcp26.ini", package = "hector") 
ini_file45 <- system.file("input/hector_rcp45.ini", package = "hector") 

#Initialize the Cores
core26 <- newcore(ini_file26)
core45 <- newcore(ini_file45)

#Run the Core
run(core26) 
run(core45) 

#Getting tracking
td26_with_diff <- get_tracking_data(core26)
td45_with_diff <- get_tracking_data(core45)

#Filter out diff
td26 <-
  td26_with_diff %>%
  filter(pool_name != "Diff")

td45<-
  td45_with_diff %>%
  filter(pool_name != "Diff")

#Set Theme
theme_set(theme_minimal())

##Area Graphs
areaGraph26 <- ggplot(td26)+
  aes(x = year, y = source_fraction, fill = source_name)+
  geom_bar(stat = "identity")+
  facet_wrap(~pool_name)+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  ylab("Source Fraction") +
  xlab("Year")
areaGraph26

areaGraph45 <- ggplot(td45)+
  aes(x = year, y = source_fraction, fill = source_name)+
  geom_bar(stat = "identity")+
  facet_wrap(~pool_name)+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  ylab("Source Fraction") +
  xlab("Year")
areaGraph45

#Shutdown
shutdown(core26)
shutdown(core45)





