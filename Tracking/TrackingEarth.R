#Importing libraries
library(hector)
library(ggplot2)
library(dplyr)

#establish core
rcp26 <- system.file("input/hector_rcp26.ini", package = "hector")
core <- newcore(rcp26, suppresslogging  = TRUE)

#run core
run(core)

#Get Results
results <- get_tracking_data(core)

results %>%
  filter(pool_name == "earth_c") %>%
  filter(year >= 2073) %>%
  mutate(source_amount = source_fraction*pool_value)->
  results

earth_2073 <- 
  results %>%
  filter(source_name == "earth_c") 

earth_2073_carbon <-
  first(earth_2073$source_amount)

earth_2073_fraction <-
  first(earth_2073$source_fraction)

results %>%
  mutate(source_amount = source_amount - earth_2073_carbon*(source_name=="earth_c"))%>%
  mutate(source_fraction = source_fraction - earth_2073_fraction*(source_name=="earth_c"))->
  results

areaGraph<- ggplot(results)+
  aes(x = year, y = source_amount, fill = source_name)+
  geom_bar(stat = "identity")+
  scale_fill_discrete(labels = c("Detritus",
                                 "Vegetation",
                                 "Soil",
                                 "Earth",
                                 "Atmosphere",
                                 "High Level Ocean",
                                 "Intermediate Ocean",
                                 "Low Level Ocean",
                                 "Deep Ocean"))+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  ylab("Source Amount (Pg C)") +
  ggtitle("Earth Pool (Net Change from 2073)")+
  xlab("Year")
areaGraph

areaGraph2 <- ggplot(results)+
  aes(x = year, y = source_fraction, fill = source_name)+
  geom_bar(stat = "identity")+
  scale_fill_discrete(labels = c("Detritus",
                                 "Vegetation",
                                 "Soil",
                                 "Earth",
                                 "Atmosphere",
                                 "High Level Ocean",
                                 "Intermediate Ocean",
                                 "Low Level Ocean",
                                 "Deep Ocean"))+
  geom_col(width = 1)+
  guides(fill = guide_legend(title = "Carbon Pools"))+
  ylab("Source Fraction") +
  ggtitle("Earth Pool fraction (Net Change from 2073)")+
  xlab("Year")
areaGraph2