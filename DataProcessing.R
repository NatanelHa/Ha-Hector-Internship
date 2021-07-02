#Importing Libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#Reading Datasets
collar_map <- readr::read_csv("collar_map.csv")
fluxes <- readr::read_csv("fluxes.csv")

#Creating a long data table from fluxes and collar_map
fluxes %>%
  pivot_longer(-Collar, names_to="Date") %>% #Wide to Long
  filter(!is.na(value))%>%                   #Filter out NA
  left_join(collar_map, by="Collar")->        #Merging 
  fluxes_merged

#Compute and Print Summary
fluxes_merged %>%
  group_by(Treatment) %>%
  summarise(mean = mean(value), N = n(), Std_dev = sd(value))%>% 
  print()

#Boxplot with overlaid dots
fluxes_merged %>%
  ggplot(aes(x = Treatment, y = value))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, aes(color = Date))+
  ggtitle("Soil-to-atmosphere CO2 flux Boxplot by treatment")+
  ylab("Soil-to-atmosphere CO2 Flux (µmol/m2/s)")+
  theme_minimal()

#Boxplot Without Outliers
#Marking Which are Outliers
fluxes_merged %>% 
  group_by(Treatment) %>%
  summarise(median = median(value), IQR = IQR(value)) %>%
  right_join(fluxes_merged, by="Treatment") %>%
  mutate(not.outlier = as.numeric(abs(value-median)<1.5*IQR)) -> 
  filtered_fluxes

#Plotting Boxplot
filtered_fluxes %>%
  ggplot(aes(x = Treatment, y = value))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color = Date, alpha = not.outlier))+
  scale_alpha_continuous(range = c(0, 1), guide = FALSE)+
  ylim(c(0,18))+
  ggtitle("Soil-to-atmosphere CO2 flux Boxplot by treatment")+
  ylab("Soil-to-atmosphere CO2 Flux (µmol/m2/s)")+
  theme_minimal()

#Line Graph with error bars
fluxes_merged %>%
  group_by(Treatment, Date) %>%
  summarise(mean = mean(value), Std_dev = sd(value)) %>%
  ggplot(aes(x = Date, y = mean, color = Treatment, group = Treatment))+
  geom_line()+ 
  geom_errorbar(aes(ymax = mean + Std_dev, 
                    ymin = mean - Std_dev), 
                    width = 0.2, 
                    alpha = 0.4)+
  ggtitle("Soil-to-atmosphere CO2 Flux Line Graph with Error bars")+
  ylab("Mean Soil-to-atmosphere CO2 Flux (µmol/m2/s)")+
  theme_minimal()

#Finding out which collars do not appear in fluxes and printing 
collar_map %>%
  anti_join(fluxes, by="Collar")

print("Collar 34 is listed in the metadata that do not appear in the fluxes data file.")
print("Collar 34 is in the Fresh Plot, in Group 9, and the treatment was Disturbance")  
                 
