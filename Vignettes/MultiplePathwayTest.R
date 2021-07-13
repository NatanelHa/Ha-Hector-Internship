#Importing Libraries 
library(hector)
library(ggplot2)

#Creating Multiple Cores
rcp26 <- system.file("input", "hector_rcp26.ini", package = "hector")
core26 <- newcore(rcp26, suppresslogging  = TRUE)

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core45 <- newcore(rcp45, suppresslogging  = TRUE)

rcp60 <- system.file("input", "hector_rcp60.ini", package = "hector")
core60 <- newcore(rcp60, suppresslogging  = TRUE)

rcp85 <- system.file("input", "hector_rcp85.ini", package = "hector")
core85 <- newcore(rcp85, suppresslogging  = TRUE)

#Run Cores
run(core26)
run(core45)
run(core60)
run(core85)

#Retrieve Results Carbon
result_vars <- c(ATMOSPHERIC_C(), SOIL_C(), DETRITUS_C(), VEG_C())
results26 <- fetchvars(core26, 2000:2200, result_vars, scenario = "RCP 2.6")
results45 <- fetchvars(core45, 2000:2200, result_vars, scenario = "RCP 4.5")
results60 <- fetchvars(core60, 2000:2200, result_vars, scenario = "RCP 6.0")
results85 <- fetchvars(core85, 2000:2200, result_vars, scenario = "RCP 8.5")

results <- rbind(results26,results45, results60, results85)

#Plot Results
ggplot(results)+
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")+
  ylab("Carbon (Pg C)")





