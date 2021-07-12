#Importing Libraryes
library('ggplot2')
library('hector')

#Configuring and running Hector
hector_inifile <- file.path(system.file('input', package='hector'), 'hector_rcp60.ini')
hcore <- newcore(hector_inifile, suppresslogging=TRUE)
run(hcore, 1999)

#Creating a function that runs hector and returns difference between target and actual 
f <- function(emiss)
{
  setvar(hcore, 2000:2100, EMISSIONS_CH4(), emiss, "Tg CH4")
  reset(hcore, 1999)
  run(hcore, 2100)
  hout <- fetchvars(hcore, 2000:2100, ATMOSPHERIC_CH4())
  ## return the difference between the target of 1850 (constant) and the actual
  hout$value - 1820
}

#Solve for target emissions
x <- rep(300.0, times=101)  # 2000:2001 includes both endpoints
slv <- nleqslv::nleqslv(x, f, method="Broyden")

#Checking to see if it converged
max(abs(slv$fvec))

#Getting results 
df <- data.frame(year = 2000:2100,
value = slv[['x']])

#Plotting results
ggplot2::ggplot(data = df, aes(year, value)) +
  geom_point() +
  labs(y = "Emissions (Tg CH4)", x = 'year')

#Shutting down
shutdown(hcore)
