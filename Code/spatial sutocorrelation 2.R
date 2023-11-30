#SPATIAL AUTOCORRELATION


#LIBRARIES
library(geosphere)
library(tidyverse)
library(geostats)
library(spdep)
library(gstat)
names(long_data)
#DOWNLOAD DATA
data <- read.csv("Data/data.csv", header=TRUE)
head(long_data)
mode<- lm(stand_density~soil_ph, long_data)
plot(mode)
names(data)
#DATA MANIPULATION
#Change data to long form
long_data <- data %>% pivot_longer(cols = 11:32, names_to = "Species", values_to = "count")
data1 <- long_data %>%
  mutate(forest_type = if_else(.$pine_percentage == "100", 'pine', 'mixed')) 

#Check for spatial autocorrelation
#MORAN'S I
spatial <- distm(fun = distGeo, x = data[,c ("lat", "long")] )

morans_i<-Moran.I(x = data$exp_shannon, spatial)

#VISUALLY

#Plot a variogram
semivariogram(x = data$lat, y = data$long, z = data$exp_shannon)

#With lag units in kilometres
data$s_lat <- data$lat * 111
data$s_long <- data$long * 111

# Compute the semivariogram with the scaled coordinates (so the x axis is in km)
semivariogram<- semivariogram(x = data$s_lat, y = data$s_long, z = data$exp_shannon, fit=TRUE)

semivariogram #returns sill, nugget and range

#dependent on rotation
# Create a spatial data frame in km
coordinates(data) <- ~s_long + s_lat

# Create a variogram model with specified orientation
variogram_model <- vgm(psill = 50, model = "Exp", range = 10, 
                       dir.horiz = 180, dir.vert = 70 )

# Compute the empirical semivariogram
semivariogram <- variogram(exp_shannon ~ 1, data, model = variogram_model)

# Fit the variogram model
semivariogram_fit <- fit.variogram(semivariogram, model = variogram_model, fit=TRUE)

# Plot the results
plot(semivariogram, semivariogram_fit)
semivariogram_fit

# Plot a variogram cloud to see spatial patterns.
plot(variogram(exp_shannon~1,  data=data, cloud=TRUE))
