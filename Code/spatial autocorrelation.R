#SPATIAL AUTOCORRELATION

#LIBRARIES
library(spdep)
library(geosphere)
library(tidyverse)
library(ape)
library(nlme)
library (geosphere)
library(ape)
library(readxl)
library(geostats)
library(nlme)
#DOWNLOAD DATA
data <- read.csv("Data/data.csv", header=TRUE)
mod<-lm(exp_shannon~LAI, data=data)
plot(mod)
#DATA MANIPULATION
#Change data to long form
long_data <- data %>% pivot_longer(cols = 11:32, names_to = "Species", values_to = "count")
view(long_data)
#Create a spatial matrix of data
spatial <- distm(fun = distGeo, x = data [,c ("lat", "long")] ) 

#CHECK FOR CORRELATION (INDEPENDENCE ASSUMPTION)
#First by calculating Moran's I 
Moran.I(x = data$exp_shannon, spatial)


#Next by calculating correlation
cor_test_result <- cor.test(data$LAI, data$exp_shannon)

# Extract the p-value
p_value <- cor_test_result$p.value



#PLOT SEMIVARIOGRAM
# Scale the latitude and longitude columns to km.
lat_scaling_factor <- 111  # 1 degree of latitude is approximately 111 kilometers
long_scaling_factor <- 111 

data_scaled <- data
data_scaled$lat <- data$lat * lat_scaling_factor
data_scaled$long <- data$long * long_scaling_factor

# Compute the semivariogram with the scaled coordinates (so the x axis is in km)
semivariogram(x = data_scaled$lat, y = data_scaled$long, z = data$exp_shannon)


#INCORPORATE INTO GLS
# gls model - input a correlation structure
# account for spa
# spatial a structure  distance matrix (spatial)

mod_gls <- gls( exp_shannon ~ LAI, data = data_final, correlation = corSpatial(form = ~ lat + long, 
                                                                               nugget = T ) )

?gls()
