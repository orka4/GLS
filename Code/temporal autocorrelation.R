##TEMPORAL AUTOCORRELATION

#LIBRARIES
library(tidyverse)


#DOWNLOAD DATA
data2 <- read.csv("Data/data2.csv", header=TRUE)

#DATA MANIPULATION
data2 <- data2 %>%
  na.omit()
  group_by(Location) %>%  # group rows so that each group is one population
  mutate(maxyear = max(Year), minyear = min(Year),  # Create columns for the first and most recent years that data was collected
         lengthyear = maxyear-minyear,  # Create a column for the length of time data available
         scalepop = (Population-min(Population))/(max(Population)-min(Population))) %>%  # Scale population trend data so that all values are between 0 and 1
  filter(is.finite(scalepop),  # remove NAs
         lengthyear > 5) %>%  # Only keep rows with more than 5 years of data
  ungroup() 

data2$Year<-as.numeric(data2$Year)
data2$simple_year <- data2$Year-1950

#CHECK ASSUMPTION OF INDPENDENCE
#Plot a linear model
lmod<-lm(scalepop~simple_year, data = data2)


#CHECK STRUCTURE OF CORRELATION
#look at residuals
n=length(residuals(lmod))
cor(residuals(lmod)[-1], residuals(lmod)[-n]) 

#Visually check the structure of the correlation
acf(data2$scalepop)#exponential decrease so AR
pacf(data2$scalepop) #3 significant

#Is this correct? Run multiple gls and check AIC value.

#Plot time series
plot(scalepop~simple_year, data=data2)
library(nlme)
#TURN INTO A GLS MODEL
?gls
mod2<-gls(scalepop~simple_year, data=data2, correlation = corAR(p = 3))
mod2 <- gls(scalepop ~ simple_year, data = data2, correlation = corAR1(0.5, form = ~ 1 | simple_year))

mod2