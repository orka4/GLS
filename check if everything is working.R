#Intro to GLS
#Hannah Udall
#28/11/23

# Set the working directory
setwd("C:/Users/Hannah Udall/OneDrive - University of Edinburgh/Documents/R script/repositary/tutorial-orka4")


# Load packages
library(nlme)
library(lmtest)
library(tidyverse)
library(geosphere)
library(ggeffects)
library(ape)
library(geostats) 

#DATA
airdata<- airquality

#EXPLORE DATA
names(airdata) #returns the column headings
head(airdata) #returns the first five rows

unique(airdata$Day) #returns the unique values in the 'Day' column of our dataset
unique(airdata$Month) #returns the unique values in the 'Month' column of our dataset

#TIME COLUMN
airdata$Date <- as.Date(paste(airdata$Month, airdata$Day, sep = "-"), format = "%m-%d") #concatenate Day and Month

airdata$DayOfYear <- as.numeric(format(airdata$Date, "%j")) # Extract the day of the year and create a new column to contain these values.

unique(airdata$DayOfYear) #returns the unique values in 'DayOfYear' column.

airdata<-airdata %>% select(-c("Day", "Month","Date"))

#RECOGNISING TEMPORAL AUTOCORRELATION
unique(airdata$Ozone)

na_count <- sum(is.na(airdata$Ozone)) #Counts the number of missing values in the ozone column
na_rows <- which(is.na(airdata$Ozone)) #The 'which' function gives us the indices of the rows with missing values.
nrow(airdata) #Counts the number of rows in the airdata dataframe.

airdata <- airdata[!is.na(airdata$Ozone), ] #Remove NA values.
data3_1<- airdata[-116,] #dataframe containing rows 1 to n-1. Last row is removed so it is the same dimensions as lagged dataframe.
data3_2<-airdata[-1,] #dataframe containing rows 2 to n. Lagged dataframe, compared to data3_1.
plot(data3_1$Ozone, data3_2$Ozone) #Plot t against t+1

acf(airdata$Ozone, lag.max=31) #autocorrelation function
pacf(airdata$Ozone, lag.max=31) #partial autocorrelation function

#Run a correlation test
cor<- cor.test(data3_1$Ozone,data3_2$Ozone) #Using our dataframes with time lag = 1 from earlier.

p<-cor$p.value  #extracts the p value from the summary statistics of the correlation test.

#Calculate correlation
cor(data3_1$Ozone,data3_2$Ozone)

#GLS MODEL
model <- gls(Ozone ~ DayOfYear, data = airdata,
             correlation = corARMA(form = ~ DayOfYear, p = 3))

model2<-gls(Ozone~DayOfYear, data=airdata,
            correlation = corARMA(form=~DayOfYear, q=1))

#Compute AIC for model and model2
AIC(model)
AIC(model2)

#Linear model
lmod<-lm(Ozone ~ DayOfYear, data = airdata) #Linear model
AIC(lmod)

summary(lmod) #the summary statistics of the linear model
summary(model) #the summary statistics of the gls model

#Check statistical assumptions
plot(model)

pacf(airdata$Ozone, na.action = na.pass, lag.max=31)
acf(airdata$Ozone, na.action = na.pass, lag.max=31)

#Plotting our model
#Create predicted values
pred.mm <- ggpredict(model, terms = c("DayOfYear"))  # this gives overall predictions for the model

# final plot ----

(final_plot_time <- ggplot(pred.mm) +
   geom_line(aes(x = x, y = predicted, linetype = "model predictions"),
             colour = "#FF8C00", size = 1) + 
   scale_linetype_manual('', values =c("model predictions" = 1))+
   geom_point(data = airdata,                      # adding the raw data 
              aes(x = DayOfYear, y = Ozone), size =2)+
   theme_classic() +
   ylab("\nOzone Concentration (ppb)") +                             
   xlab("Day of the Year\n")  +
   theme(axis.text.x = element_text(size = 15),
         axis.text.y = element_text(size = 15),
         axis.title = element_text(size = 15, face = "plain"),                      
         panel.grid = element_blank(), 
         plot.margin = unit(c(1,1,1,1), units = , "cm")))

#Save plot
ggsave("Images/final_plot_time.png", width = 9, height = 6, dpi = 300)

##PART TWO- HETEROSKEDASTICITY
#Linear model
lmod<-lm(Ozone ~ DayOfYear, data = airdata) 

#Breusch-Pagan Test (only works for linear regression models)
bptest(lmod)

#Diagnostic plots for our linear model
plot(lmod, which = 1) #This is the 'residuals vs fitted' plot
plot(lmod, which=3) #This is the 'scale-location' plot

model3<-gls(Ozone ~ DayOfYear, data = airdata,
            correlation = corARMA(form=~DayOfYear, p=3),weights=varPower(form=~DayOfYear))

summary(model3)

lmod3<- lm(abs(lmod$residuals) ~ lmod$fitted.values)

weights  <-  1 / lmod3$fitted.values^2

model4<-gls(Ozone ~ DayOfYear, data = airdata,
            correlation = corARMA(form=~DayOfYear, p=3),weights=weights)


AIC(model) #AIC for GLS model without variance structure weights
AIC(model3) #AIC for GLS model with variance structure weights

pred.mm2 <- ggpredict(model3, terms = c("DayOfYear"))  # this gives overall predictions for the model

# final plot ----

(final_plot_variance <- ggplot(pred.mm2) +
   geom_line(aes(x = x, y = predicted, linetype = "model predictions"),
             colour = "#FF8C00", size = 1) + 
   scale_linetype_manual('', values =c("model predictions" = 1))+
   geom_point(data = airdata,                      # adding the raw data 
              aes(x = DayOfYear, y = Ozone), size =2) +
   theme_classic() +
   ylab("\nOzone Concentration (ppb)") +                             
   xlab("Day of the Year\n")  +
   theme(axis.text.x = element_text(size = 15),
         axis.text.y = element_text(size = 15),
         axis.title = element_text(size = 15, face = "plain"),                      
         panel.grid = element_blank(), 
         plot.margin = unit(c(1,1,1,1), units = , "cm")))

ggsave("Images/final_plot_variance.png", width = 9, height = 6, dpi = 300)


#PART THREE- SPATIAL AUTOCORRELATION
#Load the data
data <- read.csv("Data/data.csv", header=TRUE)

spatial <- distm(fun = distGeo, x = data [,c ("lat", "long")] ) 

Moran.I(x = data$exp_shannon, spatial ) #need ape package

semivariogram<- semivariogram(x = data$lat, y = data$long, z = data$exp_shannon) #need geostats package

semivariogram

mod_gls <- gls( exp_shannon ~ LAI, data = data, correlation = corSpatial(form = ~ lat + long, 
                                                                         nugget = T ) )

summary(model)
plot(mod_gls)

plot(lmod2)
lmod2<- lm(exp_shannon~LAI, data=data)
summary(lmod2)

AIC(lmod2)

summary(model3)

AIC(mod_gls)
pred.mm3 <- ggpredict(mod_gls, terms = c("LAI"))  # this gives overall predictions for the model
# final plot ----

(final_plot_spatial <- ggplot(pred.mm3) +
   geom_line(aes(x = x, y = predicted, linetype = "model predictions"),
             colour = "#FF8C00", size = 1) + 
   scale_linetype_manual('', values =c("model predictions" = 1))+
   geom_point(data = data,                      # adding the raw data (scaled values)
              aes(x = LAI, y = exp_shannon), size =2) +
   #  geom_smooth(data=data, method = "lm", aes(x = LAI, y = exp_shannon), se=FALSE, colour = "red") +
   theme_classic() +
   ylab("Exponential Shannon's Diversity Index\n") +                             
   xlab("\nLeaf Area Index")  +
   labs(color = "Forest type\n")+
   theme(axis.text.x = element_text(size = 15),
         axis.text.y = element_text(size = 15),
         axis.title = element_text(size = 15, face = "plain"),                      
         panel.grid = element_blank(), 
         legend.text=element_text(size = 15),
         legend.title = element_text(size = 16),
         plot.margin = unit(c(1,1,1,1), units = , "cm")))

ggsave("Images/final_plot_spatial.png", width = 9, height = 6, dpi = 300)




