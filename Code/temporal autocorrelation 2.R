#TEMPORAL AUTOCORRELATION

#LIBRARIES
library(tidyverse)
library(GGally)
library(nlme)

#DATA
data3<- airquality
head(airquality)

#DATA WRANGLING
unique(airquality$Day) #Test to see if days are unique or not

#DATA MANIPULATION


data3$Date <- as.Date(paste(data3$Month, data3$Day, sep = "-"), format = "%m-%d")
names(airquality)
# Extract the day of the year
data3$DayOfYear <- as.numeric(format(data3$Date, "%j"))
data3<-data3 %>% select(-c("Day", "Month","Date"))

data3$DayOfYear
#TEST FOR TEMPORAL AUTOCORRELATION BETWEEN OZONE AND TIME
data3 <- data3[!is.na(data3$Solar.R), ] #Make sure all ozone values are numeric

data3_1<- data3[-111,] #dataframe containing rows 1- n-1

data3_2<-data3[-1,] #dataframe containing rows 2-n
nrow(data3)

plot(data3_1$Ozone, data3_2$Ozone) #Plot t against t+1

#Test for correlation
cor<- cor.test(data3_1$Solar.R,data3_2$Solar.R) #Dataframe is in time order
p<-cor$p.value #p value very small so there is temporal correlation
p
# Example with a data frame
data1<-cbind(data3_1,data3_2)
correlation_matrix <- cor(data3)
# Example heatmap using the corrplot package
library(corrplot)
corrplot(correlation_matrix, method = "color")
data3$Ozone<-as.numeric(data3$Ozone)
data3$Solar.R<-as.numeric(data3$Solar.R)
#Calculate correlation
cor(data3_1$Solar.R,data3_2$Solar.R)
str(data3)
#There is temporal correlation which is a problem as it means assumption of independence of data points
#doe not hold.

#To see the correlation at different lag values (above it was just for lag 1) we can use acf()
acf(data3$Solar.R, lag.max=31) #Since the vertical lines go above the dashed line up until point 4, there
#is significant autocorrelation up to lag 4.

pacf(data3$Solar.R, na.action = na.pass, lag.max=31)
#The autocorrelation function (acf()) includes the cumulative effect of intermediate lags, while the partial autocorrelation function (pacf()) isolates the direct relationship between a data point and its lagged values.data3)
?acf()

##PLOT T AGAINST T+1 ALL AT ONCE

# Assuming 'your_data' is your dataframe
# Replace it with your actual data

# Create a lagged version of the dataframe (t+1)
lagged_data <- data3[-1,]

# Assuming your dataframes are named df1 and df2


names(data3_1)<-c('Ozone1', 'Solar.R1', 'Wind1', 'Temp1')
# Merge the two dataframes based on a common column, e.g., 'id'
merged_df <- cbind(lagged_data, data3_1)

# Create a matrix of scatterplots using ggpairs
ggpairs(merged_df, 
        columns = c('Ozone', 'Solar.R', 'Wind', 'Temp','Ozone1', 'Solar.R1', 'Wind1', 'Temp1'),  # Replace with your actual variable names
        title = "Scatterplots of Variables with one time lag")


#This is also good for looking for collinear variables

#MODEL DATA
model <- gls(Ozone ~ DayOfYear, data = data3,
             correlation = corARMA(form = ~ DayOfYear, p = 20))

model2 <- gls(Ozone ~ DayOfYear, data = data3,
             correlation = corARMA(form=~DayOfYear, q=3))

model4<-gls(Ozone ~ DayOfYear, data = data3,
            correlation = corARMA(form=~DayOfYear, p=3),weights=varPower(form=~DayOfYear))
AIC(model)
#or correlation = corAR1())
AIC(model4)
AIC(model2) #model is better as we used correct correlation structure.
AIC(lmod)
plot(lmod)
plot(model, which = 3, pch = 16, col = "blue", main = "Scale-Location Plot")
model1<-gls(Ozone~DayOfYear, data=data3)
library(forecast)
lmod<-lm(Ozone ~ DayOfYear, data = data3)
summary(lmod) #shows correlation
summary(model) #no correlation exists in reality
plot(lmod)
?gls()


