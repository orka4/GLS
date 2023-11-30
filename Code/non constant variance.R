#HOMOSKEDASTICITY VIOLATION



#DOWNLOAD LPI WOLF DATA
data2 <- read.csv("Data/data2.csv", header=TRUE)



#DATA MANIPULATION
data2$year.scaled <- scale(I(data2$Year-1950))
#Plot variance
#Execute linear model with population as the response variable, so we have heteroskedasticity.
lmod<-lm(Population~year.scaled, data = data2)
plot(lmod)
#The residuals are not normal at the moment either, but we are going to pretend they are as this is a requirement for this test.

#Both the scale-location and the residuals vs fitted plots show we do not have equal variance.
install.packages("lmtest")
library(lmtest)
bptest(lmod) #Breusch-Pagan Test
#p-value is lower than 0.05 so there is heteroskedasticity.
library(nlme)
?gls()
varFunc(lmod)

#CHECK FOR CONSTANT VARIANCE
# Extract the variance-covariance matrix
cov_matrix <- vcov(lmod)

# Extract standard errors from the diagonal of the matrix
standard_errors <- sqrt(diag(cov_matrix))
varFunc(standard_errors)

#ADD WEIGHTS


#GLS WITH WEIGHTS AND TEMPORAL CORRELATION