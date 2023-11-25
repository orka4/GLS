

<p align="center">
  <img src="Heading image.png" >
</p>

*Created by Hannah Udall - last updated*
***
This tutorial is aimed at people who already have a good understanding of linear models, the statistical assumptions of these models and have a good level of R proficiency. If you are less confident of the above topics I recommend briefly looking at these tutorials for the appropriate background information.

**Acknowledgements:** Thanks are due to my fieldwork group (Luisa Dickenmann, Rebecca Hies, Kinga Kaszap, Thomas Pendlebury, Else Radeloff and Tegan Williams) whom I collected the data with for the Spatial autocorrelation part of the tutorial, and Mathew Rees who was our supervisor during this project and introduced me to the concept of generalised least square models. Also to Claudia Colesie, who taught me about semivariance in the Ecological Measurement course.

There is a lot of text in this tutorial, as the form of variance, spatial and temporal autocorrelation varies significantly between datasets, so a good understanding of the model is important. 

<a name="download"></a> >To get all the materials for this tutorial head <a href="https://github.com/ourcodingclub/CC-EAB-tut-ideas" target="_blank">this GitHub repository</a>, click on the Code button and Download ZIP and unzip the folder. Alternatively clone the repositary to your github- more information on how to do this is found [here](https://ourcodingclub.github.io/tutorials/git/index.html). 


## Tutorial Aims

### <a href="#section1"> 1. What is a GLS model and why should I care?</a>

### <a href="#section2"> 2. Temporal autocorrelation</a>
  #### <a href="#subsection2">   - What is temporal autocorrelation and why is it a problem?</a>

### <a href="#section3"> 3. Understanding non-constant variance</a>

## <a name="section1"></a> What is a GLS model and why should I care?
This tutorial is an introduction to the usefulness of **generalised least square models**, or gls for short. These models are incredibly useful as they allow you to statistically model data whose data is not necessarily independent and/or has non-constant variance (heteroskedastic). Linear models can only be created with condfidence if certain statistical assumptions are met. Two of these assumptions are **independence of datapoints** and **constant variance**. This tutorial will hopefully help anyone who has data that is not independent or does not have constant variance- as your data can still be modelled!

So let's begin! Open `RStudio` and create two new script's (as this tutorial has two main sections) by clicking on `File/ New File/ R Script`. Set the working directory to wherever you have saved the folder for this tutorial (mentioned <a href="#download"> here</a>), and start by loading the relevant **packages**.


```r
# Set the working directory
setwd("your_filepath")

# Load packages
library(nlme)
library(tidyverse)
library(lmtest)
```

If you don't have them already, remember to install them as so:

```r
install.packages("package name")
```

This tutorial is split into three sections: 

Section one discusses how to deal with **temporal autocorrelation** which is when your datapoints are not independent through time.

Section two discusses how to deal with **non constant variance** which is when your (typically linear) model has non-constant variance.

Section three discusses how to deal with **spatial autocorrelation** which is when your datapoints are not independent through space.

If you already know which part you need to learn about, feel free to skip to the relevant section by clicking on the relevant aim in the 'tutorial aims' section above. On the otherhand, if you want to complete the whole tutorial, or don't know what the three section headings mean, complete the tutorial in order.

## <a name="section2"></a> Temporal autocorrelation

### <a name="subsection2"></a>  What is temporal autocorrelation and why is it a problem?


**Temporal autocorrelation is when your data has an internal structure that means datapoints separated by certain intervals of time are more similar than other points. Often this can be seen as data collected daily being more similar to days close to it, however other structures can also be apparent.**

Temporal autocorrelation is a problem as it negates the statistical assumption of **independence of observations**. In this tutorial we are going to learn how to check for temporal autocorrelation in our data and what to do if we find it.

**Load the data**
Since we are using a dataset in-built to R, we can just write the name of the dataset and rename it to 'airdata'.

```r
#DATA
airdata<- airquality
```

# Explore the data
The data we are going to be using in this part of the tutorial is a dataset pre-loaded onto R called 'airquality'. This dataset contains data about the air quality in New York in 1973 and has data from May to September.

Let's explore the data using the functions names() and head()

```r
names(data) #returns the column headings
head(data) #returns the first five rows
```
In this tutorial we are going to look at how the concentration of ozone changes through time.

As we are going to be looking at the **temporal structure**, we need to understand how the temporal data is inputted. To do this, run the following code
to understand how often data is collected.

```r
unique(airdata$Day) #returns the unique values in the 'Day' column of our dataset
unique(airdata$Month) #returns the unique values in the 'Month' column of our dataset
```

From this we can see that the 'Day' column contains the **date**, and the 'Month' column contains the **numerical month** (where 1 = January).
To analyse how the variables change over time we need a continuous time variable. To create this we are going to concatenate 'Day' and 'Month' and use the following code to calculate the day of the year.

```r
airdata$Date <- as.Date(paste(airdata$Month, airdata$Day, sep = "-"), format = "%m-%d") #concatenate Day and Month

airdata$DayOfYear <- as.numeric(format(airdata$Date, "%j")) # Extract the day of the year and create a new column to contain these values.
```
Now, let's run the following code to check the 'DayOfYear' has turned out like we expect.

```r
unique(data3$DayOfYear) #returns the unique values in 'DayOfYear' column.
```

We shall delete the other 'time' columns to avoid confusion later in the tutorial.
```r
airdata<-airdata %>% select(-c("Day", "Month","Date"))
```

# Test for temporal autocorrelation
Since we want to run a statistical model looking at how ozone concentration changes with time, we are going to test for temporal autocorrelation in the ozone variable. 

To visually check for temporal autocorrelation between time 't' and time 't+1' we can create two dataframes, one lagged and one not. 

First, we want to check the structure of the ozone data. To do this we shall look at the unique values.
```r
unique(airdata$Ozone)
```
Since there are NA values in the column, we need to see how often they occur and whether they are scattered throughout the dataset or clumped. This will tell us whether removing the rows will alter our dataset in a big way and affect our analysis.
```r
na_count <- sum(is.na(airdata$Ozone)) #Counts the number of missing values in the ozone column
na_rows <- which(is.na(df$your_column)) #The 'which' function gives us the indices of the rows with missing values.
nrow(airdata) #Counts the number of rows in the airdata dataframe.
```
As the number of missing values ('NA' values) are small compared to the length of the dataframe, and they are scattered not clumped, we can remove them.

```r
airdata <- airdata[!is.na(airdata$Ozone), ] #Remove NA values.
data3_1<- data3[-116,] #dataframe containing rows 1 to n-1. Last row is removed so it is the same dimensions as lagged dataframe.
data3_2<-data3[-1,] #dataframe containing rows 2 to n. Lagged dataframe, compared to data3_1.
plot(data3_1$Ozone, data3_2$Ozone) #Plot t against t+1
```
If there was no temporal autocorrelation we would expect no pattern in the plot, however there is which suggests autocorrelation. This quick plot has only checked for autocorrelation with a time lag of 1 (here 1 day). To see if there is correlation between different time intervals, we can plot an **autocorrelation function** and a **partial autocorrelation function**.

```r
acf(airdata$Ozone, lag.max=31) #autocorrelation function
pacf(data3$Ozone, lag.max=31) #partial autocorrelation function
```
The plots should look like so:
<p align="center">
  <img src="Images/ACF.png" alt="ACF">
</p>

<p align="center">
  <img src="Images/PACF.png" alt="PACF">
</p>

If the vertical black line is **above** the dotted line it means there is temporal autocorrelation at that lag value.

**Note:** The autocorrelation function (ACF) for temporal patterns is always 1 at lag 0.

The **autocorrelation function (acf())** includes the **cumulative** effect of intermediate lags, while the **partial autocorrelation function (pacf())** isolates the **direct relationship** between a data point and its lagged values. 

'lag.max' allows you to state how many lag values you want included on the x axis. We have chosen 31 here to replicate the 30/31 days in the month's in our dataset.

Since the black vertical line **surpasses the dotted line** at multiple lag values in both plots we can conclude there is temporal autocorrelation in this data. 
We shall analyse these plots more later on in the tutorial.

## Numerical methods of testing for temporal autocorrelation
To test for temporal autocorrelation **numerically** we can run a correlation test or calculate the correlation using the code below. 

```r
#Run a correlation test
cor<- cor.test(data3_1$Ozone,data3_2$Ozone) #Using our dataframes with time lag = 1 from earlier.

p<-cor$p.value  #extracts the p value from the summary statistics of the correlation test.
```
If the p value is less than the significance threshold (often 0.05), the null hypothesis of correlation is accepted.
The p value is very small, which confirms temporal autocorrelation with a time lag of 1.

```r
#Calculate correlation
cor(data3_1$Ozone,data3_2$Ozone)
```
The correlation is always between **-1 and 1**. A positive number signifies a positive linear relationship, a negative number signifies a negative linear relationship and if it is near zero this signifies zero correlation. The strength of the correlation is based on the magnitude of the correlation. Here we have moderate correlation between time 't' and 't+1'.

## Modelling temporally autocorrelated data
This is when the **generalised least squares model** comes in! 
This model allows you to account for autocorrelation which is super important because without accounting for autocorrelation you might get a significant result when in reality there is none- it is just an internal correlation structure causing a relationship!

Gls works similarly to ordinary least squares, like the linear model (function lm()) we are used to, by fitting a **line-of-best-fit** through the data points. However, instead of assuming constant variance and no correlation, gls allows these characteristics to be present in the data.

Gls introduces the concept of weights into the model. These weights are chosen based on an assumed structure in the variance of the data. Observations with higher weights contribute more to the fitting process. This 'weights' process allows the effect autocorrelation to be accounted for when fitting the model.

The syntax is as follows. To determine what 'correlation' is equal to, we need to determine the **correlation structure** we have in our data.

```r
model <- gls(Ozone ~ DayOfYear, data = data3,
             correlation = -----)
```

To determine the correlation structure we need to look again at the **acf and pacf plots**.

If the acf drops of **gradually** and the pacf drops off **abruptly**, you have and **autoregressive model (AR)** with order of the largest significant lag in the pacf.

If the acf drops off **abruptly** and the pacf drops off **gradually**, you have a **moving average model (MA)** with an order to largest significance lag in acf (discounting lag = 0).

In both cases alternating between positive and negative doesn't matter.

Higher order processes can be harder to diagnose. Also some data with have a combination of autoregressive and moving average which are especially hard to diagnose. I am going to show you how to incorporate simple structures into your gls, and then we shall chat about what to do if the structure looks complex and you are not sure.

So, now you know the structure, what is the syntax for the gls model?

It is pretty simple. It takes the form corARMA(form = ~DayOfYear, **p**= 'order') for AR structures and corARMA(form=~DayOfYear, **q**='order') for MA structures.

The 'form=~' part should be equal to the vector containing the time data. For your own data, change 'DayOfYear' accordingly.

Let's look at the acf and pacf plots again:
<p align="center">
  <img src="Images/ACF.png" alt="ACF">
</p>

<p align="center">
  <img src="Images/PACF.png" alt="PACF">
</p>

Here, the acf drops off gradually and the pacf has largest significance 3. (_Note:_ there is slight significance at lag = 20 however we are not going to input this into the model as there is no significance before this lag value, and it is only slight)

So here we have a AR structure with order 3 so we use the following syntax for the gls:

```r
model <- gls(Ozone ~ DayOfYear, data = data3,
             correlation = corARMA(form = ~ DayOfYear, p = 3))
```

**What if I am unsure about my correlation structure?**

If you are unsure which structure your dataset has, a handy way of figuring it out is to run multiple gls models which different autocorrelation structures, then compute the **AIC** for each of these models.

<a name="AIC"></a>**What is the AIC?**
If you don't know what the AIC is, it is a function used to understand how well your model fits the data. To find out the AIC of your model, run AIC() with the name of your model between the brackets. The AIC() value is a **relative measure**, so you need two AIC() values to get a meaningful interpretation. A model with a lower AIC value fits the data better than a model with a higher AIC value that explains the same data.

So after running multiple models and AIC values, pick the model with the **lowest AIC** and whatever the correlation structure you used for this model is likely the one in your data.

For example, lets run a gls pretending we have a MA order 1 structure and compare AIC's.

```r
model2<-gls(Ozone~DayOfYear, data=data3,
            correlation = corARMA(form=~DayOfYear, q=1))

#Compute AIC for model and model2
AIC(model)
AIC(model2)
```

The AIC for 'model' is **1105.637** and the AIC for 'model2' is **1114.702**, so 'model' fits the data better (because we used the correct correlation structure).

For comparison, let's run a **linear model** on the data, and look at the AIC.

```r
lmod<-lm(Ozone ~ DayOfYear, data = data3) #Linear model
AIC(lmod)
```
The AIC is **1142.392** for the linear model, which is much higher than the AIC for 'model' and 'model2'. 

Let's compare the model summary of 'model' and 'lmod'.

```r
summary(lmod) #the summary statistics of the linear model
summary(model) #the summary statistics of the gls model
```
The output of the model's should be as shown below:

**Linear model:**

|            | Value     | Std.Error | t-value   | p-value |
|-----------|----------|------------|---------|----------|
| (Intercept)| 19.31968 | 13.76113   | 1.404   | 0.163   |
| DayOfYear  | 0.11237  | 0.06612    | 1.699   | 0.092   |



**GLS model:**

|            | Value     | Std.Error | t-value   | p-value |
|------------|-----------|-----------|-----------|---------|
| (Intercept)| 27.690038 | 32.21383  | 0.8595699 | 0.3918  |
| DayOfYear  | 0.072848  | 0.15822   | 0.4604161 | 0.6461  |

We can see that neither the linear model now the gls model show statistical significance when looking at the p-value and using the threshold value of 0.05 (both p-values are above 0.05. However, the **linear model** does show a **p value much smaller** than that in the GLS model, which demonstrates how without accomodating for autocorrelation in your models it would be possible to find a statistically significant relationship between two variables where none exists.

### Explaining outputs of the GLS model

The GLS model summary also included the following parameters:

**ϕ (Phi) parameter**

In this GLS model we are given 3 ϕ values.
| Phi1       | Phi2       | Phi3       |
|------------|------------|------------|
| 0.41971676 | 0.06503704 | 0.22284351 |


This is because these values represent the **correlation parameters**. They are the values used by the model to weight the values at lag 1, lag 2 and lag 3, diminishing their impact on the best-fitted line to account for the temporal autocorrelation. We are given 3 values because we stated **'p=3'** in our model correlation structure. The number of ϕ values returned will mirror the specified p or q in your correlation structure.

The output of the model also includes the **residual standard error**. This is a measure of the difference between observed values and those values predicted by our model. We want to residual standard error to be small, as this signifies the model fits the data well. Our residual standard error is very large, which suggests something else is going wrong in the model. We shall explore the statistical assumptions of the model below.

## Checking the statistical assumptions of the GLS model.
The gls model still assumes **homoskedasticity** (constant variance) unless non-constant variance is accounted for in the model. Plotting the model we can see that variance is **not constant**:

```r
plot(model)
```

Therefore, we should add something to the GLS model to account for this non-constant variance, which we shall do in the section below.

Finally, I want to end this section on how to quickly check all your variables for temporal autocorrelation. A good, speedy way to do this is to plot the acf() for each variable and see if any of the vertical lines surpass the dotted line. Remember, acf() and pacf() does not like it if there are NA values in the vector you are interested in. You can alter the code as follows to tell the function to ignore NA values.

However, it is better to go through the steps we did earlier, as just removing NA values without understanding how it will impact the dataset is dangerous as it might alter your dataset in a way that affects your statistical analysis (for example if values for a whole month were missing).

```r
pacf(data3$Ozone, na.action = na.pass, lag.max=31)
acf(data3$Ozone, na.action = na.pass, lag.max=31)
```


## Help! What should I do when I have heteroskedasticity? <a name="section3"></a>

**Is my model heteroskedastic?**

**Heteroskedasticity** is a feature of a model where the **residuals** do **not have constant variance**. As constant variance is an assumption of linear models, it can give your statistical model less confidence if variance is not constant.

 You can acertain is you have heteroskedasticity visually or numerically.
To work out if you have non constant variance **numerically**, you can run the **Breusch-Pagan Test** on your linear model. We shall use the linear model we created above:

```r
#Linear model
lmod<-lm(Ozone ~ DayOfYear, data = airdata) 
```

For this test you need the package **lmtest**, which we downloaded at the start of the tutorial.

```r
#Breusch-Pagan Test
bptest(model)
```
If the result of the bptest is **lower** than 0.05 there is **heteroskedasticity**.

To check for non-constant variance **visually** you look at the **Residuals vs fitted** and **Scale-Location** plots. This is one of the diagnostic plots of the model and can be accessed as so:

```r
#For our linear model
plot(lmod, which = 1) #This is the 'residuals vs fitted' plot
plot(lmod, which=3) #This is the 'scale-location' plot
```
If you observe a pattern or a **non-constant spread** of points in either plot, your model has non constant variance in the residuals. 

## Ok, so now what do we do?
One of the statistical assumptions of linear models is homoskedasticity, ie. constant variance. Therefore we need to remedy our non constant variance.
We can do this by adding a variance function to the **'weights'** part of the gls model.

The gls model has the following structure:

`gls(model, data, correlation, weights, subset, method, na.action,
    control, verbose)`

In the 'weights' section we add a variance function which describes the **pattern** our variances show. The model then works in a similar way to how it worked with autocorrelation, which is adding values to the model to discount certain variances and transform the data to have **constant variance**.

**How do I know which variance structure I have?**

There are many different **variance structures**, but some of the most common are **`varPower(), varExp() and varIdent()`**.
There are two ways to decide which variance structure you should use.
The first, and simplest is to run multiple models with different variance structures and pick the model with the lowest AIC (as this indicates the best fit, look <a href="#AIC"> here</a> for an explanation on AIC).

The second way is to look at the **'Residuals vs fitted plot'** and the **'Scale-Location'** plot of the linear model.

For **`varPower()`**
The plot has a fan like shape, with the variance changing as the fitted value increases.
In the 'Scale-Location' plot you may see a systematic change in the spread of residuals along the fitted values, but less sudden than might been seen in a exponential structure (explained below).

For **`varExp()`**
It is similar to the pattern for `varPower()`, but the variance increases exponentially with increased fitted value.
In the 'Scale-Location' plot you may see a systematic change in the spread of residuals along the fitted values.

For **`varIdent()`**
The variance should not change as fitted value increases, however there is still non-constant variance, for example higher magnitude in positive variance compared to negative variance. 
Again, we should see a constant spread of residual along the fitted values in the 'Scale-Location' plot.

Inside the brackets put `form=~DayOfYear` which says we expect the non-constant variance to be due to the 'DayOfYear' variable, which we will assume for this tutorial. Obviously non-constant variance can be due to other variables, however this gets quite complicated but if you are interested I recommend the following websites.

Our linear model shows a **mild fan shape** in the 'residuals-fitted' plot which indicates **`varPower()`**.

<p align="center">
  <img src="Images/PACF.png" alt="PACF">
</p>

Let's add this section of code to our model, and compare the `AIC()` to see if the model fit has improved.

```r
model3<-gls(Ozone ~ DayOfYear, data = data3,
            correlation = corARMA(form=~DayOfYear, p=3),weights=varPower(form=~DayOfYear))

AIC(model2)
AIC(model3)
```
Indeed, whilst the AIC value didn't change by much, it did **decrease** which shows our addition of a variance structure improved the fit of the model.

Be aware that there are other ways to deal with heteroskedasticity which is not covered here but which may be useful for your dataset. One easy way is to transform the data, by taking the logarithm, square root or cubed root for example. More detail is found [here](https://www.bookdown.org/rwnahhas/RMPH/mlr-constant-variance.html).

## <a name="section2"></a> Spatial autocorrelation
### What is spatial autocorrelation and why is it a problem?
Almost all statistical models have the statistical assumption **independence of data points** however, not all datasets meet this assumption. **Spatial autocorrelation** is a specific type of non-independence where variable values which are geographically close to each other are more similar than those further away (or vice versa in some cases). 

It is important to check your data for spatial autocorrelation, as it can influence statistical tests and lead to an apparent relationship which is not there in reality.
In this tutorial we will look at how to **check your data** for spatial autocorrelation, as well as what to do if it is there. Because- don't fret! - you can still run certain statistical tests on spatially autocorrelated data.

Spatial autocorrelation comes up frequently in a certain type of ecological sampling, that of quadrat sampling along a transect. If you are not an ecologist, here is a brief overview of what this type of sampling entails.

A quadrat is a square (often 1m x 1m) which is placed on the ground. The things in the quadrat are then counted and recorded. Other measurements may also be made within the quadrat. Repeat quadrat measurements are made along a transect, which is a predefined line that the person doing the sampling walks down, making quadrat measurements at certain distances. Spatial autocorrelation comes up when either your transects are two close together, or your quadrats are. We will be using data attained by this method in this tutorial.

#### Explore the data

For this part of the tutorial we will be using a different dataset, as the dataset 'airquality' does not have spatial autocorrelation. The data we will be using in this tutorial was collected by myself and coursemates Luisa Dickenmann, Rebecca Hies, Kinga Kaszap, Thomas Pendlebury, Else Radeloff and Tegan Williams in the Cairngorms. We were exploring how light availability affects understory plant diversity in forests. 


<p align="center">
  <img src="Images/Images/Scotspine.png" >
</p>

We collected the plant diversity data by laying out 150m long transects and taking quadrat measurements every 30 m. Each transect was at least 30m away from every other transect. The column which contains the plant diversity data is called 'exp_shannon'.

**Let's load the data**
```r
#Load the data
data <- read.csv("Data/data.csv", header=TRUE)
```


#### How to check for spatial autocorrelation
To check for spatial autocorrelation we depend on the decimal **latitude** and **longitude** values we recorded for each quadrat. The package **geosphere** in r helpfully calculates the distance between each quadrat using the latitude and longitude.

We will use the function distm() in geosphere to calculate the distances between points. Within distm() you have to specify the function you wish it to use. Here we will use the 'distGeo' function which indicates the 'Haversine formula' as we are calculating distances on a sphere (the earth).

We also have to specify the data. The data we are using is in our 'data' dataframe, and we want the function to use the latitude and longitude values, so we specify this in the code. We will save the distances in a vector called **spatial**.

Here is the code to calculate **distances between quadrats**:
```r
spatial <- distm(fun = distGeo, x = data [,c ("lat", "long")] ) 
```

We can check for spatial autocorrelation **numerically** using **Moran's I**. This is a numerical test. The output has a p-value. If this value is **smaller** than 0.05, it indicates you can reject the null hypothesis of **no** spatial autocorrelation. 

To check for spatial autocorrelation we calculate Moran's I, using the following code:
```r
`Moran.I(x = data_final$exp_shannon, spatial )`
```
This code is saying calculate 'Moran's I' for the column 'exp_shannon' (ie. plant diversity), and see whether the diversity values are custered spatially (based on the **spatial** vector).

Our **p-value** for this dataset is **0.0001600935**, so there is spatial autocorrelation.

The other output values
------------------------
`observed: -0.1825165` 
This is the observed value of Moran's I. It is always between 1 and -1. A positive value indicates positive spatial autocorrelation (spatially near measurements are more similar) and a negative value indicates negative spatial autocorrelation (spatially near measurements are more dissimilar).

`expected: -0.03448276`
This is the value of Moran's I expected under the assumption of spatial randomness. 

`sd = standard deviation: 0.03921563`
This is the standard deviation of Moran's I under the assumption of spatial randomness. It allows you to compare the expected and observed Moran's I, to see if what the observed Moran's I is telling you is trustworthy. Since the observed Moran's I is bigger than 'expected' + 'sd' and 'expected' - 'sd', we can trust the conclusion the Moran's I tells us.

What to do when you have found spatial autocorrelation
-------------------------------------------------------
The first thing it is good to do is plot a semivariogram. 
A semivariogram is a graph with 'separation distance' along the x axis and 'semivariance' along the y axis. 

**Semivariograms: What is semivariance and how do I plot a semivariogram?**
Semivariance is the average of the variability of all pairs of points a set 'x' distance apart. Plotting this against separation distance (is the different 'x's) allows you to visualise how the variability of the variable changes with distance.

Plotting a semivariogram allows you to see at what distance there is no longer spatial autocorrelation, as well as other useful information.
To plot a semivariogram in r we use the following code. We shall store it as an object called 'semivariogram'.
```r
semivariogram<- semivariogram(x = data_final$lat, y = data_final$long, z = data_final$exp_shannon)
```
Which gives us this:

<p align="center">
  <img src="Images/Rplot.png" alt="Semivariogram Plot">
</p>

This plot shows **semivariance** on the y axis and **separation distance** on the x axis. The units of separation distance as **kilometres**, as we scaled out latitude and longitude values, however the default is whatever units your coordinates are in.

#### How to interpret the semivariogram
--------------------------------------------

In spatially correlated data the semivariance gradually increases with increasing lag distance up to a certain value, where the graph then levels out horizontally. The **sill** is the maximum variability, and the semivariance at which the graph levels off. At this variability, the separation distance at this point is no longer spatially autocorrelated. This information is useful, as it tells you the ideal separation distance of your sites if you were to repeat the experiment.

The point at which the graph intersects th y axis is the **nugget**. This is the point of zero separation distance- so surely it would have zero semivariance? In fact this is rarely the case, and can tell you the underlying variation in the data not accounted for by spatial patterns (perhaps due to measurement error or finer resolutions). 

The **range** in a semivariogram is the span of separation distance's for which points are correlated.

The **nugget to sill ratio** is an important parameter in understanding spatial autocorrelation. 

**<0.25** = **strong** spatial dependence
**0.25-0.75** = **moderate** spatial dependence
**1** = **no** spatial dependence.

Let's calculate the **nugget to sill ratio**.
First we need to extract these values from our semivariogram. We can do this by recalling out semivariogram object.

```r
semivariogram
```

And we get this:
| Sill       | Nugget     | Range      |
|------------|------------|------------|
| 1.2715226  | 0.3310921  | 0.1584368  |

Calculate our nugget to sill ratio with the following code:
```r
0.3310921/1.2715226
```
This gives us **0.2603903** which indicates **strong** spatial autocorrelation.

## How to incorporate spatial autocorrelation into your gls model
We add the following code to our gls model

```r
mod_gls <- gls( exp_shannon ~ LAI, data = data_final, correlation = corSpatial(form = ~ lat + long, 
                                                                               nugget = T ) )
```
Where 'corSpatial' indicates the correlation structure, 'form' indicates the columns containing our spatial coordinates and 'nugget=TRUE' because our nugget was not equal to zero, and we want the model to take account of this.
#
<style>
.alert-info {
  color: rgb(49,112,143) !important;
}
</style>
