Cleaning the Data
========================================================


This example assumes that the appropriate data is loaded into R already.  Unfortunately this example assumes that this is just being used for Q3 2015 analysis, so the code would need to be edited in the future 


```r
setwd("~/MIDAS GDP Model")
library(xlsx)
```

```
## Loading required package: rJava
## Loading required package: xlsxjars
```

```r
construction<- read.xlsx('Construction_Spending.xls', sheetIndex=1, sheetName=NULL, rowIndex=NULL, startRow=11, colIndex=2, as.data.frame=TRUE, header=TRUE)

employment<- read.xlsx('TotalNonfarmPayroll.xls', sheetIndex=1, sheetName=NULL, rowIndex=NULL,
                startRow=108, colIndex=2,
                as.data.frame=TRUE, header=FALSE)

gdp<- read.xlsx('GDP.xls', sheetIndex=1, sheetName=NULL, rowIndex=NULL,
                         startRow=11, endRow=929, colIndex=2,
                         as.data.frame=TRUE, header=TRUE)
```

###Data Set for Linear Regression Models with Autoregressive Errors###


```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
construction_quarters<- rollapply(construction[,1], width=3, mean, by=3)
#The numbers for September aren't in yet for Construction spending, so in this case, I average the last two values together an apend it to the end of the construction_quarters for a complete data set.
extra_value<- mean(construction[271:272,1])
construction_quarters<- append(construction_quarters, extra_value)


employment_quarters<- rollapply(employment[,1], width=3, mean, by=3)
```

Quick test that it worked properly

```r
construction_quarters[1] == mean(construction[1:3,1])
```

```
## [1] TRUE
```

```r
construction_quarters[2] == mean(construction[4:6,1])
```

```
## [1] TRUE
```

```r
employment_quarters[1] == mean(employment[1:3,1])
```

```
## [1] TRUE
```

```r
employment_quarters[2] == mean(employment[4:6,1])
```

```
## [1] TRUE
```

We then set all data sets as time series before we begin analysis


```r
construction_q_ts<- ts(construction_quarters, start = c(1993, 1), frequency=4)
employment_q_ts<- ts(employment_quarters, start = c(1947, 1), frequency=4)
gdp_q_ts<- ts(gdp,start = c(1947, 1), frequency = 4)

colnames(construction_q_ts)<- c('construction')
```

```
## Error in `colnames<-`(`*tmp*`, value = "construction"): attempt to set 'colnames' on an object with less than two dimensions
```

```r
colnames(employment_q_ts) <- c('employment')
```

```
## Error in `colnames<-`(`*tmp*`, value = "employment"): attempt to set 'colnames' on an object with less than two dimensions
```

```r
colnames(gdp_q_ts)<- c('gdp')
```

###Removing Autocorrelation in GDP###

The plot for GDP is not covariant stationary, but the first differincing is worth checking:

```r
plot(gdp_q_ts)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
plot(log(gdp_q_ts))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png) 

```r
plot(diff(gdp_q_ts))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-3.png) 

```r
gdp_diff<- diff(gdp_q_ts)

library(tseries)
adf.test(diff(gdp_q_ts), k = 8)
```

```
## Warning in adf.test(diff(gdp_q_ts), k = 8): p-value smaller than printed p-
## value
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  diff(gdp_q_ts)
## Dickey-Fuller = -4.3361, Lag order = 8, p-value = 0.01
## alternative hypothesis: stationary
```

The Augmented Dickey-Fuller Test shows that the differenced GDP is stationary. 

```r
acf(gdp_diff)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
pacf(gdp_diff)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-2.png) 

The acf test shows a steady decline, which indicates AR is present.  The PACF cuts off after 3 lags though some longer lags show signifigance.  This is likely due to seasonality in the data.


```r
ar_3<- arima(gdp_diff, order= c(3,0,0))
```


The first differencing is the difference in GDP from the previous period.  Even the log of the first difference shows a pretty clear break in the data in the early 80s.  In the hopes of keeping the useful data, a local autocovariance model is used.

library(forecast)

1. local autocovariance model is used
2. add in the time series regression with the local autocovariance
3.


