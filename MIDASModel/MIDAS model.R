setwd("~/MIDAS GDP Model")
library(xlsx)

HOW do I use all the available data even if they start at different points?
An Auto regressive MIDAS model that uses?

#Transform Construction Data
construction<- read.xlsx('Construction_Spending.xls', sheetIndex=1, sheetName=NULL, rowIndex=NULL,
                startRow=11, colIndex=2,
                as.data.frame=TRUE, header=TRUE)

construction_ts<- ts(construction,start = c(1993, 1), frequency=12)
colnames(construction_ts)<- c('construction')
  
#Employment data
employment<- read.xlsx('TotalNonfarmPayroll.xls', sheetIndex=1, sheetName=NULL, rowIndex=NULL,
                startRow=108, colIndex=2,
                as.data.frame=TRUE, header=FALSE)

employment_ts<- ts(employment,start = c(1947, 1), frequency=12)
colnames(employment_ts) <- c('employment')

#GDP data
#This is set up to specifically work with data from Septemeber 2015.
#need to universilize it so it can work in the future.
gdp<- read.xlsx('GDP.xls', sheetIndex=1, sheetName=NULL, rowIndex=NULL,
                         startRow=11, endRow=929, colIndex=2,
                         as.data.frame=TRUE, header=TRUE)

gdp_ts<- ts(gdp,start = c(1947, 1), frequency = 4)
colnames(gdp_ts)<- c('gdp')

#Data Set for Linear Regression Models with Autoregressive Errors
library(zoo)
construction_quarters<- rollapply(construction[,1], width=3, mean, by=3)
#The numbers for September aren't in yet for Construction spending, so in this case, I average the last two values together an apend it to the end of the construction_quarters for a complete data set.
extra_value<- mean(construction[271:272,1])
construction_quarters<- append(construction_quarters, extra_value)
employment_quarters<- rollapply(employment[,1], width=3, mean, by=3)


#Double Check the function Worked
head(construction_quarters)
construction_quarters[1] == mean(construction[1:3,1])
construction_quarters[2] == mean(construction[4:6,1])

employment_quarters[1] == mean(employment[1:3,1])
employment_quarters[2] == mean(employment[4:6,1])

#Merge the Datasets
library(dplyr)

