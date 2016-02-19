library(openxlsx)
library(dplyr)
library(magrittr)
library(reshape2)
library(tidyr)

#IMF Financial Access Survey
fas<- read.xlsx('Data/ExternalDBs/FAS Data Refresh.xlsx', 
                        startRow= 3)

#All the names and the Country codes are in every row in the hidden columns 1 and 2.
#this means we can delete 'Row.Labels" and the second "ISO.Code" column (column 3 and 4).
fas$Row.Labels <- NULL
fas[,3] <- NULL #Used the column number because it was named the same as another column

# Global Findex (WB)
# Downloaded All Countries and all attribues for 2014 survey
findex <- read.csv('Data/ExternalDBs/Data_Extract_From_Global_Findex_(Global_Financial_Inclusion_Database)_Data.csv', sep=',')

#pull out just 'wave 2', or the 2014 data for analysis
findex <- filter(findex, grepl('w2', Series.Name))

#Delete unneeded column
findex$Series.Code <- NULL

#swing the data so that each country is a row and all the measured attribues are columns
findex<- spread(findex, key = Series.Name, value = X2014..YR2014.)



#IFC Enterprise Finance Gap Database
gap <- read.csv('Data/ExternalDBs/IFC_Enterprise_Finance_Gap_Database_-_Raw_Data.csv', 
                sep=',')



# WDI - World Development Indicators
wdi <- read.csv('Data/ExternalDBs/WDI_Data.csv', 
                sep=',')

# strip out all of the uneeded years 
# 1960-2003
#also remove Indicator.Code
wdi<- select(wdi, Country.Name, Country.Code, Indicator.Name, X2004, X2005, 
             X2006, X2007, X2008, X2009, X2010, X2011, X2012, X2013, X2014)

#take out just the atributes we want (I NEED TO KNOW WHAT VALUES WE WANT)
#The below is just an example

#take of regions
wdi.regions <- wdi[c(1:45730),]

wdi.regions.conversion<- filter(wdi.regions, Indicator.Name == "2005 PPP conversion factor, GDP (LCU per international $)")
wdi.regions.savings <- filter(wdi.regions, Indicator.Name == "Gross domestic savings (% of GDP)")
wdi.regions.pop <- filter(wdi.regions, Indicator.Name =='Population, total')
wdi.regions.ppp <- filter(wdi.regions, Indicator.Name =='Price level ratio of PPP conversion factor (GDP) to market exchange rate')

wdi.regions <- rbind(wdi.regions.conversion, wdi.regions.savings, wdi.regions.pop, wdi.regions.ppp)

#countries
wdi.countries <- wdi[-c(1:45730),]

wdi.conversion<- filter(wdi.countries, Indicator.Name == "2005 PPP conversion factor, GDP (LCU per international $)")
wdi.savings <- filter(wdi.countries, Indicator.Name == "Gross domestic savings (% of GDP)")
wdi.pop <- filter(wdi.countries, Indicator.Name =='Population, total')
wdi.ppp <- filter(wdi.countries, Indicator.Name =='Price level ratio of PPP conversion factor (GDP) to market exchange rate')

wdi.countries <- rbind(wdi.conversion, wdi.savings, wdi.pop, wdi.ppp)

#pulled out the attribues, but the number of factors is still listed at the total number
wdi.countries$Indicator.Name <- as.character(wdi.countries$Indicator.Name) %>%
  as.factor()
wdi.countries$Indicator.Code <- as.character(wdi.countries$Indicator.Code) %>%
  as.factor

#now we're breaking it down into years to reshape data frame
#this should be looped
wdi.2004 <- wdi.countries[,c(1:4)]
wdi.2004 <- spread(wdi.2004, key = Indicator.Name, value = X2004)
wdi.2004 <- mutate(wdi.2004, Year = '2004')

wdi.2005 <- wdi.countries[,c(1,2,3,5)]
wdi.2005 <- spread(wdi.2005, key = Indicator.Name, value = X2005)
wdi.2005 <- mutate(wdi.2005, Year = '2005')

wdi.2006 <- wdi.countries[,c(1,2,3,6)]
wdi.2006 <- spread(wdi.2006, key = Indicator.Name, value = X2006)
wdi.2006 <- mutate(wdi.2006, Year = '2006')

wdi.2007 <- wdi.countries[,c(1,2,3,7)]
wdi.2007 <- spread(wdi.2007, key = Indicator.Name, value = X2007)
wdi.2007 <- mutate(wdi.2007, Year = '2007')

wdi.2008 <- wdi.countries[,c(1,2,3,8)]
wdi.2008 <- spread(wdi.2008, key = Indicator.Name, value = X2008)
wdi.2008 <- mutate(wdi.2008, Year = '2008')

wdi.2009 <- wdi.countries[,c(1,2,3,9)]
wdi.2009 <- spread(wdi.2009, key = Indicator.Name, value = X2009)
wdi.2009 <- mutate(wdi.2009, Year = '2009')

wdi.2010 <- wdi.countries[,c(1,2,3,10)]
wdi.2010 <- spread(wdi.2010, key = Indicator.Name, value = X2010)
wdi.2010 <- mutate(wdi.2010, Year = '2010')

wdi.2011 <- wdi.countries[,c(1,2,3,11)]
wdi.2011 <- spread(wdi.2011, key = Indicator.Name, value = X2011)
wdi.2011 <- mutate(wdi.2011, Year = '2011')

wdi.2012 <- wdi.countries[,c(1,2,3,12)]
wdi.2012 <- spread(wdi.2012, key = Indicator.Name, value = X2012)
wdi.2012 <- mutate(wdi.2012, Year = '2012')

wdi.2013 <- wdi.countries[,c(1,2,3,13)]
wdi.2013 <- spread(wdi.2013, key = Indicator.Name, value = X2013)
wdi.2013 <- mutate(wdi.2013, Year = '2013')

wdi.2014 <- wdi.countries[,c(1,2,3,14)]
wdi.2014 <- spread(wdi.2014, key = Indicator.Name, value = X2014)
wdi.2014 <- mutate(wdi.2014, Year = '2014')

wdi.countries<- rbind(wdi.2004,wdi.2005,wdi.2006, wdi.2007, 
                      wdi.2008, wdi.2009, wdi.2010, wdi.2011,
                      wdi.2012, wdi.2013, wdi.2014)

wdi.countries<- arrange(wdi.countries, Country.Name, Year)
