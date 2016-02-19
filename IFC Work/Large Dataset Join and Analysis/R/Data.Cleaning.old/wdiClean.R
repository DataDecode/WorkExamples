library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

wdi <- read.csv('Data/ExternalDBs/WDI_Data.csv', 
                sep=',')

# strip out all of the uneeded years 
# 1960-2003

# add this in , Indicator.Code
wdi<- select(wdi, Country.Name, Country.Code, Indicator.Name, X2004, X2005, 
             X2006, X2007, X2008, X2009, X2010, X2011, X2012, X2013, X2014)


#There are a huge number of attibutes for each country.
#Below is a list of the attibutes we want and we use lapply to 
#remove these attribues for each the regions and the countries.


wdi.list<- c('Bank nonperforming loans to total gross loans (%)',
             'Borrowers from commercial banks (per 1,000 adults)',
             'Deposit interest rate (%)',
             'Depositors with commercial banks (per 1,000 adults)',
             'Depth of credit information index (0=low to 8=high)',
             'Domestic credit provided by financial sector (% of GDP)',
             'Domestic credit to private sector (% of GDP)',
             'Domestic credit to private sector by banks (% of GDP)',
             'Firms using banks to finance investment (% of firms)',
             'Firms using banks to finance working capital (% of firms)',
             'GDP (constant 2005 US$)',
             'GDP (constant LCU)',
             'GDP (current LCU)',
             'GDP (current US$)',
             'GDP deflator (base year varies by country)',
             'GDP growth (annual %)',
             'GDP per capita (constant 2005 US$)',
             'GDP per capita (constant LCU)',
             'GDP per capita (current LCU)',
             'GDP per capita (current US$)',
             'GDP per capita growth (annual %)',
             'GDP per capita, PPP (constant 2011 international $)',
             'GDP per capita, PPP (current international $)',
             'GDP, PPP (constant 2011 international $)',
             'GDP, PPP (current international $)',
             'GNI (constant 2005 US$)',
             'GNI (constant LCU)',
             'GNI (current LCU)',
             'GNI (current US$)',
             'GNI growth (annual %)',
             'GNI per capita (constant 2005 US$)',
             'GNI per capita (constant LCU)',
             'GNI per capita (current LCU)',
             'GNI per capita growth (annual %)',
             'GNI per capita, Atlas method (current US$)',
             'GNI per capita, PPP (constant 2011 international $)',
             'GNI per capita, PPP (current international $)',
             'GNI, Atlas method (current US$)',
             'GNI, PPP (constant 2011 international $)',
             'GNI, PPP (current international $)',
             'Lending interest rate (%)',
             'Mobile cellular subscriptions',
             'Mobile cellular subscriptions (per 100 people)',
             'Official exchange rate (LCU per US$, period average)',
             'Population ages 0-14 (% of total)',
             'Population ages 15-64 (% of total)',
             'Population ages 65 and above (% of total)',
             'Population, total',
             'Public credit registry coverage (% of adults)',
             'Real interest rate (%)')


#Countries
wdi.countries <- wdi[-c(1:45730),]

wdi.countries.total<- NULL
lapply(wdi.list, function(x){
  wdi.x<- filter(wdi.countries, Indicator.Name == x)
  wdi.countries.total <<- rbind(wdi.countries.total, wdi.x)
})

#pulled out the attribues, but the number of factors is still listed at the total number
wdi.countries.total$Indicator.Name <- as.character(wdi.countries.total$Indicator.Name) %>%
  as.factor()

#wdi.countries.total$Indicator.Code <- as.character(wdi.countries.total$Indicator.Code) %>%
  #as.factor()

#now we're breaking it down into years to reshape data frame
#this should be looped
wdi.2004 <- wdi.countries.total[,c(1:4)]
wdi.2004 <- spread(wdi.2004, key = Indicator.Name, value = X2004)
wdi.2004 <- mutate(wdi.2004, Year = '2004')

wdi.2005 <- wdi.countries.total[,c(1:3,5)]
wdi.2005 <- spread(wdi.2005, key = Indicator.Name, value = X2005)
wdi.2005 <- mutate(wdi.2005, Year = '2005')

write.csv(wdi.2005, 'Clean.data/test2005.csv')
wdi.2006 <- wdi.countries.total[,c(1:3,6)]
wdi.2006 <- spread(wdi.2006, key = Indicator.Name, value = X2006)
wdi.2006 <- mutate(wdi.2006, Year = '2006')

wdi.2007 <- wdi.countries.total[,c(1:3,7)]
wdi.2007 <- spread(wdi.2007, key = Indicator.Name, value = X2007)
wdi.2007 <- mutate(wdi.2007, Year = '2007')

wdi.2008 <- wdi.countries.total[,c(1:3,8)]
wdi.2008 <- spread(wdi.2008, key = Indicator.Name, value = X2008)
wdi.2008 <- mutate(wdi.2008, Year = '2008')

wdi.2009 <- wdi.countries.total[,c(1:3,9)]
wdi.2009 <- spread(wdi.2009, key = Indicator.Name, value = X2009)
wdi.2009 <- mutate(wdi.2009, Year = '2009')

wdi.2010 <- wdi.countries.total[,c(1:3,10)]
wdi.2010 <- spread(wdi.2010, key = Indicator.Name, value = X2010)
wdi.2010 <- mutate(wdi.2010, Year = '2010')

wdi.2011 <- wdi.countries.total[,c(1:3,11)]
wdi.2011 <- spread(wdi.2011, key = Indicator.Name, value = X2011)
wdi.2011 <- mutate(wdi.2011, Year = '2011')

wdi.2012 <- wdi.countries.total[,c(1:3,12)]
wdi.2012 <- spread(wdi.2012, key = Indicator.Name, value = X2012)
wdi.2012 <- mutate(wdi.2012, Year = '2012')

wdi.2013 <- wdi.countries.total[,c(1:3,13)]
wdi.2013 <- spread(wdi.2013, key = Indicator.Name, value = X2013)
wdi.2013 <- mutate(wdi.2013, Year = '2013')

wdi.2014 <- wdi.countries.total[,c(1:3,14)]
wdi.2014 <- spread(wdi.2014, key = Indicator.Name, value = X2014)
wdi.2014 <- mutate(wdi.2014, Year = '2014')

wdi.countries<- rbind(wdi.2004,wdi.2005,wdi.2006, wdi.2007, 
                      wdi.2008, wdi.2009, wdi.2010, wdi.2011,
                      wdi.2012, wdi.2013, wdi.2014)

wdi.countries<- arrange(wdi.countries, Country.Name, Year)

# Cleaning up the 
x<- c('Country.Name',
      'Country.Code',
      'Bank.nonperforming.loans.to.total.gross.loans.Percent',
      'Borrowers.from.commercial.banks.per1kAdults',
      'Deposit.interest.rate.Percent',
      'Depositors.with.commercial.banks.per1kAdults',
      'Depth.of.credit.information.index.0=low.8=high',
      'Domestic.credit.provided.by.financial.sector.PercentofGDP',
      'Domestic.credit.to.private.sector.by.banks.PercentofGDP',
      'Domestic.credit.to.private.sector.PercentofGDP',
      'Firms.using.banks.to.finance.investment.Percent.Firms',
      'Firms.using.banks.to.finance.working.capital.Percent.Firms',
      'GDP.constant.2005.USD',
      'GDP.constantLCU',
      'GDP.currentLCU',
      'GDP.current.USD',
      'GDP.deflator.varies.by.year',
      'GDP.growth.AnnualPercent',
      'GDP.per.capita.constant.2005.USD',
      'GDP.per.capita.constantLCU',
      'GDP.per.capita.currentLCU',
      'GDP.per.capita.current.USD',
      'GDP.per.capita.growth.AnnualPercent',
      'GDP.per.capita.PPP.constant.2011.international.Money',
      'GDP.per.capita.PPP.current.International.Money',
      'GDP.PPP.2011.international.Money',
      'GDP.PPP.current.International.Money',
      'GNI.Atlas.method.current.USD',
      'GNI.constant.2005.USD',
      'GNI.constantLCU',
      'GNI.currentLCU',
      'GNI.current.USD',
      'GNI.growth.AnnualPercent',
      'GNI.per.capita.Atlas.method.current.USD',
      'GNI.per.capita.constant.2005.USD',
      'GNI.per.capita.constantLCU',
      'GNI.per.capita.currentLCU',
      'GNI.per.capita.growth.AnnualPercent',
      'GNI.per.capita.PPP.constant.2011.international.Money',
      'GNI.per.capita.PPP.current.International.Money',
      'GNI.PPP.constant.2011.international.Money',
      'GNI.PPP.current.International.Money',
      'Lending.interest.rate.Percent',
      'Mobile.cellular.subscriptions',
      'Mobile.cellular.subscriptions.per.100.people',
      'Official.exchange.rate.LCU.Per.USD.period.Average',
      'Population.ages.0-14.Percent.Total',
      'Population.ages.15-64.Percent.Total',
      'Population.ages.65.and.above.Percent.Total',
      'Population.total',
      'Public.credit.registry.coverage.Percent.Adults',
      'Real.interest.rate.Percent',
      'Reporting.Year')

length(x) == length(colnames(wdi.countries))

colnames(wdi.countries) <- x

# this list should be in every file to clean the country names
require(stringr)
wdi.countries$Country.Name <-str_trim(wdi.countries$Country.Name)

#adding country code
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
wdi.countries$Country.Code <- NULL
wdi.countries <- left_join(wdi.countries, countries, 'Country.Name')
wdi.countries$Country.Code.Year <- paste(wdi.countries$Country.Code, wdi.countries$Reporting.Year, sep='')


write.csv(wdi.countries, file = 'Clean.data/wdiCountries.csv')





#NEED TO WRITE REGION LAPPLY LOOP AND LOOP YEAR CALCULATIONS



#Take off Region
wdi.regions <- wdi[c(1:45730),]

wdi.regions.conversion<- filter(wdi.regions, Indicator.Name == "2005 PPP conversion factor, GDP (LCU per international $)")
wdi.regions.savings <- filter(wdi.regions, Indicator.Name == "Gross domestic savings (% of GDP)")
wdi.regions.pop <- filter(wdi.regions, Indicator.Name =='Population, total')
wdi.regions.ppp <- filter(wdi.regions, Indicator.Name =='Price level ratio of PPP conversion factor (GDP) to market exchange rate')

wdi.regions <- rbind(wdi.regions.conversion, wdi.regions.savings, wdi.regions.pop, wdi.regions.ppp)