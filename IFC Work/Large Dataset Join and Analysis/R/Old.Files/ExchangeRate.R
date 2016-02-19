#pulling out Exchange rates for the years needed
wdi.exchange<- read.csv('Clean.data/wdiCountries.csv', sep = ',')
wdi.exchange <- select(wdi.exchange, Country.Code.Year,
                       contains('Official.Exchange'))


#The exchange rates I needed to change
wdi.exchange[617:627,2] <- 1 # ECU2007 - 2014
wdi.exchange[803,2] <- 3.204 # GHA2014
wdi.exchange[1177,2] <- .5788 # LVA2014
wdi.exchange[1251:1254,2] <- c(30.9104, 30.5344, 29.4169, 33.22) # LUX2011 - 2014
wdi.exchange[1865:1867,2] <- c(21.000, 22.952, 23.084) #SVK2009 - 2011
wdi.exchange[2267:2277,2] <- c(NA, NA, NA, 1286.13, 1317.92, 1464.24,
                               1587.57, 1716.41, 1893.32, 
                               2106.34, 2322.66) # UZB2004 - 2014

wdi.exchange[912:913, 2] <- c(20.2402,21.0493) #HND2013 -2014
wdi.exchange[1126, 2] <- NA  #KSV2007
wdi.exchange[1210, 2] <- 91.5000  #LBR2014



write.csv(wdi.exchange, "Clean.data/Combined.data/exchangerate.csv")

#hard entering missing data that is needed for calculations
#ccyear <- c('ECU2004', 'ECU2005', 'ECU2006',
#'ECU2007', 'ECU2008', 'ECU2009',
#'ECU2010', 'ECU2013', 'ECU2014',
#'GHA2014', 
#'LVA2014',
#'LUX2011', 'LUX2012', 'LUX2013',
#'LUX2014',
#'SVK2009', 'SVK2010', 'SVK2011',
#'UZB2004', 'UZB2005', 'UZB2006',
#'UZB2007', 'UZB2008', 'UZB2009',
#            'UZB2010', 'UZB2011', 'UZB2012',
#            'UZB2013', 'UZB2014') 

#ccexchange <- c(1, 1, 1,
#                1, 1, 1,
#                  1, 1, 1,
#               3.204,
#               .5788,
#               30.9104, 30.5344, 29.4169,
#               33.22,
#               21.000, 22.952, 23.084,
#               NA, NA, NA,
#             1286.13, 1317.92, 1464.24,
#                 1587.57, 1716.41, 1893.32, 
#               2106.34, 2322.66)