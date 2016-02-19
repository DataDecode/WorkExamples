library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

# won't work with openxlsx
require(xlsx)
income <- read.xlsx('Original.Data/ExternalDBs/CLASS.XLS', startRow = 5, endRow = 221, 
                    sheetIndex = 1)

#data.frame comes in with a lot of missing columns and rows
income$NA. <- NULL
income$NA..1 <- NULL
income$NA..2 <- NULL
income$NA..3 <- NULL
income$NA..4 <- NULL

income<- income[-1,]

#Don't need this column
income$Other <- NULL

#This column has the wrong designations for some countries
#removing it to limit confusion later
income$Region <- NULL

# make the .. to NA
income[income == '..'] <- NA

# Less confusing column name
colnames(income) <- c('Country.Name',
                      'Country.Code',
                      'Country.Income.Group',
                      'Country.Lending.Category')


# this list should be in every file to clean the country names
require(stringr)
income$Country.Name <-str_trim(income$Country.Name)
income$Country.Name[income$Country.Name == "Afghanistan, I.R. of"] <- "Afghanistan"
income$Country.Name[income$Country.Name == "Azerbaijan, Rep. of"] <- "Azerbaijan"
income$Country.Name[income$Country.Name == "Bahrain, Kingdom of"] <- "Bahrain"
income$Country.Name[income$Country.Name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
income$Country.Name[income$Country.Name == "Cabo Verde"] <- "Cape Verde"
income$Country.Name[income$Country.Name == "Central African Republic"] <- "Central African Rep."
income$Country.Name[income$Country.Name == "Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
income$Country.Name[income$Country.Name == "Congo, Dem. Rep. of"] <- "Congo, Dem. Rep."
income$Country.Name[income$Country.Name == "China,P.R.: Mainland"] <- "China"
income$Country.Name[income$Country.Name == "China,P.R.:Hong Kong"] <- "Hong Kong SAR, China"
income$Country.Name[income$Country.Name == "China,P.R.:Macao"] <- "Macao SAR, China"
income$Country.Name[income$Country.Name == "Congo, Republic of"] <- "Congo, Rep."
income$Country.Name[income$Country.Name == "Cote D'Ivoire"] <- "Cote d'Ivoire"
income$Country.Name[income$Country.Name == "Cote D&#39;Ivoire"] <- "Cote d'Ivoire"
income$Country.Name[income$Country.Name == "C√¥te d'Ivoire"] <- "Cote d'Ivoire"
income$Country.Name[income$Country.Name == "Cura√ßao"] <- "Curacao"
income$Country.Name[income$Country.Name == "Egypt, Arab Republic of"] <- "Egypt, Arab Rep."
income$Country.Name[income$Country.Name == "Egypt"] <- "Egypt, Arab Rep."
income$Country.Name[income$Country.Name == "Faroe Islands"] <- "Faeroe Islands"
income$Country.Name[income$Country.Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
income$Country.Name[income$Country.Name == "Iran, I.R. of"] <- "Iran, Islamic Rep."
income$Country.Name[income$Country.Name == "Korea, Republic of"] <- "Korea, Rep."
income$Country.Name[income$Country.Name == "Lao People's Democratic Republic"] <- "Lao PDR"
income$Country.Name[income$Country.Name == "Lao People&#39;s Democratic Republic"] <- "Lao PDR"
income$Country.Name[income$Country.Name == "Lao People's Dem.Rep"] <- "Lao PDR"
income$Country.Name[income$Country.Name == "Macedonia, Former Yugoslav Republic of"] <- "Macedonia, FYR"
income$Country.Name[income$Country.Name == "Marshall Islands"] <- "Marshall Islands"
income$Country.Name[income$Country.Name == "Marshall Islands,Rep"] <- "Marshall Islands"
income$Country.Name[income$Country.Name == "Marshall Islands, Rep."] <- "Marshall Islands"
income$Country.Name[income$Country.Name == "Micronesia, Fed.Sts."] <- "Micronesia, Fed. Sts."
income$Country.Name[income$Country.Name == "São Tomé and Principe"] <- "Sao Tome and Principe"
income$Country.Name[income$Country.Name == "S√£o Tom√© and Principe"] <- "Sao Tome and Principe"
income$Country.Name[income$Country.Name == "S√£o Tom√© & Pr√≠ncipe"] <- "Sao Tome and Principe"
income$Country.Name[income$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela, RB"
income$Country.Name[income$Country.Name == "Yemen, Republic of"] <- "Yemen, Rep."
income$Country.Name[income$Country.Name == "Yemen"] <- "Yemen, Rep."
income$Country.Name[income$Country.Name == "Slovakia"] <- "Slovak Republic"
income$Country.Name[income$Country.Name == "Congo Republic"] <- "Congo, Dem. Rep."
income$Country.Name[income$Country.Name == "Saint Lucia"] <- "St. Lucia"
income$Country.Name[income$Country.Name == "St. Vincent & Grens."] <- "St. Vincent and the Grenadines"
income$Country.Name[income$Country.Name == "Serbia, Republic of"] <- "Serbia"
income$Country.Name[income$Country.Name == "Timor Leste"] <- "Timor-Leste"
income$Country.Name[income$Country.Name == "Venezuela, Rep. Bol."] <- "Venezuela"
income$Country.Name[income$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela"
income$Country.Name[income$Country.Name == "South Sudan, Rep. of"] <- "South Sudan"
income$Country.Name[income$Country.Name == "Anguilla"] <- "Anguila"
income$Country.Name[income$Country.Name == "Brunei Darussalam"] <- "Brunei"

#this was Serbia and Motenegro, changed to just Serbia (they are the same Income group)
income$Country.Name[income$Country.Name == "Serbia and Montenegro"] <- "Serbia"

#adding country code
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
income$Country.Code <- NULL
income <- left_join(income, countries, 'Country.Name')

write.csv(income, file = 'Clean.data/countryIncomeCategory.csv')