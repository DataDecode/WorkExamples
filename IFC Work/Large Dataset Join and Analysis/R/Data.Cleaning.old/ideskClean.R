library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

#SHOULD I BE PULLING MORE INFORMATION FROM THIS?
#LEAVING ALL OF THE PROJECT SPECIFIC INFORMATION BEHIND.

idesk<- read.xlsx('Data/Institution Extract-June 30, 2015 (iDesk raw extract).xlsx')

# We take just the columns we want in analysis
# Left out country code since it does not line up with the other country codes.
idesk.subset<- select(idesk, Institution.Nbr, Institution.Short.Name, Institution.Legal.Name,
                      Institution.Country, `Institution.Region.-.Seven`, Geographic.Region,
                      Primary.Sector.Code, Secondary.Sector.Code, Tertiary.Sector.Code,
                      Primary.Sector.Name, Secondary.Sector.Name, Tertiary.Sector.Name,
                      SME.Ind, Master.Project.ID)

x<- c('Institution.Nbr',
      'Institution.Short.Name',
      'Institution.Legal.Name',
      'Country.Name',
      'Regional.Name',
      'Geographic.Region.Name',
      'Primary.Sector.Code',
      'Secondary.Sector.Code',
      'Tertiary.Sector.Code',
      'Primary.Sector.Name',
      'Secondary.Sector.Name',
      'Tertiary.Sector.Name',
      'SME.Ind',
      'Master.Project.ID')

length(x) == length(colnames(idesk.subset))

colnames(idesk.subset)<- x

# this list should be in every file to clean the country names
require(stringr)
idesk.subset$Country.Name <-str_trim(idesk.subset$Country.Name)
idesk.subset$Country.Name[idesk.subset$Country.Name == "Afghanistan, I.R. of"] <- "Afghanistan"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Azerbaijan, Rep. of"] <- "Azerbaijan"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Bahrain, Kingdom of"] <- "Bahrain"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Cabo Verde"] <- "Cape Verde"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Central African Republic"] <- "Central African Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Congo, Dem. Rep. of"] <- "Congo, Dem. Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "China,P.R.: Mainland"] <- "China"
idesk.subset$Country.Name[idesk.subset$Country.Name == "China,P.R.:Hong Kong"] <- "Hong Kong SAR, China"
idesk.subset$Country.Name[idesk.subset$Country.Name == "China,P.R.:Macao"] <- "Macao SAR, China"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Congo, Republic of"] <- "Congo, Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Cote D'Ivoire"] <- "Cote d'Ivoire"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Cote D&#39;Ivoire"] <- "Cote d'Ivoire"
idesk.subset$Country.Name[idesk.subset$Country.Name == "C√¥te d'Ivoire"] <- "Cote d'Ivoire"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Cura√ßao"] <- "Curacao"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Egypt, Arab Republic of"] <- "Egypt, Arab Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Egypt"] <- "Egypt, Arab Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Faroe Islands"] <- "Faeroe Islands"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Iran, I.R. of"] <- "Iran, Islamic Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Korea, Republic of"] <- "Korea, Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Lao People's Democratic Republic"] <- "Lao PDR"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Lao People&#39;s Democratic Republic"] <- "Lao PDR"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Lao People's Dem.Rep"] <- "Lao PDR"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Macedonia, Former Yugoslav Republic of"] <- "Macedonia, FYR"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Marshall Islands"] <- "Marshall Islands"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Marshall Islands,Rep"] <- "Marshall Islands"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Marshall Islands, Rep."] <- "Marshall Islands"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Micronesia, Fed.Sts."] <- "Micronesia, Fed. Sts."
idesk.subset$Country.Name[idesk.subset$Country.Name == "São Tomé and Principe"] <- "Sao Tome and Principe"
idesk.subset$Country.Name[idesk.subset$Country.Name == "S√£o Tom√© and Principe"] <- "Sao Tome and Principe"
idesk.subset$Country.Name[idesk.subset$Country.Name == "S√£o Tom√© & Pr√≠ncipe"] <- "Sao Tome and Principe"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela, RB"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Yemen, Republic of"] <- "Yemen, Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Yemen"] <- "Yemen, Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Slovakia"] <- "Slovak Republic"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Congo Republic"] <- "Congo, Dem. Rep."
idesk.subset$Country.Name[idesk.subset$Country.Name == "Saint Lucia"] <- "St. Lucia"
idesk.subset$Country.Name[idesk.subset$Country.Name == "St. Vincent & Grens."] <- "St. Vincent and the Grenadines"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Serbia, Republic of"] <- "Serbia"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Timor Leste"] <- "Timor-Leste"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Venezuela, Rep. Bol."] <- "Venezuela"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela"
idesk.subset$Country.Name[idesk.subset$Country.Name == "South Sudan, Rep. of"] <- "South Sudan"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Anguilla"] <- "Anguila"
idesk.subset$Country.Name[idesk.subset$Country.Name == "Brunei Darussalam"] <- "Brunei"

#this was Serbia and Motenegro, changed to just Serbia (they are the same Income group)
idesk.subset$Country.Name[idesk.subset$Country.Name == "Serbia and Montenegro"] <- "Serbia"

#adding country code
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
idesk.subset$Country.Code <- NULL
idesk.subset <- left_join(idesk.subset, countries, 'Country.Name')

write.csv(idesk.subset, file = 'Clean.data/idesk.csv')