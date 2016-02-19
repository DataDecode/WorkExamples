library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

dots.ratings <- read.xlsx('Data/DOTS Detailed Report (ratings report raw extract).xlsx', 
                          sheet = 1, startRow = 10)

#duplicated column messes up the select function
dots.ratings$`Economic.Performance-Date.Rated` <- NULL

#taking just the columns we want
dots.ratings.subset <- select(dots.ratings, Industry.Department, Industry.Dept.Div,
                              Regional.Department, Country.name, `Country.ID#`,
                              Tertiary.Sector.Code, Industry.Group.Name,
                              Project.Name, `Project.ID#`,Project.Stage, 
                              `Project.Sub-Category`, Project.SME.Code,
                              Project.Tier, Project.Size, DOTS.Development.Outcome.Rating.Label,
                              DOTS.Financial.Performance.Rating.Label, DOTS.Economic.Performance.Rating.Label,
                              `DOTS.Environmental.&.Social.Performance.Rating.Label`, DOTS.PSD.Impact.Rating.Label,
                              Project.DOTS.Rating.Entered, DOTS.Development.Outcome.Rating, DOTS.Financial.Performance.Rating,
                              DOTS.Economic.Performance.Rating, `DOTS.Environmental.&.Social.Performance.Rating`, DOTS.Private.Sector.Development.Impact.Rating,
                              DOTS.Average.Indicator.Rating, Project.status)

# new names for columns

x<- c('Industry.Department',
      'Industry.Dept.Div',
      'Regional.Department',
      'Country.Name',
      'Country.Code',
      'Tertiary.Sector.Code',
      'Industry.Group.Name',
      'Project.Name',
      'Project.ID',
      'Project.Stage',
      'Project.Sub.Category',
      'Project.SME.Code',
      'Project.Tier',
      'Project.Size',
      'Development.Outcome.Rating.Label',
      'Financial.Performance.Rating.Label',
      'Economic.Performance.Rating.Label',
      'Environmental.Social.Performance.Rating.Label',
      'PSD.Impact.Rating.Label',
      'Project.Rating',
      'Development.Outcome.Rating',
      'Financial.Performance.Rating',
      'Economic.Performance.Rating',
      'Environmental.Social.Performance.Rating',
      'Private.Sector.Development.Impact.Rating',
      'Average.Indicator.Rating',
      'Project.Status')

length(x) == length(colnames(dots.ratings.subset))

colnames(dots.ratings.subset) <- x

# this list should be in every file to clean the country names
require(stringr)
dots.ratings.subset$Country.Name <-str_trim(dots.ratings.subset$Country.Name)
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Afghanistan, I.R. of"] <- "Afghanistan"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Azerbaijan, Rep. of"] <- "Azerbaijan"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Bahrain, Kingdom of"] <- "Bahrain"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Cabo Verde"] <- "Cape Verde"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Central African Republic"] <- "Central African Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Congo, Dem. Rep. of"] <- "Congo, Dem. Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "China,P.R.: Mainland"] <- "China"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "China,P.R.:Hong Kong"] <- "Hong Kong SAR, China"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "China,P.R.:Macao"] <- "Macao SAR, China"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Congo, Republic of"] <- "Congo, Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Cote D'Ivoire"] <- "Cote d'Ivoire"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Cote D&#39;Ivoire"] <- "Cote d'Ivoire"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "C√¥te d'Ivoire"] <- "Cote d'Ivoire"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Cura√ßao"] <- "Curacao"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Egypt, Arab Republic of"] <- "Egypt, Arab Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Egypt"] <- "Egypt, Arab Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Faroe Islands"] <- "Faeroe Islands"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Iran, I.R. of"] <- "Iran, Islamic Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Korea, Republic of"] <- "Korea, Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Lao People's Democratic Republic"] <- "Lao PDR"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Lao People&#39;s Democratic Republic"] <- "Lao PDR"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Lao People's Dem.Rep"] <- "Lao PDR"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Macedonia, Former Yugoslav Republic of"] <- "Macedonia, FYR"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Marshall Islands"] <- "Marshall Islands"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Marshall Islands,Rep"] <- "Marshall Islands"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Marshall Islands, Rep."] <- "Marshall Islands"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Micronesia, Fed.Sts."] <- "Micronesia, Fed. Sts."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "São Tomé and Principe"] <- "Sao Tome and Principe"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "S√£o Tom√© and Principe"] <- "Sao Tome and Principe"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "S√£o Tom√© & Pr√≠ncipe"] <- "Sao Tome and Principe"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela, RB"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Yemen, Republic of"] <- "Yemen, Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Yemen"] <- "Yemen, Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Slovakia"] <- "Slovak Republic"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Congo Republic"] <- "Congo, Dem. Rep."
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Saint Lucia"] <- "St. Lucia"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "St. Vincent & Grens."] <- "St. Vincent and the Grenadines"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Serbia, Republic of"] <- "Serbia"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Timor Leste"] <- "Timor-Leste"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Venezuela, Rep. Bol."] <- "Venezuela"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "South Sudan, Rep. of"] <- "South Sudan"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Anguilla"] <- "Anguila"
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Brunei Darussalam"] <- "Brunei"

#this was Serbia and Motenegro, changed to just Serbia (they are the same Income group)
dots.ratings.subset$Country.Name[dots.ratings.subset$Country.Name == "Serbia and Montenegro"] <- "Serbia"

#adding country code
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
dots.ratings.subset$Country.Code <- NULL
dots.ratings.subset <- left_join(dots.ratings.subset, countries, 'Country.Name')


write.csv(dots.ratings.subset, file = 'Clean.data/dotsratings2014.csv')