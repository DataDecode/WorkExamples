library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

dots.indicator.1 <- read.xlsx('Data/DOTS Roic indicator raw extract.xlsx', startRow = 10)
dots.indicator.2 <- read.xlsx('Data/DOTS Extract ROIC and ROE_pulled 2015.12.02.xlsx', sheet=1, startRow = 10)
dots.indicator.3 <- read.xlsx('Data/DOTS Extract ROIC and ROE_pulled 2015.12.02.xlsx', sheet=2, startRow = 10)

dots.indicator <- rbind(dots.indicator.1, dots.indicator.2, dots.indicator.3)

dots.indicator<- distinct(dots.indicator)

dots.indicator$indicator_field = paste(dots.indicator$Indicator.Name, dots.indicator$Field.Name, sep="_")

#in this step, we lose all the "Life of Project" calcualtions
dots.indicator[dots.indicator == '-'] <- NA

dots.indicator <- filter(dots.indicator, !is.na(Reporting.Year))

dots.indicator$inst.year = paste(dots.indicator$Institution.NBR, dots.indicator$Reporting.Year, sep = '') %>%
  as.numeric()

#this swings out all of the variables into columns.
dots.indicator<- spread(dots.indicator, key = indicator_field, value = Indicator.Value)


#build out elements
dots.indicator.full <- data.frame(dots.indicator$inst.year)
dots.indicator.full<- unique(dots.indicator.full)
colnames(dots.indicator.full) <- "inst.year"

# need one of these for all the columns we want as one year
# YOU NEED TO LOOP THIS PROCESS

e.1<- select(dots.indicator, inst.year, `EROIC (%) - Annual_Annual USD EROIC (%)`)
e.1 <- arrange(e.1, `EROIC (%) - Annual_Annual USD EROIC (%)`)
e.1<- distinct(e.1, inst.year)

e.2<- select(dots.indicator, inst.year, `EROIC (%) - Annual_Annual USD WACC (%)`)
e.2 <- arrange(e.2, `EROIC (%) - Annual_Annual USD WACC (%)`)
e.2<- distinct(e.2, inst.year)                                                              

e.3<- select(dots.indicator, inst.year, `EROIC (%) - Annual_Average Annual Difference (%)`)
e.3 <- arrange(e.3, `EROIC (%) - Annual_Average Annual Difference (%)`)
e.3<- distinct(e.3, inst.year)
      
e.4<- select(dots.indicator, inst.year, `EROIC (%) - Annual_Difference (%)`)
e.4 <- arrange(e.4, `EROIC (%) - Annual_Difference (%)`)
e.4<- distinct(e.4, inst.year)
      
e.5<- select(dots.indicator, inst.year, `EROIC (%) - Annual_Difference from Expectations (%)`)
e.5 <- arrange(e.5, `EROIC (%) - Annual_Difference from Expectations (%)`)
e.5<- distinct(e.5, inst.year)                                       
      
e.6<- select(dots.indicator, inst.year, `EROIC (%) - Annual_EROIC-WACC (USD) Difference (%)`)
e.6 <- arrange(e.6, `EROIC (%) - Annual_EROIC-WACC (USD) Difference (%)`)
e.6<- distinct(e.6, inst.year)

e.7<- select(dots.indicator, inst.year, `EROE (%): Annual_Annual COE %: 5yr US TBond + equity risk premium + country spread + project spread`)
e.7 <- arrange(e.7, `EROE (%): Annual_Annual COE %: 5yr US TBond + equity risk premium + country spread + project spread`)
e.7<- distinct(e.7, inst.year)

e.8<- select(dots.indicator, inst.year, `EROE (%): Annual_Annual EROE %`)
e.8 <- arrange(e.8, `EROE (%): Annual_Annual EROE %`)
e.8<- distinct(e.8, inst.year)

e.9<- select(dots.indicator, inst.year, `EROE (%): Annual_Annual Risk Premium (RP) %: equity risk premium + country spread + project spread`)
e.9 <- arrange(e.9, `EROE (%): Annual_Annual Risk Premium (RP) %: equity risk premium + country spread + project spread`)
e.9<- distinct(e.9, inst.year)

e.10<- select(dots.indicator, inst.year, `EROE (%): Annual_EROE Spread %: EROE - COE`)
e.10 <- arrange(e.10, `EROE (%): Annual_EROE Spread %: EROE - COE`)
e.10<- distinct(e.10, inst.year)

r.1<- select(dots.indicator, inst.year, `ROIC (%) - Annual_Average Annual Difference (%)`)
r.1 <- arrange(r.1, `ROIC (%) - Annual_Average Annual Difference (%)`)
r.1<- distinct(r.1, inst.year)

r.2<- select(dots.indicator, inst.year, `ROIC (%) - Annual_Difference (%)`)
r.2 <- arrange(r.2, `ROIC (%) - Annual_Difference (%)`)
r.2 <- distinct(r.2, inst.year)

r.3<- select(dots.indicator, inst.year, `ROIC (%) - Annual_Difference from Expectations (%)`)
r.3 <- arrange(r.3, `ROIC (%) - Annual_Difference from Expectations (%)`)
r.3 <- distinct(r.3, inst.year)

r.4<- select(dots.indicator, inst.year, `ROIC (%) - Annual_ROIC-WACC (USD) Difference (%)`)
r.4 <- arrange(r.4, `ROIC (%) - Annual_ROIC-WACC (USD) Difference (%)`)
r.4 <- distinct(r.4, inst.year)

r.5<- select(dots.indicator, inst.year, `ROIC (%) - Annual_USD ROIC (%)`)
r.5 <- arrange(r.5, `ROIC (%) - Annual_USD ROIC (%)`)
r.5 <- distinct(r.5, inst.year)

r.6<- select(dots.indicator, inst.year, `ROIC (%) - Annual_USD WACC (%)`)
r.6 <- arrange(r.6, `ROIC (%) - Annual_USD WACC (%)`)
r.6 <- distinct(r.6, inst.year)


r.7<- select(dots.indicator, inst.year, `ROE (%): Annual_Annual COE %: 5yr US TBond + equity risk premium + country spread + project spread`)
r.7 <- arrange(r.7, `ROIC (%) - Annual_USD WACC (%)`)
r.7 <- distinct(r.7, inst.year)

r.8<- select(dots.indicator, inst.year, `ROE (%): Annual_Annual Risk Premium (RP) %: equity risk premium + country spread + project spread`)
r.8 <- arrange(r.8, `ROE (%): Annual_Annual Risk Premium (RP) %: equity risk premium + country spread + project spread`)
r.8 <- distinct(r.8, inst.year)

r.9<- select(dots.indicator, inst.year, `ROE (%): Annual_Annual ROE %`)
r.9 <- arrange(r.9, `ROE (%): Annual_Annual ROE %`)
r.9 <- distinct(r.9, inst.year)

r.10<- select(dots.indicator, inst.year, `ROE (%): Annual_ROE Spread %: ROE - COE %`)
r.10 <- arrange(r.10, `ROE (%): Annual_ROE Spread %: ROE - COE %`)
r.10 <- distinct(r.10, inst.year)



dots.indicator.full <- full_join(dots.indicator.full , e.1, by = "inst.year") %>%
  full_join(e.2, by = "inst.year") %>%
  full_join(e.3, by = "inst.year") %>%
  full_join(e.4, by = "inst.year") %>%
  full_join(e.5, by = "inst.year") %>%
  full_join(e.6, by = "inst.year") %>%
  full_join(e.7, by = "inst.year") %>%
  full_join(e.8, by = "inst.year") %>%
  full_join(e.9, by = "inst.year") %>%
  full_join(e.10, by = "inst.year") %>%
  full_join(r.1, by = "inst.year") %>%
  full_join(r.2, by = "inst.year") %>%
  full_join(r.3, by = "inst.year") %>%
  full_join(r.4, by = "inst.year") %>%
  full_join(r.5, by = "inst.year") %>%
  full_join(r.6, by = "inst.year") %>%
  full_join(r.7, by = "inst.year") %>%
  full_join(r.8, by = "inst.year") %>%
  full_join(r.9, by = "inst.year") %>%
  full_join(r.10, by = "inst.year")

#changing all the indicators to percentages
#HELD OFF ON THIS STEP RIGHT NOW

#Add in the other info you want
dots.indicator.connector<- select(dots.indicator, Reporting.Year,
                                  Institution.NBR, Institution.Short.Name, 
                                  Project.ID, Project.Short.Name, 
                                  Reporting.Year, Industry.Department.Code, 
                                  Regional.Department.Code, Regional.Department.Name, 
                                  Country.Name, IFC.Tertiary.Sector.Code,
                                  inst.year)

dots.indicator <- full_join(dots.indicator.connector,dots.indicator.full, by= 'inst.year')
dots.indicator<- distinct(dots.indicator, inst.year)

cols.num <- c("EROIC (%) - Annual_Annual USD EROIC (%)", "EROIC (%) - Annual_Annual USD WACC (%)",
              "EROIC (%) - Annual_Average Annual Difference (%)", "EROIC (%) - Annual_Difference (%)", 
              "EROIC (%) - Annual_Difference from Expectations (%)", "EROIC (%) - Annual_EROIC-WACC (USD) Difference (%)", 
              "ROIC (%) - Annual_Average Annual Difference (%)", "ROIC (%) - Annual_Difference (%)",
              "ROIC (%) - Annual_Difference from Expectations (%)", "ROIC (%) - Annual_ROIC-WACC (USD) Difference (%)",
              "ROIC (%) - Annual_USD ROIC (%)", "ROIC (%) - Annual_USD WACC (%)",
              "EROE (%): Annual_Annual COE %: 5yr US TBond + equity risk premium + country spread + project spread",
              "EROE (%): Annual_Annual EROE %",                                                                     
              "EROE (%): Annual_Annual Risk Premium (RP) %: equity risk premium + country spread + project spread", 
              "EROE (%): Annual_EROE Spread %: EROE - COE",
              "ROE (%): Annual_Annual COE %: 5yr US TBond + equity risk premium + country spread + project spread",
              "ROE (%): Annual_Annual Risk Premium (RP) %: equity risk premium + country spread + project spread",
              "ROE (%): Annual_Annual ROE %",
              "ROE (%): Annual_ROE Spread %: ROE - COE %")

dots.indicator[cols.num] <- sapply(dots.indicator[cols.num], as.numeric)

#delete years beyond 2014
dots.indicator$Reporting.Year <- as.numeric(dots.indicator$Reporting.Year)
dots.indicator<- filter(dots.indicator, Reporting.Year <= 2014)

#rename columns so they are easier to understand
x<- c('Reporting.Year',
      'Institution.Nbr',
      'Institution.Short.Name',
      'Project.ID',
      'Project.Short.Name',
      'Industry.Code',
      'Regional.Code',
      'Regional.Name',
      'Country.Name',
      'Tertiary.Sector.Code',
      'Institution.Year',
      'EROIC.Annual',
      'EROIC.Annual.WACC',
      'EROIC.Average.Annual.Difference',
      'EROIC.Annual.Difference',
      'EROIC.Annual.Difference.from.Expectations',
      'EROIC.Annual.WACC.Difference',
      "Annual.COE.5yr",
      "Annual.EROE",                                                                     
      "Annual.Risk.Premium.EROE" ,
      "Annual.EROE.Spread.EROE.minus.COE",
      'ROIC.Average.Annual.Difference',
      'ROIC.Annual.Difference',
      'ROIC.Annual.Difference.from.Expectations',
      'ROIC.Annual.WACC.Difference',
      'ROIC.Annual.ROIC',
      'ROIC.Annual.WACC',
      "ROE.Annual.COE.5yr", 
      "ROE.Annual.Risk.Premium",  
      "ROE.Annual.ROE",                                                                       
      "Annual.ROE.Spread.ROE.minus.COE")

#check that there are the same number of elements in x as there are columns in dots.indicator
length(x) == length(colnames(dots.indicator))

colnames(dots.indicator)<- x

# putting columns in percentages
dots.indicator <- dots.indicator %>%
  mutate(EROIC.Annual = EROIC.Annual / 100,
         EROIC.Annual.WACC = EROIC.Annual.WACC / 100,
         EROIC.Average.Annual.Difference = EROIC.Average.Annual.Difference/ 100,
         EROIC.Annual.Difference = EROIC.Annual.Difference/ 100,
         EROIC.Annual.Difference.from.Expectations = EROIC.Annual.Difference.from.Expectations/ 100,
         EROIC.Annual.WACC.Difference = EROIC.Annual.WACC.Difference/ 100,
         Annual.COE.5yr = Annual.COE.5yr/ 100,
         Annual.EROE = Annual.EROE/ 100,                                                                     
         Annual.Risk.Premium.EROE = Annual.Risk.Premium.EROE / 100,
         Annual.EROE.Spread.EROE.minus.COE = Annual.EROE.Spread.EROE.minus.COE / 100,
         ROIC.Average.Annual.Difference = ROIC.Average.Annual.Difference / 100,
         ROIC.Annual.Difference = ROIC.Annual.Difference / 100,
         ROIC.Annual.Difference.from.Expectations = ROIC.Annual.Difference.from.Expectations / 100,
         ROIC.Annual.WACC.Difference = ROIC.Annual.WACC.Difference / 100,
         ROIC.Annual.ROIC = ROIC.Annual.ROIC / 100,
         ROIC.Annual.WACC = ROIC.Annual.WACC / 100,
         ROE.Annual.COE.5yr = ROE.Annual.COE.5yr / 100, 
         ROE.Annual.Risk.Premium = ROE.Annual.Risk.Premium / 100,  
         ROE.Annual.ROE = ROE.Annual.ROE / 100,                                                                       
         Annual.ROE.Spread.ROE.minus.COE = Annual.ROE.Spread.ROE.minus.COE / 100)




# this list should be in every file to clean the country names
require(stringr)
dots.indicator$Country.Name <-str_trim(dots.indicator$Country.Name)
dots.indicator$Country.Name[dots.indicator$Country.Name == "Afghanistan, I.R. of"] <- "Afghanistan"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Azerbaijan, Rep. of"] <- "Azerbaijan"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Bahrain, Kingdom of"] <- "Bahrain"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Cabo Verde"] <- "Cape Verde"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Central African Republic"] <- "Central African Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Congo, Dem. Rep. of"] <- "Congo, Dem. Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "China,P.R.: Mainland"] <- "China"
dots.indicator$Country.Name[dots.indicator$Country.Name == "China,P.R.:Hong Kong"] <- "Hong Kong SAR, China"
dots.indicator$Country.Name[dots.indicator$Country.Name == "China,P.R.:Macao"] <- "Macao SAR, China"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Congo, Republic of"] <- "Congo, Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Cote D'Ivoire"] <- "Cote d'Ivoire"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Cote D&#39;Ivoire"] <- "Cote d'Ivoire"
dots.indicator$Country.Name[dots.indicator$Country.Name == "C√¥te d'Ivoire"] <- "Cote d'Ivoire"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Cura√ßao"] <- "Curacao"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Egypt, Arab Republic of"] <- "Egypt, Arab Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Egypt"] <- "Egypt, Arab Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Faroe Islands"] <- "Faeroe Islands"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Iran, I.R. of"] <- "Iran, Islamic Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Korea, Republic of"] <- "Korea, Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Lao People's Democratic Republic"] <- "Lao PDR"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Lao People&#39;s Democratic Republic"] <- "Lao PDR"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Lao People's Dem.Rep"] <- "Lao PDR"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Macedonia, Former Yugoslav Republic of"] <- "Macedonia, FYR"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Marshall Islands"] <- "Marshall Islands"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Marshall Islands,Rep"] <- "Marshall Islands"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Marshall Islands, Rep."] <- "Marshall Islands"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Micronesia, Fed.Sts."] <- "Micronesia, Fed. Sts."
dots.indicator$Country.Name[dots.indicator$Country.Name == "São Tomé and Principe"] <- "Sao Tome and Principe"
dots.indicator$Country.Name[dots.indicator$Country.Name == "S√£o Tom√© and Principe"] <- "Sao Tome and Principe"
dots.indicator$Country.Name[dots.indicator$Country.Name == "S√£o Tom√© & Pr√≠ncipe"] <- "Sao Tome and Principe"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela, RB"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Yemen, Republic of"] <- "Yemen, Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Yemen"] <- "Yemen, Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Slovakia"] <- "Slovak Republic"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Congo Republic"] <- "Congo, Dem. Rep."
dots.indicator$Country.Name[dots.indicator$Country.Name == "Saint Lucia"] <- "St. Lucia"
dots.indicator$Country.Name[dots.indicator$Country.Name == "St. Vincent & Grens."] <- "St. Vincent and the Grenadines"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Serbia, Republic of"] <- "Serbia"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Timor Leste"] <- "Timor-Leste"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Venezuela, Rep. Bol."] <- "Venezuela"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela"
dots.indicator$Country.Name[dots.indicator$Country.Name == "South Sudan, Rep. of"] <- "South Sudan"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Anguilla"] <- "Anguila"
dots.indicator$Country.Name[dots.indicator$Country.Name == "Brunei Darussalam"] <- "Brunei"

#this was Serbia and Motenegro, changed to just Serbia (they are the same Income group)
dots.indicator$Country.Name[dots.indicator$Country.Name == "Serbia and Montenegro"] <- "Serbia"


#adding country code
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
dots.indicator$Country.Code <- NULL
dots.indicator <- left_join(dots.indicator, countries, 'Country.Name')



write.csv(dots.indicator, file = 'Clean.data/dotsindicator.csv')
