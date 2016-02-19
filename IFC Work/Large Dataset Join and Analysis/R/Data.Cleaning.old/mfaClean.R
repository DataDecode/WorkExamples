library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

#the mfa data is in tabs depending on loan type.  Each tab has universal and individual values.
#Since we are intersted in the values we can compare across group
mfa.banking<- read.xlsx('Data/MFA Historical Raw Data as of June 2015_all models.xlsx', 
                        sheet=1)
mfa.life<- read.xlsx('Data/MFA Historical Raw Data as of June 2015_all models.xlsx', 
                     sheet=2)
mfa.nonlife<- read.xlsx('Data/MFA Historical Raw Data as of June 2015_all models.xlsx', 
                        sheet=3)
mfa.housing<- read.xlsx('Data/MFA Historical Raw Data as of June 2015_all models.xlsx', 
                        sheet=4)
mfa.leasing<- read.xlsx('Data/MFA Historical Raw Data as of June 2015_all models.xlsx', 
                        sheet=5)
mfa.onbfi<- read.xlsx('Data/MFA Historical Raw Data as of June 2015_all models.xlsx', 
                      sheet=6)

#needed intrest spread for the SME example
mfa.banking.interest <- select(mfa.banking,  CUSTOMERID , STATEMENTDATE, NETINTERESTSPREAD)
mfa.housing.interest <- select(mfa.housing,  CUSTOMERID , STATEMENTDATE, NETINTERESTSPREAD)
mfa.onbfi.interest <- select(mfa.onbfi,  CUSTOMERID , STATEMENTDATE, NETINTERESTSPREAD)


#leasing had multiple cloumns of the same name
#mfa.leasing.interest <- select(mfa.leasing,  CUSTOMERID , STATEMENTDATE, NETINTERESTSPREAD)

mfa.interst.spread <- rbind(mfa.banking.interest, mfa.housing.interest, mfa.onbfi.interest)
mfa.interst.spread$YEAR<- substr(mfa.interst.spread$STATEMENTDATE, 0, 4) %>%
  as.numeric()
mfa.interst.spread$inst.year = paste(mfa.interst.spread$CUSTOMERID, mfa.interst.spread$YEAR, sep = '') %>%
  as.numeric()

mfa.interst.spread<- select(mfa.interst.spread, inst.year, NETINTERESTSPREAD)
                              
#combining the mfa data frames into one 
#with all the needed information from the mfa
common_cols <- intersect(colnames(mfa.banking),
                         colnames(mfa.housing)) %>%
  intersect(colnames(mfa.leasing)) %>%
  intersect(colnames(mfa.life)) %>%
  intersect(colnames(mfa.nonlife)) %>%
  intersect(colnames(mfa.onbfi))


mfa<- rbind(subset(mfa.banking, select = common_cols),
            subset(mfa.housing, select = common_cols),
            subset(mfa.leasing, select = common_cols),
            subset(mfa.life, select = common_cols),
            subset(mfa.nonlife, select = common_cols),
            subset(mfa.onbfi, select = common_cols))



#Making a Year Variable
mfa$YEAR<- substr(mfa$STATEMENTDATE, 0, 4) %>%
  as.numeric()

mfa$AUDITMETHOD<- as.factor(mfa$AUDITMETHOD)
mfa$STMTSTATUS<- as.factor(mfa$STMTSTATUS)
mfa$STATEMENTTYPE<- as.factor(mfa$STATEMENTTYPE)
mfa$STATEMENTDATE <- as.Date(mfa$STATEMENTDATE)

# In mfa, there are 1308 unique institutions
# Since there are mulitple statements from every year, we pull out the last statement from every year
# for every organization.
mfa$inst.year = paste(mfa$CUSTOMERID, mfa$YEAR, sep = '') %>%
  as.numeric()
mfa <- arrange(mfa, CUSTOMERID, desc(STATEMENTDATE))

#there are 8063 distinct observations for the institution&year attribute

mfa<- distinct(mfa, inst.year)
#now we have the last entry from every year available for all of the institutions

# We take just the columns we want in analysis
mfa.subset<- select(mfa, CUSTOMERID, YEAR, inst.year, INSTITUTION_NME, CUSTOMERNAME, COUNTRY_CODE,
              COUNTRY_NME, COUNTRY_NME, REGION_NME, DEPT_NME,
              IFC_SECTOR_NME, REGION_NME, STATEMENTID, STATEMENTDATE,
              STATEMENTMONTHS, AUDITMETHOD, STMTSTATUS, STATEMENTTYPE,
              TOTALASSETS, TOTALEQUITY, TOTALLIABILITIES, TOTALLIABSEQUITY,
              NETINCOME, NETINCOMEGROWTH)

mfa.interst.spread <- distinct(mfa.interst.spread, inst.year)

mfa.subset<- left_join(mfa.subset, mfa.interst.spread, 'inst.year')

x <- c('Institution.Nbr',
       'Reporting.Year',
       'Institution.Year',
       'Institution.Short.Name',
       'Institution.Legal.Name',
       'Country.Code',
       'Country.Name',
       'Regional.Name',
       'Department.Name',
       'Sector.Name',
       'Statement.ID',
       'Statement.Date',
       'Statement.Number.of.Months.Covered',
       'Audit.Method',
       'Statement.Status',
       'Statement.Type',
       'Total.Assets',
       'Total.Equity',
       'Total.Liabilities',
       'Total.Liabs.Equity',
       'Net.Income',
       'Net.Income.Growth',
       'Net.Interest.Spread')

length(x) == length(colnames(mfa.subset))

colnames(mfa.subset) <- x

#Making percentages
mfa.subset <- mfa.subset %>%
  mutate(Net.Income.Growth = Net.Income.Growth / 100,
         Net.Interest.Spread = Net.Interest.Spread /100)

# this list should be in every file to clean the country names
require(stringr)
mfa.subset$Country.Name <-str_trim(mfa.subset$Country.Name)
mfa.subset$Country.Name[mfa.subset$Country.Name == "Afghanistan, I.R. of"] <- "Afghanistan"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Azerbaijan, Rep. of"] <- "Azerbaijan"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Bahrain, Kingdom of"] <- "Bahrain"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Cabo Verde"] <- "Cape Verde"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Central African Republic"] <- "Central African Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Congo, Dem. Rep. of"] <- "Congo, Dem. Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "China,P.R.: Mainland"] <- "China"
mfa.subset$Country.Name[mfa.subset$Country.Name == "China,P.R.:Hong Kong"] <- "Hong Kong SAR, China"
mfa.subset$Country.Name[mfa.subset$Country.Name == "China,P.R.:Macao"] <- "Macao SAR, China"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Congo, Republic of"] <- "Congo, Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Cote D'Ivoire"] <- "Cote d'Ivoire"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Cote D&#39;Ivoire"] <- "Cote d'Ivoire"
mfa.subset$Country.Name[mfa.subset$Country.Name == "C√¥te d'Ivoire"] <- "Cote d'Ivoire"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Cura√ßao"] <- "Curacao"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Egypt, Arab Republic of"] <- "Egypt, Arab Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Egypt"] <- "Egypt, Arab Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Faroe Islands"] <- "Faeroe Islands"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Iran, I.R. of"] <- "Iran, Islamic Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Korea, Republic of"] <- "Korea, Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Lao People's Democratic Republic"] <- "Lao PDR"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Lao People&#39;s Democratic Republic"] <- "Lao PDR"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Lao People's Dem.Rep"] <- "Lao PDR"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Macedonia, Former Yugoslav Republic of"] <- "Macedonia, FYR"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Marshall Islands"] <- "Marshall Islands"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Marshall Islands,Rep"] <- "Marshall Islands"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Marshall Islands, Rep."] <- "Marshall Islands"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Micronesia, Fed.Sts."] <- "Micronesia, Fed. Sts."
mfa.subset$Country.Name[mfa.subset$Country.Name == "São Tomé and Principe"] <- "Sao Tome and Principe"
mfa.subset$Country.Name[mfa.subset$Country.Name == "S√£o Tom√© and Principe"] <- "Sao Tome and Principe"
mfa.subset$Country.Name[mfa.subset$Country.Name == "S√£o Tom√© & Pr√≠ncipe"] <- "Sao Tome and Principe"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela, RB"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Yemen, Republic of"] <- "Yemen, Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Yemen"] <- "Yemen, Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Slovakia"] <- "Slovak Republic"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Congo Republic"] <- "Congo, Dem. Rep."
mfa.subset$Country.Name[mfa.subset$Country.Name == "Saint Lucia"] <- "St. Lucia"
mfa.subset$Country.Name[mfa.subset$Country.Name == "St. Vincent & Grens."] <- "St. Vincent and the Grenadines"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Serbia, Republic of"] <- "Serbia"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Timor Leste"] <- "Timor-Leste"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Venezuela, Rep. Bol."] <- "Venezuela"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela"
mfa.subset$Country.Name[mfa.subset$Country.Name == "South Sudan, Rep. of"] <- "South Sudan"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Anguilla"] <- "Anguila"
mfa.subset$Country.Name[mfa.subset$Country.Name == "Brunei Darussalam"] <- "Brunei"

#this was Serbia and Motenegro, changed to just Serbia (they are the same Income group)
mfa.subset$Country.Name[mfa.subset$Country.Name == "Serbia and Montenegro"] <- "Serbia"

#adding country code
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
mfa.subset$Country.Code <- NULL
mfa.subset <- left_join(mfa.subset, countries, 'Country.Name')

write.csv(mfa.subset, file = 'Clean.data/mfa.csv')