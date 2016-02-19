library(openxlsx)
library(dplyr)
library(magrittr)
library(reshape2)
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
mfa<- distinct(mfa, inst.year)
#now we have the last entry from every year available for all of the institutions

# We take just the columns we want in analysis
mfa.subset<- select(mfa, CUSTOMERID, YEAR, inst.year, INSTITUTION_NME, CUSTOMERNAME, COUNTRY_CODE,
              COUNTRY_NME, COUNTRY_NME, REGION_NME, DEPT_NME,
              IFC_SECTOR_NME, REGION_NME, STATEMENTID, STATEMENTDATE,
              STATEMENTMONTHS, AUDITMETHOD, STMTSTATUS, STATEMENTTYPE,
              TOTALASSETS, TOTALEQUITY, TOTALLIABILITIES, TOTALLIABSEQUITY,
              NETINCOME, NETINCOMEGROWTH)

write.csv(mfa.subset, file = 'Clean.data/mfa.csv')


#cleaning I-DESK
idesk<- read.xlsx('Data/Institution Extract-June 30, 2015 (iDesk raw extract).xlsx')

# We take just the columns we want in analysis
idesk.subset<- select(idesk, Institution.Nbr, Institution.Short.Name, Institution.Legal.Name,
                      Institution.Country, IBRD.Country.Code, `Institution.Region.-.Seven`, Geographic.Region,
                      Primary.Sector.Code, Secondary.Sector.Code, Tertiary.Sector.Code,
                      Primary.Sector.Name, Secondary.Sector.Name, Tertiary.Sector.Name,
                      SME.Ind, Master.Project.ID)

write.csv(idesk, file = 'Clean.data/idesk.csv')



#DOTS Ratings
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

write.csv(dots.ratings.subset, file = 'Clean.data/dotsratings2014.csv')


#DOTS Indicator
dots.indicator <- read.xlsx('Data/DOTS Roic indicator raw extract.xlsx', startRow = 10)
dots.indicator$indicator_field = paste(dots.indicator$Indicator.Name, dots.indicator$Field.Name, sep="_")

#in this step, we lose all the "Life of Project" calcualtions
dots.indicator$inst.year = paste(dots.indicator$Institution.NBR, dots.indicator$Reporting.Year, sep = '') %>%
  as.numeric()

dots.indicator<- dots.indicator[complete.cases(dots.indicator$inst.year),]

#this swings out all of the variables into columns.
#STILL DON"T KNOW
dots.indicator<- spread(dots.indicator, key = indicator_field, value = Indicator.Value)

#build out elements
dots.indicator.full <- data.frame(dots.indicator$inst.year)
dots.indicator.full<- unique(dots.indicator.full)
colnames(dots.indicator.full) <- "inst.year"

# need one of these for all the columns we want as one year
# PROBLEM These are creating new observations in the dots.indicator.full data.frame
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

dots.indicator.full <- full_join(dots.indicator.full , e.1, by = "inst.year") %>%
  full_join(e.2, by = "inst.year") %>%
  full_join( e.3, by = "inst.year") %>%
  full_join(e.4, by = "inst.year") %>%
  full_join(e.5, by = "inst.year") %>%
  full_join(e.6, by = "inst.year") %>%
  full_join(r.1, by = "inst.year") %>%
  full_join(r.2, by = "inst.year") %>%
  full_join(r.3, by = "inst.year") %>%
  full_join(r.4, by = "inst.year") %>%
  full_join(r.5, by = "inst.year") %>%
  full_join(r.6, by = "inst.year")

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
              "ROIC (%) - Annual_USD ROIC (%)", "ROIC (%) - Annual_USD WACC (%)")

dots.indicator[cols.num] <- sapply(dots.indicator[cols.num], as.numeric)

#delete years beyond 2014
dots.indicator$Reporting.Year <- as.numeric(dots.indicator$Reporting.Year)
dots.indicator<- filter(dots.indicator, Reporting.Year <= 2014)


#rename columns so they are easier to understand
colnames(dots.indicator)<- c("Reporting.Year", "Institution.NBR", "Institution.Short.Name",
                             "Project.ID", "Project.Short.Name", "Industry.Department.Code",
                             "Regional.Department.Code", "Regional.Department.Name", "Country.Name",
                             "IFC.Tertiary.Sector.Code", "inst.year", "EROIC Annual USD",
                             "EROIC.Annual.USD.WACC","EROIC.Average.Annual.Difference",
                             "EROIC.Annual.Difference","EROIC.Annualn.Difference.from.Expectations",
                             "EROIC.Annual.WACC.Difference.USD","ROIC.Average.Annual.Difference",
                             "ROIC.Annual.Difference","ROIC.Annual.Difference.from.Expectations",
                             "ROIC.Annual.WACC.Difference.USD","ROIC.Annual.ROIC.USD",
                             "ROIC.Annual.WACC.USD")

write.csv(dots.indicator, file = 'Clean.data/dotsindicator.csv')

#Reach DATA
#this data is pretty straight forward for what we need.  Not much cleaning to do.
#More cleaning will depend on what we want to do.
reach <- read.xlsx('Data/Reach Data 2004-2014_Consolidated (original).xlsx', startRow = 6)
reach <- arrange(reach, `PARTNER.#`, Year)

reach$XX <- NULL
reach$XXX <- NULL
reach$xx <- NULL
write.csv(reach, file = 'Clean.data/reach.csv')

#15 Days of work
#11.24.15 9am - 6pm
# 11.25.15 9 -6pm
# 11/28/15 11:45am - 6:45
# 11/29/15 1:45 - 2:30
# 11/29/15 4:45 - 5:45
# 11/30/15 9:15am - 7:00pm
# 12/1/15 1pm - 8pm
# 12/2/15 9 - 6:30
# 12/3/15 9:30 - 6:30
# 12/4/15 10 - 6pm
# 12/7/15 - INTERVIEW DAY
# 12/8/15 9 - 6pm
# 12/9/15 9- 6:30
# 12/10/15 10 - 5
# 12/11/15 10 - 6:30
# 12/14/15 10 - 6pm
# 12/15/15 9:30 - 5:30

#20 more days on the contract
# 12/16/15 9:30 - 5:45
# 12/17/15 9:30 - 6pm
# 12/18/15 9:30 - ?


#5 day contract

9+9+7+.75+1+9.75+7+9.5+9+8+9+9.5+7+8.5

