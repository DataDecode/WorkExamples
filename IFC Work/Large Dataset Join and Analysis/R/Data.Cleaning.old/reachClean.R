library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

#Reach DATA
#this data is pretty straight forward for what we need.  Not much cleaning to do.
#More cleaning will depend on what we want to do.
reach <- read.xlsx('Data/Reach Data 2004-2014_Consolidated.xlsx', startRow = 6, sheet= 1)

reach <- arrange(reach, `PARTNER.#`, Year)

reach$XX <- NULL
reach$XXX <- NULL
reach$xx <- NULL
reach$xx <- NULL
reach$xx <- NULL

x <- c('Reporting.Year',
       'Institution.Nbr',
       'Institution.Short.Name',
       'Region.Acronym',
       'Regional.Name',
       'Country.Name',
       'MSME.Type.Original',
       'SME.Type',
       'Product',
       'MFA.in.USD',
       'Portfoilo.Extrapolation',
       'NPL.Extrapolation',
       'Disb..Extrapolation',
       'Outs.Mortgage.Loans.Number',
       'Outs.Mortgage.Loans.Amount',
       'Purchase.of.Newly.Constructed.Houses.Number',
       'Purchase.of.Newly.Constructed.Houses.Amount',
       'Purchase.of.Existing.Houses.Number',
       'Purchase.of.Existing.Houses.Amount',
       'Home.Equity.Loans.Number',
       'Home.Equity.Loans.Amount',
       'Other.Retail.Number',
       'Other.Retail.Amount',
       'Educational.Loan.Number',
       'Educational.Loan.Amount',
       'Outs.Total.Retail.Number',
       'Outs.Total.Retail.Amount',
       'Outs.Micro.lessthan.1k.Number',
       'Outs.Micro.lessthan.1k.Amount',
       'Estimated.Outs.Micro.lessthan.1k.Women.Percent',
       'Estimated.Outs.Micro.lessthan.1k.Women.Entrepreneurs.Number',
       'Outs.Micro.between.1k.10k.Number',
       'Outs.Micro.between.1k.10k.Amount',
       'Estimated.Outs.Micro.between.1k.10k.Women.Percent',
       'Estimated.Outs.Micro.between.1k.10k.Women.Number',
       'Outs.Micro.Number',
       'Outs.Micro.Amount',
       'Outs.Small.between.10k.100k.Number',
       'Outs.Small.between.10k.100k.Amount',
       'Estimated.Outs.Small.between.10k.100k.Women.Percent',
       'Estimated.Outs.Small.between.10k.100k.Women.Number',
       'Outs.Small.between.100k.200k.Number',
       'Outs.Small.between.100k.200k.Amount',
       'Estimated.Outs.Small.between.100k.200k.Women.Percent',
       'Estimated.Outs.Small.between.100k.200k.Women.Number',
       'Outs.Medium.between.200k.1m.Number',
       'Outs.Medium.between.200k.1m.Amount',
       'Estimated.Outs.Medium.between.200k.1m.Women.Percent',
       'Estimated.Outs.Medium.between.200k.1m.Women.Number',
       'Outs.Medium.between.100k.1m.Number',
       'Outs.Medium.between.100k.1m.Amount',
       'Estimated.Outs.Medium.between.100k.1m.Women.Percent',
       'Estimated.Outs.Medium.between.100k.1m.Women.Number',
       'Outs.Medium.Coporate.between.1m.2m.Number',
       'Outs.Medium.Coporate.between.1m.2m.Amount',
       'Estimated.Outs.Medium.Coporate.between.1m.2m.Women.Percent',
       'Estimated.Outs.Medium.Coporate.between.1m.2m.Women.Number',
       'Outs.morethan.2m.Number',
       'Outs.morethan.2m.Amount',
       'Outs.Small.Number',
       'Outs.Small.Amount',
       'Outs.Medium.Number',
       'Outs.Medium.Amount',
       'Outs.SME.Number',
       'Outs.SME.Amount',
       'Outs.Corporate.Number',
       'Outs.Corporate.Amount',
       'Outs.Commercial.Number',
       'Outs.Commercial.Amount',
       'Outs.Total.Portfolio.Number',
       'Outs.Total.Portfolio.Amount',
       'NPL.Mortgage.Number',
       'NPL.Mortgage.Amount',
       'NPL.Purchase.of.Newly.Constructed.Houses.Number',
       'NPL.Purchase.of.Newly.Constructed.Houses.Amount',
       'NPL.Purchase.of.Existing.Houses.Number',
       'NPL.Purchase.of.Existing.Houses.Amount',
       'NPL.Home.Equity.Loans.Number',
       'NPL.Home.Equity.Loans.Amount',
       'NPL.Other.Retail.Number',
       'NPL.Other.Retail.Amount',
       'NPL.Educational.Loan.Number',
       'NPL.Educational.Loan.Amount',
       'NPL.Retail.Number',
       'NPL.Retail.Amount',
       'NPL.lessthan.1k.Number',
       'NPL.lessthan.1k.Amount',
       'NPL.between.1k.10k.Number',
       'NPL.between.1k.10k.Amount',
       'NPL.Micro.Number',
       'NPL.Micro.Amount',
       'NPL.between.10k.100k.Number',
       'NPL.between.10k.100k.Amount',
       'NPL.between.100k.200k.Number',
       'NPL.between.100k.200k.Amount',
       'NPL.between.200k.1m.Number',
       'NPL.between.200k.1m.Amount',
       'NPL.between.100k.1m.Number',
       'NPL.between.100k.1m.Amount',
       'NPL.between.1m.2m.Number',
       'NPL.between.1m.2m.Amount',
       'NPL.greaterthan.2m.Number',
       'NPL.greaterthan.2m.Amount',
       'NPL.Small.Number',
       'NPL.Small.Amount',
       'NPL.Medium.Number',
       'NPL.Medium.Amount',
       'NPL.SME.Number',
       'NPL.SME.Amount',
       'NPL.Corporate.Number',
       'NPL.Corporate.Amount',
       'NPL.Commercial.Number',
       'NPL.Commercial.Amount',
       'Total.NPL.Number',
       'Total.NPL.Amount',
       'Mortgage.Disb.Number',
       'Mortgage.Disb.Amount',
       'Disbursements.Purchase.of.Newly.Constructed.Houses.Number',
       'Disbursements.Purchase.of.Newly.Constructed.Houses.Amount',
       'Disbursements.Purchase.of.Existing.Houses.Number',
       'Disbursements.Purchase.of.Existing.Houses.Amount',
       'Disbursements.Home.Equity.Loans.Number',
       'Disbursements.Home.Equity.Loans.Amount',
       'Disbursements.Other.Retail.Number',
       'Disbursements.Other.Retail.Amount',
       'Disbursements.Educational.Loan.Number',
       'Disbursements.Educational.Loan.USD.Amount',
       'Retail.Disbursements.Number',
       'Retail.Disbursements.Amount',
       'Disb.lessthan.1k.Number',
       'Disb.lessthan.1k.Amount',
       'Disb.between.1k.10k.Number',
       'Disb.between.1k.10k.Amount',
       'Micro.Disbursements.Number',
       'Micro.Disbursements.Amount',
       'Disb.bewteen.10k.100k.Number',
       'Disb.bewteen.10k.100k.bewteen.Amount',
       'Disb.bewteen.100k.200k.Number',
       'Disb.bewteen.100k.200k.Amount',
       'Disb.bewteen.200k.1m.Number',
       'Disb.bewteen.200k.1m.Amount',
       'Disb.bewteen.100k.1m.Number',
       'Disb.bewteen.100k.1m.Amount',
       'Disb.bewteen.1m.2m.Number',
       'Disb.bewteen.1m.2m.Amount',
       'Disb.bewteen.2m.Number',
       'Disb.bewteen.2m.Amount',
       'Small.Disbursements.Number',
       'Small.Disbursements.Amount',
       'Medium.Disbursements.Number',
       'Medium.Disbursements.Amount',
       'SME.Disbursements.Number',
       'SME.Disbursements.Amount',
       'Corporate.Disbursements.Number',
       'Corporate.Disbursements.Amount',
       'Commercial.Disbursements.Number',
       'Commercial.Disbursements.Amount',
       'Total.Disbursements.Number',
       'Total.Disbursements.Amount',
       'Deposits.under.500.Number',
       'Deposits.under.500.Amount',
       'Deposits.between.500.1k.Number',
       'Deposits.between.500.1k.Amount',
       'Deposits.between.1k.10k.Number',
       'Deposits.between.1k.10k.Amount',
       'Deposits.between.10k.100k.Number',
       'Deposits.between.10k.100k.Amount',
       'Deposits.between.100k.1m.Number',
       'Deposits.between.100k.1m.Amount',
       'Deposits.between.1m.2m.Number',
       'Deposits.between.1m.2m.Amount',
       'Deposits.over.2m.Number',
       'Deposits.over.2m.Amount',
       'Libil.Individual.Number',
       'Libil.Individual.Amount',
       'Libil.Micro.Number',
       'Libil.Micro.Amount',
       'Libil.SME.Number',
       'Libil.SME.Amount',
       'Libil.Corporate.Number',
       'Libil.Corporate.Amount',
       'Libil.Government.Number',
       'Libil.Government.Amount',
       'Employees.lessthan.1k.Number',
       'Employees.between.1k.10k.Number',
       'Employees.between.10k.100k.Number',
       'Employees.between.100k.200k.Number',
       'Employees.between.200k.1m.Number',
       'Employees.between.1m.2m.Number',
       'Advanced',
       'IS.only',
       'IS.AS',
       'AS.only',
       'Type.of.Engagement',
       'IDA',
       'Conflict.Affected',
       'Excl.fm.PF.factor',
       'Excl.fm.NPL.factor',
       'Excl.fm.Disb.factor',
       'NPL.Ratio.Micro',
       'NPL.Ratio.Small',
       'NPL.Ratio.Medium',
       'Adjustments')

length(x) == length(colnames(reach))

colnames(reach) <- x

reach.msme <- read.xlsx('Data/Reach Data 2004-2014_Consolidated.xlsx', startRow = 6, sheet= 2) %>%
  select (`PARTNER.#`, MSME.Type, Year) %>%
  rename(Institution.Nbr = `PARTNER.#`, Reporting.Year = Year, 
         MSME.Type.Reconciled  = MSME.Type)

reach.msme$Institution.Year <- paste(reach.msme$Institution.Nbr, reach$Reporting.Year, sep='')

reach.msme$Institution.Nbr <- NULL
reach.msme$Reporting.Year <- NULL

#create ID
reach$Institution.Year <- paste(reach$Institution.Nbr, reach$Reporting.Year, sep='')


#This builds out a Yes if the reconciled MSME
# data has MSME listed in both 2014 and 2012
reach.2012.2014<- reach 

reach.2012.2014[is.na(reach.2012.2014)] <- 'not'

reach.2012.2014 <- reach.2012.2014 %>%
  full_join(reach.msme, 'Institution.Year') %>%
  select(Institution.Nbr, Reporting.Year, MSME.Type.Original) %>%
  filter(Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
  spread(key = Reporting.Year, value = MSME.Type.Original)

  reach.2012.2014[is.na(reach.2012.2014)] <- 'not'

reach.2012.2014 <- reach.2012.2014 %>%
  mutate(test = ifelse(`2014` == 'MSME', "Yes", "No")) %>%
  mutate(test2 = ifelse(`2012` == 'MSME', "Yes", "No")) %>%
  mutate(MSME.2012.or.2014 = ifelse(test == "Yes", "Yes",
                                     ifelse(test2 == "Yes", "Yes", NA))) %>%
  select(Institution.Nbr, MSME.2012.or.2014)

#test1 <- reach %>%
 # select(Institution.Nbr, Reporting.Year, Outs.SME.Number)%>%
  #filter(Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
#spread(key = Reporting.Year, value = Outs.SME.Number)
  

reach <- reach %>%
  left_join(reach.2012.2014, 'Institution.Nbr') %>%
  left_join(reach.msme, 'Institution.Year')



# this list should be in every file to clean the country names
require(stringr)
reach$Country.Name <-str_trim(reach$Country.Name)
reach$Country.Name[reach$Country.Name == "Afghanistan, I.R. of"] <- "Afghanistan"
reach$Country.Name[reach$Country.Name == "Azerbaijan, Rep. of"] <- "Azerbaijan"
reach$Country.Name[reach$Country.Name == "Bahrain, Kingdom of"] <- "Bahrain"
reach$Country.Name[reach$Country.Name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
reach$Country.Name[reach$Country.Name == "Cabo Verde"] <- "Cape Verde"
reach$Country.Name[reach$Country.Name == "Central African Republic"] <- "Central African Rep."
reach$Country.Name[reach$Country.Name == "Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
reach$Country.Name[reach$Country.Name == "Congo, Dem. Rep. of"] <- "Congo, Dem. Rep."
reach$Country.Name[reach$Country.Name == "China,P.R.: Mainland"] <- "China"
reach$Country.Name[reach$Country.Name == "China,P.R.:Hong Kong"] <- "Hong Kong SAR, China"
reach$Country.Name[reach$Country.Name == "China,P.R.:Macao"] <- "Macao SAR, China"
reach$Country.Name[reach$Country.Name == "Congo, Republic of"] <- "Congo, Rep."
reach$Country.Name[reach$Country.Name == "Cote D'Ivoire"] <- "Cote d'Ivoire"
reach$Country.Name[reach$Country.Name == "Cote D&#39;Ivoire"] <- "Cote d'Ivoire"
reach$Country.Name[reach$Country.Name == "C√¥te d'Ivoire"] <- "Cote d'Ivoire"
reach$Country.Name[reach$Country.Name == "Cura√ßao"] <- "Curacao"
reach$Country.Name[reach$Country.Name == "Egypt, Arab Republic of"] <- "Egypt, Arab Rep."
reach$Country.Name[reach$Country.Name == "Egypt"] <- "Egypt, Arab Rep."
reach$Country.Name[reach$Country.Name == "Faroe Islands"] <- "Faeroe Islands"
reach$Country.Name[reach$Country.Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
reach$Country.Name[reach$Country.Name == "Iran, I.R. of"] <- "Iran, Islamic Rep."
reach$Country.Name[reach$Country.Name == "Korea, Republic of"] <- "Korea, Rep."
reach$Country.Name[reach$Country.Name == "Lao People's Democratic Republic"] <- "Lao PDR"
reach$Country.Name[reach$Country.Name == "Lao People&#39;s Democratic Republic"] <- "Lao PDR"
reach$Country.Name[reach$Country.Name == "Lao People's Dem.Rep"] <- "Lao PDR"
reach$Country.Name[reach$Country.Name == "Macedonia, Former Yugoslav Republic of"] <- "Macedonia, FYR"
reach$Country.Name[reach$Country.Name == "Marshall Islands"] <- "Marshall Islands"
reach$Country.Name[reach$Country.Name == "Marshall Islands,Rep"] <- "Marshall Islands"
reach$Country.Name[reach$Country.Name == "Marshall Islands, Rep."] <- "Marshall Islands"
reach$Country.Name[reach$Country.Name == "Micronesia, Fed.Sts."] <- "Micronesia, Fed. Sts."
reach$Country.Name[reach$Country.Name == "São Tomé and Principe"] <- "Sao Tome and Principe"
reach$Country.Name[reach$Country.Name == "S√£o Tom√© and Principe"] <- "Sao Tome and Principe"
reach$Country.Name[reach$Country.Name == "S√£o Tom√© & Pr√≠ncipe"] <- "Sao Tome and Principe"
reach$Country.Name[reach$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela, RB"
reach$Country.Name[reach$Country.Name == "Yemen, Republic of"] <- "Yemen, Rep."
reach$Country.Name[reach$Country.Name == "Yemen"] <- "Yemen, Rep."
reach$Country.Name[reach$Country.Name == "Slovakia"] <- "Slovak Republic"
reach$Country.Name[reach$Country.Name == "Congo Republic"] <- "Congo, Dem. Rep."
reach$Country.Name[reach$Country.Name == "Saint Lucia"] <- "St. Lucia"
reach$Country.Name[reach$Country.Name == "St. Vincent & Grens."] <- "St. Vincent and the Grenadines"
reach$Country.Name[reach$Country.Name == "Serbia, Republic of"] <- "Serbia"
reach$Country.Name[reach$Country.Name == "Timor Leste"] <- "Timor-Leste"
reach$Country.Name[reach$Country.Name == "Venezuela, Rep. Bol."] <- "Venezuela"
reach$Country.Name[reach$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela"
reach$Country.Name[reach$Country.Name == "South Sudan, Rep. of"] <- "South Sudan"
reach$Country.Name[reach$Country.Name == "Anguilla"] <- "Anguila"
reach$Country.Name[reach$Country.Name == "Brunei Darussalam"] <- "Brunei"

#this was Serbia and Motenegro, changed to just Serbia (they are the same Income group)
reach$Country.Name[reach$Country.Name == "Serbia and Montenegro"] <- "Serbia"

#adding country code
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
reach$Country.Code <- NULL
reach <- left_join(reach, countries, 'Country.Name')


write.csv(reach, file = 'Clean.data/reach.csv')