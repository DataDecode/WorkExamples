library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

static.data <- read.csv('Clean.data/Combined.data/static.csv')
static.data$X <- NULL


income <- read.csv('Clean.data/Combined.data/income.csv')
income$X <- NULL

# I want to take just the information I need
static.data.basic <- select(static.data,Institution.Nbr,
                            Tertiary.Sector.Code, 
                            Client.Type)

#mfa
mfa <- read.csv('Clean.data/mfa.csv', sep = ',')
mfa$X <- NULL
mfa$Country.Code.Year <- paste(mfa$Country.Code, mfa$Reporting.Year, sep='')

mfa <- select(mfa, -Institution.Legal.Name, -Statement.ID, -Statement.Date, 
                        -Statement.Number.of.Months.Covered,-Audit.Method,
                        -Statement.Status, -Statement.Type)

#adding country code
#should be on the individual sheet level
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
mfa$Country.Code <- NULL
mfa <- left_join(mfa, countries, 'Country.Name')

mfa$Country.Code.Year <- paste(mfa$Country.Code, mfa$Reporting.Year, sep='')

#link the data sets with the institution number and Country Income
mfa.with.data <- left_join(mfa,static.data.basic, by = "Institution.Nbr") %>%
  left_join(income, by = "Country.Code")

#creating the Size of the bank attribute
size <- select(mfa.with.data, Institution.Nbr,  Client.Type, Total.Assets) %>%
  distinct(Institution.Nbr) %>%
  group_by(Client.Type) %>%
  mutate(Bank.Size.percent = cume_dist(Total.Assets)) %>%
  mutate(Bank.Size = ifelse(between(Bank.Size.percent, 0, .33),"Small", 
                            ifelse(between(Bank.Size.percent, .33, .66), "Medium",
                                   ifelse(between(Bank.Size.percent, .66, 1), "Large", "NA")))) %>%
  select(Institution.Nbr, Bank.Size)

size$Client.Type <- NULL

mfa.with.data<- left_join(mfa.with.data, size, "Institution.Nbr")

mfa.with.data[mfa.with.data == 0] <- NA


# REACH SERVEY DATA
reach<- read.csv('Clean.data/reach.csv')

reach$Country.Code.Year<- paste(reach$Country.Code, reach$Reporting.Year, sep='')

reach <- select(reach, Institution.Year, Reporting.Year, Institution.Nbr,
  Institution.Short.Name, Region.Acronym, Regional.Name, Country.Name,
  contains('SME'), Total.NPL.Number, Total.NPL.Amount, Country.Code) # %>% took out MSME.Type
  #We only want institutions taged as MSME in the MSME.Type Category  
  #filter(MSME.Type == 'MSME')

reach$Institution.Year <- as.character(reach$Institution.Year) %>%
  as.numeric()

#CAGR of SME loan Number- UPDATED
reach.CAGR <- select(reach, Institution.Year, Reporting.Year, Institution.Nbr, Outs.SME.Number)

reach.change.in.SME.CAGR.Number <- filter(reach.CAGR, Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
  spread(key = Reporting.Year, value = Outs.SME.Number)

e.2012 <- select(reach.change.in.SME.CAGR.Number, Institution.Nbr, `2012`) %>%
  arrange(`2012`) %>%
  distinct(Institution.Nbr)

e.2014 <- select(reach.change.in.SME.CAGR.Number, Institution.Nbr, `2014`) %>%
  arrange(`2014`) %>%
  distinct(Institution.Nbr)

reach.change.in.SME.CAGR.Number <- full_join(e.2012, e.2014, "Institution.Nbr") %>%
  mutate(SME.CAGR.Number.2012.2014 = ((`2014` / `2012`)^(1/3)-1)) %>%
  select(Institution.Nbr, SME.CAGR.Number.2012.2014)

reach.change.in.SME.CAGR.Number$Institution.Year <- paste(reach.change.in.SME.CAGR.Number$Institution.Nbr,
                                                          '2014',
                                                          sep='') %>%
  as.numeric()

reach.change.in.SME.CAGR.Number$Institution.Nbr <- NULL

#CAGR of SME loan Amount - UPDATED
reach.CAGR.Amount <- select(reach, Institution.Year, Reporting.Year, Institution.Nbr, Outs.SME.Amount)

reach.change.in.SME.CAGR.Amount <- filter(reach.CAGR.Amount, Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
  spread(key = Reporting.Year, value = Outs.SME.Amount)

e.2012.amt <- select(reach.change.in.SME.CAGR.Amount, Institution.Nbr, `2012`) %>%
  arrange(`2012`) %>%
  distinct(Institution.Nbr)

e.2014.amt <- select(reach.change.in.SME.CAGR.Amount, Institution.Nbr, `2014`) %>%
  arrange(`2014`) %>%
  distinct(Institution.Nbr)

reach.change.in.SME.CAGR.Amount <- full_join(e.2012.amt, e.2014.amt, "Institution.Nbr") %>%
  mutate(SME.CAGR.Amount.2012.2014 = ((`2014` / `2012`)^(1/3)-1)) %>%
  select(Institution.Nbr, SME.CAGR.Amount.2012.2014)

reach.change.in.SME.CAGR.Amount$Institution.Year <- paste(reach.change.in.SME.CAGR.Amount$Institution.Nbr,
        '2014',
        sep='') %>%
  as.numeric()

reach.change.in.SME.CAGR.Amount$Institution.Nbr <- NULL
  
#Change in # of SME loans- UPDATED
reach.change.in.SME.Number <- select(reach, Institution.Year, Reporting.Year, 
                                     Institution.Nbr, Outs.SME.Number) %>%
  filter(Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
  spread(key = Reporting.Year, value = Outs.SME.Number)

e.2012.nbr <- select(reach.change.in.SME.Number, Institution.Nbr,`2012`) %>%
  arrange(`2012`) %>%
  distinct(Institution.Nbr)
e.2012.nbr[e.2012.nbr == 0] <- NA   

e.2014.nbr <- select(reach.change.in.SME.Number, Institution.Nbr,`2014`) %>%
  arrange(`2014`) %>%
  distinct(Institution.Nbr) 
e.2014.nbr[e.2014.nbr == 0] <- NA  

reach.change.in.SME.Number <- full_join(e.2012.nbr, e.2014.nbr, "Institution.Nbr") %>%
  mutate(SME.Change.2012.2014.Number = `2014` - `2012`) %>%
  select(Institution.Nbr, SME.Change.2012.2014.Number)

reach.change.in.SME.Number$Institution.Year <- paste(reach.change.in.SME.Number$Institution.Nbr,
                                                          '2014',
                                                          sep='') %>%
  as.numeric()

#Change in Amount of SME loans - UPDATED
reach.change.in.SME.Amount <- select(reach, Institution.Year, Reporting.Year, 
                                     Institution.Nbr, Outs.SME.Amount) %>%
  filter(Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
  spread(key = Reporting.Year, value = Outs.SME.Amount)

e.2012.amt <- select(reach.change.in.SME.Amount, Institution.Nbr,`2012`) %>%
  arrange(`2012`) %>%
  distinct(Institution.Nbr)
e.2012.amt[e.2012.amt == 0] <- NA 

e.2014.amt <- select(reach.change.in.SME.Amount, Institution.Nbr,`2014`) %>%
  arrange(`2014`) %>%
  distinct(Institution.Nbr) 
e.2014.amt[e.2014.amt == 0] <- NA

reach.change.in.SME.Amount <- full_join(e.2012.amt, e.2014.amt, "Institution.Nbr") %>%
  mutate(SME.Change.2012.2014.Amount = `2014` - `2012`) %>%
  select(Institution.Nbr, SME.Change.2012.2014.Amount)

reach.change.in.SME.Amount$Institution.Year <- paste(reach.change.in.SME.Amount$Institution.Nbr,
                                                     '2014',
                                                     sep='') %>%
  as.numeric()

#Calulating Loan to Deposit Ratio
reach.loan<- read.csv('Clean.data/reach.csv')

reach.loan <- select(reach.loan, Institution.Year,Institution.Nbr, Reporting.Year, Outs.Total.Portfolio.Amount, 
                     contains('Deposits'))

#making the NAs Zero for addition; the group sum doesn't add if there are NAs
reach.loan <- mutate(reach.loan, 
                     Deposits.over.2m.Amount = ifelse(is.na(Deposits.over.2m.Amount),0,Deposits.over.2m.Amount),
                     Deposits.under.500.Amount = ifelse(is.na(Deposits.under.500.Amount),0,Deposits.under.500.Amount),
                     Deposits.between.1m.2m.Amount = ifelse(is.na(Deposits.between.1m.2m.Amount),0,Deposits.between.1m.2m.Amount),
                     Deposits.between.500.1k.Amount = ifelse(is.na(Deposits.between.500.1k.Amount),0,Deposits.between.500.1k.Amount),
                     Deposits.between.1k.10k.Amount = ifelse(is.na(Deposits.between.1k.10k.Amount),0,Deposits.between.1k.10k.Amount),
                     Deposits.between.100k.1m.Amount = ifelse(is.na(Deposits.between.100k.1m.Amount),0,Deposits.between.100k.1m.Amount),
                     Deposits.between.10k.100k.Amount = ifelse(is.na(Deposits.between.10k.100k.Amount),0,Deposits.between.10k.100k.Amount)
                     ) %>%
  # Adding the Accounts together
  mutate(Total.Deposits.Amount = Deposits.over.2m.Amount + Deposits.under.500.Amount
                     + Deposits.between.1m.2m.Amount + Deposits.between.500.1k.Amount
                     + Deposits.between.1k.10k.Amount + Deposits.between.100k.1m.Amount
                     + Deposits.between.10k.100k.Amount) %>%
  mutate(Loan.Deposit.Ratio = Outs.Total.Portfolio.Amount / Total.Deposits.Amount)

#creating the variable to attach to other tables
reach.loan$Institution.Year <- paste(reach.loan$Institution.Nbr, reach.loan$Reporting.Year, sep='')
reach.loan$Institution.Year <- as.numeric(reach.loan$Institution.Year)

reach.loan <- select(reach.loan, Institution.Year, Outs.Total.Portfolio.Amount, 
                     Total.Deposits.Amount, Loan.Deposit.Ratio)


#Joining things together
reach.1<- full_join(reach.change.in.SME.Number, reach.change.in.SME.CAGR.Number, "Institution.Year") %>% 
  full_join(reach.change.in.SME.CAGR.Amount, "Institution.Year") %>%
  full_join(reach.change.in.SME.Amount, "Institution.Year") %>%
  full_join(reach.loan, 'Institution.Year')

reach <- left_join(reach, reach.1, "Institution.Year")

#NPL for SME loans for 2014
#This is the total NPL for the SMEs against the NPLs for the institution
reach<- mutate(reach, SME.NPL.Loans.to.Total.NPL.ratio = (NPL.SME.Amount/Total.NPL.Amount)) %>%
  mutate(SME.NPL.to.Total.SME.Amount.ratio = NPL.SME.Amount/ Outs.SME.Amount) %>%
  mutate(SME.NPL.to.Total.SME.Number.ratio = NPL.SME.Number/ Outs.SME.Number)

reach[reach == 0] <- NA   
reach[reach == 'Inf'] <- NA

# Pull in columns from DOTS
dots <- read.csv('Clean.data/dotsindicator.csv', sep = ',')

dots<- select(dots, Institution.Year, ROE.Annual.ROE, Annual.ROE.Spread.ROE.minus.COE, 
              ROE.Annual.Risk.Premium)

# Bring the 3 reports together into one
final.sme <- full_join(mfa.with.data, dots, "Institution.Year") %>%
  # mfa.with.data has income and static institutional data in it already
  #restricing analysis to 2012 to 2014
  #filter(Reporting.Year >=2012, Reporting.Year <= 2014) %>%
  # Getting rid of the duplicate columns before merging
  select(-Institution.Short.Name, -Institution.Nbr,
         -Regional.Name, -Country.Name.x, -Country.Name.y,
         -Institution.Nbr, -Reporting.Year, -Country.Code,
         -Country.Income.Group, -Country.Lending.Category)

#removing unwanted columns
income <- income %>% select(-Country.Name)

test<- reach %>%
  left_join(income, "Country.Code") %>%
  full_join(final.sme, "Institution.Year") #%>%
  #filter(MSME.Type == 'MSME')

#I want to add in any additional information I can for client type
clients <- select(static.data.basic, Institution.Nbr, Client.Type)

test<- test %>%
  select(-Client.Type) %>%
  left_join(clients, "Institution.Nbr")

#adding country code
#should be on the individual sheet level
countries <- read.csv('Clean.data/Country.List/CountryList.csv')
test$Country.Code <- NULL
test <- left_join(test, countries, 'Country.Name')

test$Country.Code.Year <- paste(test$Country.Code, test$Reporting.Year, sep='')

#write to CSV
write.csv(test, 'Clean.data/finalSME.csv')

test1 <- test %>%
  filter(Reporting.Year == 2014)

write.csv(test1, 'Clean.data/finalSME2014.csv')


#The follow is tests to compare how well the data
#grab is compared with the Excel alternative

#CAGR Number
test1<- test %>% 
  filter(!is.na(SME.CAGR.2012.2014.Number)) %>%
  group_by('Country.Income.Group')
    count(test1, Country.Income.Group)

#CAGR Amount
    test1<- test %>% 
      filter(!is.na(SME.CAGR.2012.2014.Amount)) %>%
      group_by('Country.Income.Group')
    count(test1, Country.Income.Group)    
    
# Change in SME Number
  test1<- test %>% 
    filter(!is.na(Change.in.SME.Number.2012.2014)) %>%
    group_by('Country.Income.Group')
  
  count(test1, Country.Income.Group)  

# Change in SME Amount
  test1<- test %>% 
    filter(!is.na(Change.in.SME.Amount.2012.2014)) %>%
    group_by('Country.Income.Group')

  count(test1, Country.Income.Group)  

# ROE
  test1<- test %>% 
    filter(!is.na(ROE.Annual.ROE)) %>%
    filter(Reporting.Year == 2014) %>%
    group_by('Country.Income.Group')

  count(test1, Country.Income.Group)   
  
  # ROE Spread
  test1<- test %>% 
    filter(!is.na(Annual.ROE.Spread.ROE.minus.COE)) %>%
    filter(Reporting.Year == 2014) %>%
    group_by('Country.Income.Group')

  count(test1, Country.Income.Group)
  
# NPL SME loan ratio
  test1<- test %>% 
    filter(!is.na(NPL.SME.Loans.to.total.NPL.ratio)) %>%
    filter(Reporting.Year == 2014)
    group_by('Country.Income.Group')
  count(test1, Country.Income.Group)  
   
#MFA Net Interest Spread
  test1<- test %>% 
    filter(!is.na(Net.Interest.Spread)) %>%
    filter(Reporting.Year == 2014) %>%
    group_by('Country.Income.Group')
  
  count(test1, Country.Income.Group)
  

  
  

#FAS and USD Coversion
fas <- read.csv('Clean.data/fas.csv', sep = ',')
fas.SME <- select(fas, Country.Name,
                  Reporting.Year,
                  Country.Code.Year,
                  contains("SME"), Official.exchange.rate.LCU.Per.USD.period.Average) 

countrylist <- read.csv('Clean.data/Country.List/CountryList.csv')

fas.SME <- left_join(fas.SME, countrylist, "Country.Name")

#FAS Amount and CAGR.Amount
fas.SME.Amount <- fas.SME
fas.SME.Amount[is.na(fas.SME.Amount)] <- 0

fas.SME.Amount<- fas.SME.Amount %>%
  mutate(SME.Outs.USD = Commercial.banks.SME.Outstanding.Loans.USD +
           Credit.unions.and.financial.cooperatives.SME.Outstanding.Loans.USD +
           Deposit.taking.MFIs.SME.Outstanding.Loans.USD +
           Non.deposit.taking.MFIs.loans.from.SMEs.USD +
           Other.financial.intermediaries.loans.from.SMEs.USD +
           Other.Deposit.Takers.SME.outstanding.loans.USD) 

fas.SME.Amount[fas.SME.Amount == 0] <- NA

fas.SME.Amount <- select(fas.SME.Amount, Country.Name, Reporting.Year, SME.Outs.USD) %>%
  filter(Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
  spread(key=Reporting.Year , value = SME.Outs.USD) %>%
  mutate(fas.SME.Change.2012.2014.Amount = `2014` - `2012`) %>%
  mutate(fas.SME.CAGR.2012.2014.Amount = ((`2014`/`2012`)^(1/3)-1)) %>%
  select(-`2012`, -`2013`,-`2014`)

#FAS Number and CAGR.Number
fas.SME.Number <- fas.SME
fas.SME.Number[is.na(fas.SME.Number)] <- 0

fas.SME.Number <- fas.SME.Number %>%
  mutate(SME.Outs.Number = Commercial.banks.SME.loan.accounts + 
           Credit.unions.and.financial.cooperatives.SME.loan.accounts +
           Deposit.taking.MFIs.SME.loan.accounts +
           Non.deposit.taking.MFIs.SME.loan.accounts +
           Other.financial.intermediaries.SME.loan.accounts +
           Other.Deposit.Takers.SME.loan.accounts)

fas.SME.Number[fas.SME.Number == 0] <- NA

fas.SME.Number <- select(fas.SME.Number, Country.Name, Reporting.Year, SME.Outs.Number) %>%
  filter(Reporting.Year >= 2012, Reporting.Year <= 2014) %>%
  spread(key=Reporting.Year , value = SME.Outs.Number) %>%
  mutate(fas.SME.Change.2012.2014.Number = `2014` - `2012`) %>%
  mutate(fas.SME.CAGR.2012.2014.Number = ((`2014`/`2012`)^(1/3)-1))%>%
  select(-`2012`, -`2013`,-`2014`)%>%
  # Joining the two together
  full_join(fas.SME.Amount, "Country.Name") %>%
  left_join(countrylist, "Country.Name")


fas.SME.Number$Country.Code.Year <- paste(fas.SME.Number$Country.Code,
                                             '2014', sep='')

fas.SME.Number$Country.Code <- NULL

fas.SME <- left_join(fas.SME, fas.SME.Number)
  
write.csv(fas.SME, file = 'Clean.data/FASforSMEfinal.csv')


#NOW Combine all of the databases together
# MAY NOT WANT TO RUN

final <- left_join(test, wdi.fas, by = "Country.Code.Year")

final$Reporting.Year.y.y <- NULL
final$Reporting.Year.y.x <- NULL
final$Reporting.Year.x.y <- NULL
final$Country.Code.y <- NULL


write.csv(final, file = 'Clean.data/finalSME.csv')

