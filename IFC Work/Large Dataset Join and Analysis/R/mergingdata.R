library(openxlsx)
library(dplyr)
library(magrittr)
library(tidyr)

#MERGING STATIC COUNTRY and INSTITUTIONAL DATA
#country income
income<- read.csv('Clean.data/countryIncomeCategory.csv', sep=',')
income$X <- NULL

write.csv(income, file = 'Clean.data/Combined.data/income.csv')

#sector codes
sector.codes <- read.csv('Clean.data/FIGsectorCodes.csv', sep=',')
sector.codes$X <- NULL
#sector.codes$Tertiary.Sector.Code <- as.character(sector.codes$Tertiary.Sector.Code)

#there are multiple lines with the same Tertiary Sector.Code 
#so we need ot take out the distinct values 
#then remove the unneeded columns as they have
#conflicting information
distinct.sector.codes<- distinct(sector.codes, Tertiary.Sector.Code)
distinct.sector.codes$Sector.ID <- NULL
distinct.sector.codes$SME.Flag <- NULL
distinct.sector.codes$Business.Line.Project <- NULL
distinct.sector.codes$Business.Line.for.Profit.Tables<- NULL


#idesk
idesk<- read.csv('Clean.data/idesk.csv', sep=',')
idesk$X <- NULL
# idesk has a different set of country names than the other data sets, so 
# we need to correct the wrong country names so it lines up with the
#other datasets

wrong <- c("Cape Verde",
           "Congo, Democratic Republic of",
           "Congo, Republic of",
           "Cote D'Ivoire",
           "Egypt, Arab Republic of",
           "Iran, Islamic Republic of",
           "Korea, Republic of",
           "Lao People's Democratic Republic",
          "Macedonia, Former Yugoslav Republic of",
           "Sao Tome and Principe",
           "Venezuela, Republica Bolivariana de",
           "Yemen, Republic of")

right <- c("Cabo Verde",
           "Congo, Dem. Rep.",
           "Congo, Rep.",
           "Côte d'Ivoire",
           "Egypt, Arab Rep.",
           "Iran, Islamic Rep.",
           "Korea, Rep.",
           "Lao PDR",
           "Macedonia, FYR",
           "São Tomé and Principe",
           "Venezuela, RB",
           "Yemen, Rep.")

#this should be a loop
idesk$Country.Name <- as.character(idesk$Country.Name)
idesk$Country.Name[idesk$Country.Name == "Cape Verde"] <- "Cabo Verde"
idesk$Country.Name[idesk$Country.Name == "Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
idesk$Country.Name[idesk$Country.Name == "Congo, Republic of"] <- "Congo, Rep."
idesk$Country.Name[idesk$Country.Name == "Cote D'Ivoire"] <- "Côte d'Ivoire"
idesk$Country.Name[idesk$Country.Name == "Egypt, Arab Republic of"] <- "Egypt, Arab Rep."
idesk$Country.Name[idesk$Country.Name == "Iran, Islamic Republic of"] <- "Iran, Islamic Rep."
idesk$Country.Name[idesk$Country.Name == "Korea, Republic of"] <- "Korea, Rep."
idesk$Country.Name[idesk$Country.Name == "Lao People's Democratic Republic"] <- "Lao PDR"
idesk$Country.Name[idesk$Country.Name == "Macedonia, Former Yugoslav Republic of"] <- "Macedonia, FYR"
idesk$Country.Name[idesk$Country.Name == "Sao Tome and Principe"] <- "São Tomé and Principe"
idesk$Country.Name[idesk$Country.Name == "Venezuela, Republica Bolivariana de"] <- "Venezuela, RB"
idesk$Country.Name[idesk$Country.Name == "Yemen, Republic of"] <- "Yemen, Rep."


idesk$Tertiary.Sector.Name <- as.character(idesk$Tertiary.Sector.Name)
distinct.sector.codes$Tertiary.Sector.Code <- as.character(distinct.sector.codes$Tertiary.Sector.Code)
static.data <- left_join(idesk,distinct.sector.codes, by = 'Tertiary.Sector.Code')

static.data$Country.Name[static.data$Country.Name == "Côte d'Ivoire"] <- "Cote d'Ivoire"
static.data$Country.Name[static.data$Country.Name == "São Tomé and Principe"] <- "Sao Tome and Principe"

#static.data<- left_join(static.data,income, by = "Country.Name")
#test<- full_join(static.data,income, by = "Country.Name")

write.csv(static.data, file = 'Clean.data/Combined.data/static.csv')


#findex
findex <- read.csv('Clean.data/findex.csv')
findex$X <- NULL

#need to join all these together

#YEARLY VARIABLES
#fas
fas <- read.csv('Clean.data/fas.csv', sep = ',')
fas$X <- NULL

# need to change these with the exchange rate data

#mfa
mfa <- read.csv('Clean.data/mfa.csv', sep = ',')
mfa$X <- NULL
# need to combine with the sector codes
# to determine the average size of each sector



#reach
reach <- read.csv('Clean.data/reach.csv', sep = ',')
reach$X <- NULL

#dotsindicator
dotsindicator <- read.csv('Clean.data/dotsindicator.csv', sep = ',')
dotsindicator$X <- NULL


# JUST 2014 DATA
#dotsrating
dotsrating<- read.csv('Clean.data/dotsratings2014.csv', sep=',')
dotsrating$X <- NULL

dotsrating.merge <- select(dotsrating, Institution.number, Country.ID., Project.Stage,
                           Project.Size, Project.DOTS.Rating.Entered, DOTS.Development.Outcome.Rating,
                           DOTS.Financial.Performance.Rating, DOTS.Economic.Performance.Rating,
                           DOTS.Environmental...Social.Performance.Rating, DOTS.Private.Sector.Development.Impact.Rating,
                           DOTS.Average.Indicator.Rating)

colnames(dotsrating.merge)[1]<- 'CUSTOMERID'

#This is the data we are using across all time periods of analysis.  Much of it is 2014 or 2015
#data, but we are taking it as equal throughout time here.



mfa<- read.csv('Clean.data/mfa.csv', sep=',')
mfa$X <- NULL


newdata <- left_join(mfa, idesk.merge, by = 'CUSTOMERID')

#again, dotsrating are not based on yea, so it will be in every year of each institution
dotsrating.merge <- select(dotsrating, Institution.number, Country.ID., Project.Stage,
                           Project.Size, Project.DOTS.Rating.Entered, DOTS.Development.Outcome.Rating,
                           DOTS.Financial.Performance.Rating, DOTS.Economic.Performance.Rating,
                           DOTS.Environmental...Social.Performance.Rating, DOTS.Private.Sector.Development.Impact.Rating,
                           DOTS.Average.Indicator.Rating)

colnames(dotsrating.merge)[1]<- 'CUSTOMERID'

test <- left_join(newdata, dotsrating.merge, by = "CUSTOMERID")

