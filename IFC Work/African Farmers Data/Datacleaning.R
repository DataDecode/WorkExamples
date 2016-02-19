library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

data <- read.csv('cleaned_data_Aug24@1.csv', header = TRUE)

#section 1
#for household calcualtions
household.df <- data %>%
  select(hhsize, educhead, literate, language, qnationality, qethnicity)

hhsize<- count(household.df, hhsize)
quantile(household.df$hhsize, na.rm=TRUE)


household.df$educhead <- as.numeric(as.character(household.df$educhead))

educhead<- count(household.df, educhead)
quantile(household.df$educhead, na.rm=TRUE)

count(household.df, literate)

literate.df <- data %>%
  select(educhead, literate) %>%
  #filter(educhead == '0') %>%
  count(literate)

#What languge was the interview done in?
# THERE are multiple entries in some comlumns
language.df <- data %>%
  count(language)

#section 1
#for ethnicity calcualtion
nationality.df <- data %>%
  select(qnationality, qethnicity)

nationality.df$qnationality <- as.numeric(as.character(nationality.df$qnationality))
nationality.df$qethnicity <- as.numeric(as.character(nationality.df$qethnicity))

count(nationality.df, qnationality)
count(nationality.df, qethnicity)

nationalitysubset.df <- nationality.df %>%
  filter( qnationality == '1') %>%
  count(qethnicity)
  

#mobile phones
mobile.df <- data %>%
  select(hhsize, qnumbermobiles, qsimcardnumber, qno_mobile_but_use,
         qmobileoperator, qsmsuser, has_mobilephone)

mobile.df$qnumbermobiles <- as.numeric(as.character(mobile.df$qnumbermobiles))
mobile.df$qsimcardnumber <- as.numeric(as.character(mobile.df$qsimcardnumber))
mobile.df$has_mobilephone <- as.numeric(as.character(mobile.df$has_mobilephone))

mobile.df <- mobile.df %>%
  mutate(mobiles.per.person = qnumbermobiles / hhsize) %>%
  mutate(simcards.per.person = qsimcardnumber/ hhsize)

count(mobile.df, has_mobilephone)
number.mobiles<- count(mobile.df, qnumbermobiles)
quantile(mobile.df$qnumbermobiles, na.rm=TRUE)

quantile(mobile.df$mobiles.per.person, na.rm=TRUE)
quantile(mobile.df$simcards.per.person, na.rm=TRUE)

mobile.df$qmobileoperator <- as.numeric(as.character(mobile.df$qmobileoperator))
count(mobile.df, qmobileoperator)




qplot(mobiles.per.person, mobile.df, geom='histogram')


#section 10
#finacial account with an institution
financial.df <- data %>%
  select(X_101, #Q 10.1 do you have a finacial institution
         X_102, #What institution?
         X_104, #Have you borrowed money in the past 12 months?
         X_105, #When do you borrow money normally?
         X_107, #Who do you borrow money?
         X_109, #Why did you take out money
         X_1014, #What was the amount of your last loan?
         X_1026 #Where do you keep your money for daily household expenses?
         ) 

count(financial.df, X_101)

#Section 10
#mobile money
mobilemoney.df <- data %>%
  select(has_mobilephone, # has a mobile number
         qmobileoperator, #mobile operator
         X_1041, #Do you use mobile money?
         X_1055, #If not why?
         X_1042, #If so, what is the main operator ?
         X_1043, #Do you know the money moving point (for the main operator) closest to your home?
         X_1044, #How long does it take to go (on foot ) to the moving point nearest money ?
         X_1045, #How parmie sentence the following sentences are you most agree ? The mobile money is ...
         X_1046, #Why do you use the mobile money service? What is the main reason?
         X_1048,  #What types of services have you used ?
         X_1049, #Did you send money by mobile money in the last six months?
         X_1050, #If so, where have you sent in the past six months?
         X_1051, #Do you use electronic wallet / mobile money to keep your money ?
         X_1054, #If your cooperative offers to pay you a portion of your production via mobile money, would you accept ?
         X_1054_whynot #If not, why?
         )

mobilemoney.df$X_1041 <- as.character(mobilemoney.df$X_1041)

count(mobilemoney.df, X_1041)

mobilemoney.df$X_1055 <- as.character(mobilemoney.df$X_1055)
count(mobilemoney.df, X_1055)

count(mobilemoney.df, X_1042)

count(mobilemoney.df, X_1051)

count(mobilemoney.df, X_1054)

#cross checking the people that use Orange use Orange Money
#LOOK At MORE
mobilecheck.df <- mobilemoney.df %>%
  filter(qmobileoperator == 1) %>%
  count(X_1042)


#Section 4
#employment
employment.df <- data %>%
  select(X_41, #How many people work in your permanent planting ?
         X_42, #How do you use people contantly?
         X_44, # Apart from the workers , which are the other person permanently ?
         X_45, #Apart from those working in permance , is that there are others who work in your cocoa plantations ( occassionellement ) ?
         X_46, #How do you pay your maneuvers ?
         X_425, #Compared to the 2013-2014 campaign , have you been using more or less agricultural laborers ?
         X_426 #If less , what was the main reason for the decline of employment in relation to the previous harvest ?
         )

#cross check this with the size of the plots
#cross check with the yeilds

count(employment.df, X_41)


employment.df$X_42 <- as.numeric(as.character(employment.df$X_42))
quantile(employment.df$X_42, na.rm=TRUE)

#Section 5
# Tools
tools.df <- data %>%
  select(X_51, #Do you use pesticides ( including insecticedes , fungicides) ?
         X_52, #How often do you treat your planting in the year ?
         X_53, #Do you use pesticides regularly or only when necessary?
         X_54, #Did you have access to pesticides when you needed it ?
         X_56, #What tools do you use
         X_57, #Are these tools were available when you needed it ?
         X_57no #Machine Failure
  )

tools.df$X_52 <- as.numeric(as.character(tools.df$X_52))
quantile(tools.df$X_52, na.rm =TRUE)

count(tools.df, X_51)       

#Section 9
#Cargill Questions
Cargill.df <- data %>%
  select(X_97, #Do you know Cargill?
         cargill_98, #have you used the service " Farmer Field School " Cargill ?
         cargill_99, #Have you taken advantage of the service "Coop Academy " Cargill ?
         cargill_910, #Do you know of Cargill other activities?
         cargill_913 #Are you interested to receive other services for Cargill cocoa production ?
          )

count(Cargill.df, cargill_910)


#Section 8
#Contentment at farming
farmingfeeling.df <- data %>%
  select(X_86, #I am happy being a planeter
         X_89, #Are you always able to buy food for your household?
         X_810, #What months are hard to buy food?
         X_812 #How have you manged low prices or bad harvest?
  )

#OTHER QUESTIONS
othermobile.df <- data %>%
  select(has_mobilephone, # has a mobile number
          qmobileoperator, #mobile operator
          X_1041, #Do you use mobile money?
         X_1042, #Mobile money main operator ?
         X_1045, #How parmie sentence the following sentences are you most agree ? The mobile money is ...
         X_1046, #Why do you use the mobile money service? What is the main reason?
         X_1054, #If your cooperative offers to pay you a portion of your production via mobile money, would you accept ?
         X_101 #Q 10.1 do you have a finacial institution
  )

othermobile.df$qmobileoperator <- as.numeric(as.character(othermobile.df$qmobileoperator))          
#othermobile.df$X_1041 <- as.numeric(as.character(othermobile.df$X_1041))       
othermobile.df$X_1042 <- as.numeric(as.character(othermobile.df$X_1042))  

#can't use this with multiple mobile managers in each home.
Orange <- othermobile.df %>%
  filter(has_mobilephone == 1) %>%
  filter(qmobileoperator == 1) %>%
  count(X_1042)

MNT <- othermobile.df %>%
  filter(has_mobilephone == 1) %>%
  filter(qmobileoperator == 2) %>%
  count(X_1042)
         
mobilemoney.df <- data %>%
  select(has_mobilephone, # has a mobile number
         qmobileoperator, #mobile operator
         X_1041, #Do you use mobile money?
         X_1055, #If not why?
         X_1042, #If so, what is the main operator ?
         X_1043, #Do you know the money moving point (for the main operator) closest to your home?
         X_1044, #How long does it take to go (on foot ) to the moving point nearest money ?
         X_1045, #How parmie sentence the following sentences are you most agree ? The mobile money is ...
         X_1046, #Why do you use the mobile money service? What is the main reason?
         X_1048,  #What types of services have you used ?
         X_1049, #Did you send money by mobile money in the last six months?
         X_1050, #If so, where have you sent in the past six months?
         X_1051, #Do you use electronic wallet / mobile money to keep your money ?
         X_1054, #If your cooperative offers to pay you a portion of your production via mobile money, would you accept ?
         X_1054_whynot #If not, why?
  )


financial.df <- data %>%
  select(X_101, #Q 10.1 do you have a finacial institution
         X_102, #What institution?
         X_104, #Have you borrowed money in the past 12 months?
         X_105, #When do you borrow money normally?
         X_107, #Who do you borrow money?
         X_109, #Why did you take out money
         X_1014, #What was the amount of your last loan?
         X_1026 #Where do you keep your money for daily household expenses?
  ) 

#cross checking the people that use Orange use Orange Money
#LOOK At MORE
mobilecheck.df <- mobilemoney.df %>%
  filter(qmobileoperator == 1) %>%
  count(X_1042)


#size of farms
number_plots
X_21 number of hectors 

