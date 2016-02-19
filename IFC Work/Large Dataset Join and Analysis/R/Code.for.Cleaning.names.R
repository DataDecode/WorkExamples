#cleaning country names

income <- read.csv('Clean.data/Combined.data/income.csv')
idesk <- read.csv('Clean.data/idesk.csv')
findex <- read.csv('Clean.data/findex.csv')
dots.ratings <- read.csv('Clean.data/dotsratings2014.csv')
fas<- read.csv('Clean.data/fas.csv')
wdi <- read.csv('Clean.data/wdiCountries.csv')
mfa <- read.csv('Clean.data/mfa.csv')
reach <- read.csv('Clean.data/reach.csv')
x <- c(as.character(income$Country.Name),
       as.character(idesk$Country.Name),
       as.character(findex$Country.Name),
      as.character(dots.ratings$Country.Name),
      as.character(fas$Country.Name),
      as.character(wdi$Country.Name),
      as.character(mfa$Country.Name),
      as.character(reach$Country.Name))

x<- as.data.frame(x)
x$x <- as.character(x$x)
x$x <- str_trim(x$x)
x<- distinct(x)
x<- arrange(x, x)


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


#cleaning country code

income <- read.csv('Clean.data/Combined.data/income.csv')
idesk <- read.csv('Clean.data/idesk.csv')
findex <- read.csv('Clean.data/findex.csv')
dots.ratings <- read.csv('Clean.data/dotsratings2014.csv')
fas<- read.csv('Clean.data/fas.csv')
wdi <- read.csv('Clean.data/wdiCountries.csv')
mfa <- read.csv('Clean.data/mfa.csv')
reach <- read.csv('Clean.data/reach.csv')

income.l<- as.data.frame(cbind(as.character(income$Country.Name), as.character(income$Country.Code)))
income.l<- distinct(income.l)

findex.l<- as.data.frame(cbind(as.character(findex$Country.Name), as.character(findex$Country.Code)))
findex.l<- distinct(findex.l)

dots.ratings.l<- as.data.frame(cbind(as.character(dots.ratings$Country.Name), as.character(dots.ratings$Country.Code)))
dots.ratings.l<- distinct(dots.ratings.l)

fas.l<- as.data.frame(cbind(as.character(fas$Country.Name), as.character(fas$Country.Code)))
fas.l<- distinct(fas.l)

wdi.l<- as.data.frame(cbind(as.character(wdi$Country.Name), as.character(wdi$Country.Code)))
wdi.l<- distinct(wdi.l)

mfa.l<- as.data.frame(cbind(as.character(mfa$Country.Name), as.character(mfa$Country.Code)))
mfa.l<- distinct(mfa.l)

full <- full_join(income.l, findex.l, "V1") %>%
  full_join(dots.ratings.l, "V1") %>%
  full_join(fas.l, 'V1') %>%
  full_join(wdi.l, 'V1') %>%
  full_join(mfa.i, 'V1')


write.csv(full, 'Clean.data/Countrylist.csv')

x <- cbind(as.character(income$Country.Name), as.character(income$Country.Code),
       as.character(findex$Country.Name), as.character(findex$Country.Code),
       as.character(dots.ratings$Country.Name), as.character(findex$Country.Code),
       as.character(fas$Country.Name), as.character(fas$Country.Code),
       as.character(wdi$Country.Name), as.character(wdi$Country.Code),
       as.character(mfa$Country.Name), as.character(mfa$Country.Code))

foo <-

sapply(x,'[',1:foo)
