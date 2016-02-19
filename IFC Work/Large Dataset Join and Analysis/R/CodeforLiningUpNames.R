# making sure all the variables have similar names
income.names <- colnames(income)
sector.codes.names <- colnames(sector.codes)
findex.names <- colnames(findex)
idesk.names <- colnames(idesk)

dotsindicator.names <- colnames(dotsindicator)
fas.names <- colnames(fas)
mfa.names <- colnames(mfa)
reach.names <- colnames(reach)
dotsrating.names <- colnames(dotsrating)

n <- max(length(income.names), length(sector.codes.names),
         length(findex.names), length(idesk.names),
         length(dotsindicator.names), length(fas.names),
         length(mfa.names), length(reach.names),
         length(dotsrating.names))

length(income.names) <- n
length(sector.codes.names)<- n
length(findex.names)<- n
length(idesk.names)<- n
length(dotsindicator.names) <- n
length(fas.names)<- n
length(mfa.names)<- n
length(reach.names)<- n
length(dotsrating.names) <- n

names.all.variables <- cbind(income.names, sector.codes.names,
                             findex.names, idesk.names,
                             dotsindicator.names, fas.names,
                             mfa.names, reach.names, dotsrating.names)

write.csv(names.all.variables, 'Clean.data/colnames.csv')

#checking that all the country codes are the same
findex.cc <- paste(findex$Country.Name, findex$Country.Code, sep = '')
idesk.cc <- paste(idesk$Institution.Country, idesk$IBRD.Country.Code, sep = '')
fas.cc <- paste(fas$Country.Names, fas$ISO.Code, sep='')
mfa.cc <- paste(mfa$COUNTRY_NME, mfa$COUNTRY_CODE, sep='')
dotsrating.cc <- paste(dotsrating$Country.name, dotsrating$Country.ID., sep='')


findex.cc <- data.frame(findex.cc)
idesk.cc <- data.frame(idesk.cc)
fas.cc <- data.frame(fas.cc)
mfa.cc <- data.frame(mfa.cc)
dotsrating.cc <- data.frame(dotsrating.cc)

names(findex.cc) <- paste('names')
names(idesk.cc) <- paste('names')
names(fas.cc) <- paste('names')
names(mfa.cc) <- paste('names')
names(dotsrating.cc) <- paste('names')

test<- setdiff(findex.cc, fas.cc)

test<- full_join(findex.cc, fas.cc, by = 'names')
test<- full_join(test, mfa.cc, by = 'names')
test<- full_join(test, dotsrating.cc, by = 'names')

write.csv(test, 'Clean.data/test.csv')

fas.cc$fas.cc

n.cc <- max(length(findex.cc), length(idesk.cc),
                 length(fas.cc), length(mfa.cc),
                 length(dotsrating.cc))

length(findex.cc) <- n.cc
length(idesk.cc) <- n.cc
length(fas.cc) <- n.cc
length(mfa.cc) <- n.cc
length(dotsrating.cc) <- n.cc

cc.total <- cbind(findex.cc, idesk.cc,fas.cc,mfa.cc, dotsrating.cc)

test <- c(findex.cc, fas.cc) %>%
  as.data.frame() %>%
  colnames('name') %>%
  arrange('name')