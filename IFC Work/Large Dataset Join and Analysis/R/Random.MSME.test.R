# REACH SERVEY DATA
reach<- read.csv('Clean.data/reach.csv')

reach$Country.Code.Year<- paste(reach$Country.Code, reach$Reporting.Year, sep='')

MSME <- reach %>%
  select(Reporting.Year, Institution.Nbr, Outs.SME.Number) %>%
  #We only want institutions taged as MSME in the MSME.Type Category  
  #filter(MSME.Type == 'MSME') %>%
  filter(Reporting.Year >= 2012 , Reporting.Year <= 2014)%>%
  spread(key =Reporting.Year, value = Outs.SME.Number)

test <- reach %>%
  select(Reporting.Year, Institution.Nbr, MSME.2012.or.2014) %>%
  filter(Reporting.Year <= 2014) %>%
  left_join(MSME, "Institution.Nbr")

test[is.na(test)] <- 0

write.csv(test, "Clean.data/test.csv")
length(unique(test$Outs.SME.Number))

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
  mutate(SME.CAGR.Number.2012.2014 = ((`2014` / `2012`)^(1/3)-1)*100) %>%
  select(Institution.Nbr, SME.CAGR.Number.2012.2014)

reach.change.in.SME.CAGR.Number$Institution.Year <- paste(reach.change.in.SME.CAGR.Number$Institution.Nbr,
                                                          '2014',
                                                          sep='') %>%
  as.numeric()

reach.change.in.SME.CAGR.Number$Institution.Nbr <- NULL

length(unique(reach.change.in.SME.CAGR.Number$SME.CAGR.Number.2012.2014))