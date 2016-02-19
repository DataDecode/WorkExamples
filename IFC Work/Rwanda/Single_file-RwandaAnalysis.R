library(dplyr)
library(magrittr)
library(tidyr)
library(data.table)
library(stringr)
library(ggplot2)

library(ICC)

dat1 <- read.csv('NewData/IFCRWA HENEIKEN BRALIRWA_ RawData_Kinyarwanda (before cleaning).csv', 
                 header = TRUE, stringsAsFactors = FALSE) %>%
  as.data.frame() %>%
#selecting just the needed variables  
  select(cooperativename,
         cooperativename_other,
         qcooperative_name_otherdistrict,
         maizeplots.sizeplot_unit_1,
         maizeplots.sizeplot_1,
         maizeplots.sizeplot_meterslength_1,
         maizeplots.sizeplot_meterswidth_1,
         maizeplots.sizeplot_unit_2,
         maizeplots.sizeplot_2,
         maizeplots.sizeplot_meterslength_2,
         maizeplots.sizeplot_meterswidth_2,
         maizeplots.sizeplot_unit_3,
         maizeplots.sizeplot_3,
         maizeplots.sizeplot_meterslength_3,
         maizeplots.sizeplot_meterswidth_3,
         maizeplots.sizeplot_unit_4,
         maizeplots.sizeplot_4,
         maizeplots.sizeplot_meterslength_4,
         maizeplots.sizeplot_meterswidth_4,
         maizeplots.sizeplot_unit_5,
         maizeplots.sizeplot_5,
         maizeplots.sizeplot_meterslength_5,
         maizeplots.sizeplot_meterswidth_5,
         maizeplots.sizeplot_unit_6,
         maizeplots.sizeplot_6,
         maizeplots.sizeplot_meterslength_6,
         maizeplots.sizeplot_meterswidth_6,
         maizeplots.sizeplot_unit_7,
         maizeplots.sizeplot_7,
         maizeplots.sizeplot_meterslength_7,
         maizeplots.sizeplot_meterswidth_7,
         maizeplots.sizeplot_unit_8,
         maizeplots.sizeplot_8,
         maizeplots.sizeplot_meterslength_8,
         maizeplots.sizeplot_meterswidth_8,
         maizeplots.sizeplot_unit_9,
         maizeplots.sizeplot_9,
         maizeplots.sizeplot_meterslength_9,
         maizeplots.sizeplot_meterswidth_9,
         X_426,
         X_426a)


#### Cleaning  Names ####
dat1$qcooperative_name_otherdistrict <-str_trim(dat1$qcooperative_name_otherdistrict)
dat1$qcooperative_name_otherdistrict <- str_to_lower(dat1$qcooperative_name_otherdistrict)

dat1$qcooperative_name_otherdistrict <- ifelse(dat1$qcooperative_name_otherdistrict == "imbereheza nyamirama", "imbere heza nyamirama", 
                         ifelse(dat1$qcooperative_name_otherdistrict == "imbereheza", "imbere heza", dat1$qcooperative_name_otherdistrict))

dat1$qcooperative_name_otherdistrict <- ifelse(dat1$qcooperative_name_otherdistrict == "turwanyubutayu", "turwany ubutayu", 
                         ifelse(dat1$qcooperative_name_otherdistrict == "dufatiyambere mumihigo", "dufate iyambere mu mihigo",
                          ifelse(dat1$qcooperative_name_otherdistrict == "dufatemihigo", "dufate iyambere mumihigo",
                           ifelse(dat1$qcooperative_name_otherdistrict == "dufate iyambere mumihigo.", "dufate iyambere mumihigo", 
                            ifelse(dat1$qcooperative_name_otherdistrict == "dufatiyambere mu mihigo", "dufate iyambere mumihigo",
                             ifelse(dat1$qcooperative_name_otherdistrict == "cooperative abizeranye", "abizeranye", 
                                    dat1$qcooperative_name_otherdistrict))))))

dat1$qcooperative_name_otherdistrict <- ifelse(dat1$qcooperative_name_otherdistrict == "ubumwe.", "ubumwe", 
                               ifelse(dat1$qcooperative_name_otherdistrict == "ubumbwe", "ubumwe", dat1$qcooperative_name_otherdistrict))

dat1$cooperativename <- ifelse(dat1$cooperativename == '', dat1$qcooperative_name_otherdistrict, dat1$cooperativename)

#Need to run this 3 times since it only replaces the first instance in each observation
dat1$cooperativename <- sub('_', ' ', dat1$cooperativename, ignore.case =FALSE, fixed=FALSE)
dat1$cooperativename <- sub('_', ' ', dat1$cooperativename, ignore.case =FALSE, fixed=FALSE)
dat1$cooperativename <- sub('_', ' ', dat1$cooperativename, ignore.case =FALSE, fixed=FALSE)

dat1$cooperativename <- sub('mumihigo', 'mu mihigo', dat1$cooperativename, ignore.case =FALSE, fixed=FALSE)
dat1$cooperativename <- sub('duterimbereshyirinyota', 'duterimbere', dat1$cooperativename, ignore.case =FALSE, fixed=FALSE)
dat1$cooperativename <- sub('duterimbere shyirinyota', 'duterimbere', dat1$cooperativename, ignore.case =FALSE, fixed=FALSE)
dat1$cooperativename <- sub('turwany ubutayu', 'turwanye ubutayu', dat1$cooperativename, ignore.case =FALSE, fixed=FALSE)

#there is one oberservation that does not have any coop information that needs
#to be removed


dat1 <- dat1[-which(dat1$cooperativename == ""), ]


#### Cleaning and Checking Needed Data ####
#hectare plots
plots1 <- subset(dat1, maizeplots.sizeplot_unit_1 == '1')

plots1[is.na(plots1)] <- 0


plots1_wrong <- subset(plots1, maizeplots.sizeplot_1 >= 11) %>%
  #changes first maize plot to hectares
  mutate (maizeplots.sizeplot_1 = (maizeplots.sizeplot_1)/10000) %>%
  #changes second maize plot to hectares
  mutate(maizeplots.sizeplot_2 = (ifelse(maizeplots.sizeplot_unit_2 == 1, maizeplots.sizeplot_2/10000,
    ifelse(maizeplots.sizeplot_unit_2 == 3, ((maizeplots.sizeplot_meterswidth_2 * maizeplots.sizeplot_meterslength_2)/10000),
           maizeplots.sizeplot_2)))) %>%
  #changes thrid maize plot to hectares
  mutate(maizeplots.sizeplot_3 = maizeplots.sizeplot_3/10000)

#I think this one was originally in ares
plots1_wrong[19,5] <- 12/100



plots1_right <- subset(plots1, maizeplots.sizeplot_1 <= 11) %>%
  mutate(maizeplots.sizeplot_2 = (ifelse(maizeplots.sizeplot_unit_2 == 3,  
                                 (maizeplots.sizeplot_meterswidth_2 * maizeplots.sizeplot_meterslength_2)/10000,
                                   ifelse(maizeplots.sizeplot_unit_2 == 2, maizeplots.sizeplot_2/100,
                                    ifelse(maizeplots.sizeplot_unit_2 == 1, maizeplots.sizeplot_2,
                                      maizeplots.sizeplot_2))))) %>%
  mutate(maizeplots.sizeplot_3 = (ifelse(maizeplots.sizeplot_unit_3 == 3,  
                                         (maizeplots.sizeplot_meterswidth_3 * maizeplots.sizeplot_meterslength_3)/10000,
                                         ifelse(maizeplots.sizeplot_unit_3 == 2, maizeplots.sizeplot_3/100,
                                                ifelse(maizeplots.sizeplot_unit_3 == 1, maizeplots.sizeplot_3,
                                                       maizeplots.sizeplot_3))))) %>%
  mutate(maizeplots.sizeplot_4 = (ifelse(maizeplots.sizeplot_unit_4 == 3,  
                                         (maizeplots.sizeplot_meterswidth_4 * maizeplots.sizeplot_meterslength_4)/10000,
                                         ifelse(maizeplots.sizeplot_unit_4 == 2, maizeplots.sizeplot_4/100,
                                                ifelse(maizeplots.sizeplot_unit_4 == 1, maizeplots.sizeplot_4,
                                                       maizeplots.sizeplot_4)))))
  
plots1_right[13,9] <- 1800/10000
plots1_right[57,9] <- 3200/10000

plots1<- rbind(plots1_wrong, plots1_right)


#Are Plots
plots2 <- subset(dat1, maizeplots.sizeplot_unit_1 == '2')
plots2[is.na(plots2)] <- 0

plots2 <- plots2 %>%
  #first plot
  mutate(maizeplots.sizeplot_1 = maizeplots.sizeplot_1/100) %>%
  #second plot
  mutate(maizeplots.sizeplot_2 = (ifelse(maizeplots.sizeplot_unit_2 == 3,  
                                         (maizeplots.sizeplot_meterswidth_2 * maizeplots.sizeplot_meterslength_2)/10000,
                                         ifelse(maizeplots.sizeplot_unit_2 == 2, maizeplots.sizeplot_2/100,
                                                ifelse(maizeplots.sizeplot_unit_2 == 1, maizeplots.sizeplot_2,
                                                       maizeplots.sizeplot_2))))) %>%
  #third plot
  mutate(maizeplots.sizeplot_3 = (ifelse(maizeplots.sizeplot_unit_3 == 1,  
                                         (maizeplots.sizeplot_meterswidth_3 * maizeplots.sizeplot_meterslength_3)/10000,
                                         ifelse(maizeplots.sizeplot_unit_3 == 2, maizeplots.sizeplot_3/100,
                                                       maizeplots.sizeplot_3)))) %>%
  #forth plot
  mutate(maizeplots.sizeplot_4 = (ifelse(maizeplots.sizeplot_unit_4 == 3,  
                                         (maizeplots.sizeplot_meterswidth_4 * maizeplots.sizeplot_meterslength_4)/10000,
                                         ifelse(maizeplots.sizeplot_unit_4 == 2, maizeplots.sizeplot_4/100,
                                                ifelse(maizeplots.sizeplot_unit_4 == 1, maizeplots.sizeplot_4,
                                                       maizeplots.sizeplot_4))))) %>%
  mutate(maizeplots.sizeplot_5 = (ifelse(maizeplots.sizeplot_unit_5 == 3,  
                                         (maizeplots.sizeplot_meterswidth_5 * maizeplots.sizeplot_meterslength_5)/10000,
                                         ifelse(maizeplots.sizeplot_unit_5 == 2, maizeplots.sizeplot_5/100,
                                                ifelse(maizeplots.sizeplot_unit_5 == 1, maizeplots.sizeplot_5,
                                                       maizeplots.sizeplot_5))))) %>%
  mutate(maizeplots.sizeplot_6 = (ifelse(maizeplots.sizeplot_unit_6 == 3,  
                                         (maizeplots.sizeplot_meterswidth_6 * maizeplots.sizeplot_meterslength_6)/10000,
                                         ifelse(maizeplots.sizeplot_unit_6 == 2, maizeplots.sizeplot_6/100,
                                                ifelse(maizeplots.sizeplot_unit_6 == 1, maizeplots.sizeplot_6,
                                                       maizeplots.sizeplot_6))))) %>%
  mutate(maizeplots.sizeplot_7 = (ifelse(maizeplots.sizeplot_unit_7 == 3,  
                                         (maizeplots.sizeplot_meterswidth_7 * maizeplots.sizeplot_meterslength_7)/10000,
                                         ifelse(maizeplots.sizeplot_unit_7 == 2, maizeplots.sizeplot_7/100,
                                                ifelse(maizeplots.sizeplot_unit_7 == 1, maizeplots.sizeplot_7,
                                                       maizeplots.sizeplot_7))))) %>%
  mutate(maizeplots.sizeplot_8 = (ifelse(maizeplots.sizeplot_unit_8 == 3,  
                                         (maizeplots.sizeplot_meterswidth_8 * maizeplots.sizeplot_meterslength_8)/10000,
                                         ifelse(maizeplots.sizeplot_unit_8 == 2, maizeplots.sizeplot_8/100,
                                                ifelse(maizeplots.sizeplot_unit_8 == 1, maizeplots.sizeplot_8,
                                                       maizeplots.sizeplot_8))))) %>%
  mutate(maizeplots.sizeplot_9 = (ifelse(maizeplots.sizeplot_unit_9 == 3,  
                                         (maizeplots.sizeplot_meterswidth_9 * maizeplots.sizeplot_meterslength_9)/10000,
                                         ifelse(maizeplots.sizeplot_unit_9 == 2, maizeplots.sizeplot_9/100,
                                                ifelse(maizeplots.sizeplot_unit_9 == 1, maizeplots.sizeplot_9,
                                                       maizeplots.sizeplot_9)))))

#meter plots
plots3 <- subset(dat1, maizeplots.sizeplot_unit_1 == '3')
plots3[is.na(plots3)] <- 0

plots3 <- plots3 %>%
  mutate(maizeplots.sizeplot_1 = (maizeplots.sizeplot_meterswidth_1 * maizeplots.sizeplot_meterslength_1)/10000) %>%
  #second plot
  mutate(maizeplots.sizeplot_2 = (ifelse(maizeplots.sizeplot_unit_2 == 3,  
                                         (maizeplots.sizeplot_meterswidth_2 * maizeplots.sizeplot_meterslength_2)/10000,
                                         ifelse(maizeplots.sizeplot_unit_2 == 2, maizeplots.sizeplot_2/100,
                                                ifelse(maizeplots.sizeplot_unit_2 == 1, maizeplots.sizeplot_2,
                                                       maizeplots.sizeplot_2))))) %>%
  #third plot
  mutate(maizeplots.sizeplot_3 = (ifelse(maizeplots.sizeplot_unit_3 == 1,  
                                         (maizeplots.sizeplot_meterswidth_3 * maizeplots.sizeplot_meterslength_3)/10000,
                                         ifelse(maizeplots.sizeplot_unit_3 == 2, maizeplots.sizeplot_3/100,
                                                maizeplots.sizeplot_3)))) %>%
  #forth plot
  mutate(maizeplots.sizeplot_4 = (ifelse(maizeplots.sizeplot_unit_4 == 3,  
                                         (maizeplots.sizeplot_meterswidth_4 * maizeplots.sizeplot_meterslength_4)/10000,
                                         ifelse(maizeplots.sizeplot_unit_4 == 2, maizeplots.sizeplot_4/100,
                                                ifelse(maizeplots.sizeplot_unit_4 == 1, maizeplots.sizeplot_4,
                                                       maizeplots.sizeplot_4))))) %>%
  #fifth
  mutate(maizeplots.sizeplot_5 = (ifelse(maizeplots.sizeplot_unit_5 == 3,  
                                         (maizeplots.sizeplot_meterswidth_5 * maizeplots.sizeplot_meterslength_5)/10000,
                                         ifelse(maizeplots.sizeplot_unit_5 == 2, maizeplots.sizeplot_5/100,
                                                ifelse(maizeplots.sizeplot_unit_5 == 1, maizeplots.sizeplot_5,
                                                       maizeplots.sizeplot_5)))))

#other plots
plots97 <- subset(dat1, maizeplots.sizeplot_unit_1 == '97') %>%
  #some of these have no land amount attached to them, so I take those out
  subset(maizeplots.sizeplot_unit_2 == '3')
plots97[is.na(plots97)] <- 0 

plots97 <- plots97 %>%
  mutate(maizeplots.sizeplot_2 = (ifelse(maizeplots.sizeplot_unit_2 == 3,  
                                         (maizeplots.sizeplot_meterswidth_2 * maizeplots.sizeplot_meterslength_2)/10000,
                                         ifelse(maizeplots.sizeplot_unit_2 == 2, maizeplots.sizeplot_2/100,
                                                ifelse(maizeplots.sizeplot_unit_2 == 1, maizeplots.sizeplot_2,
                                                       maizeplots.sizeplot_2)))))
  

plots <- rbind(plots1, plots2, plots3, plots97) %>%
  mutate(hectares = maizeplots.sizeplot_1 + maizeplots.sizeplot_2 +
           maizeplots.sizeplot_3 + maizeplots.sizeplot_4 +
           maizeplots.sizeplot_5 + maizeplots.sizeplot_6 +
           maizeplots.sizeplot_7 + maizeplots.sizeplot_8 +
           maizeplots.sizeplot_9, 
         yield = X_426 + X_426a) %>%
  mutate(productivity = yield/hectares)

plots$productivity[plots$productivity == "Inf"] <- 0

plots <- plots[!(plots$hectares == 0), ]
plots <- plots[!(plots$hectares >= 11), ] 


#### Plot of data ####
p1 <- ggplot(plots, aes(x = hectares, y = yield)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

#### ICC ####
test <- ICCest(cooperativename, productivity, data = plots, alpha = 0.05, CI.type = c("THD"))
                              #THD better for unbalanced data

show <- count (plots, cooperativename) %>% as.data.frame()
