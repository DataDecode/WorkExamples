library(dplyr)
library(magrittr)
library(tidyr)
library(data.table)
library(stringr)

dat <- read.csv('cleaned_data_Aug24@1.csv', header = TRUE, stringsAsFactors=F) %>%
  data.table()

dat[ dat == ".a" ] = NA
dat[ dat == "" ] = NA
dat[ dat == ".b" ] = NA

#dat[, (2:928)][ dat == "98"] = "97"
#dat[, (2:928)][ dat == "99"] = "97"

####Basic Household Information####
household <- dat %>%
  select(hhid,
         hhsize,
         qnationality,
         qethnicity,
         language,
         allkidsinschool,
         qnumbermobiles,
         qsimcardnumber,
         qmobileoperator,
         qno_mobile_but_use,
         qreltoheadphone,
         qsmsuser)

household$allkidsinschool[ household$allkidsinschool == "yes" ] = 1
household$allkidsinschool[ household$allkidsinschool == "no" ] = 2
household$allkidsinschool[ household$allkidsinschool == "no kids of that age" ] = 3

household$qmobileoperator_Orange <- ifelse(grepl(1, household$qmobileoperator) == TRUE, 1, 0)
household$qmobileoperator_MTN <- ifelse(grepl(2, household$qmobileoperator) == TRUE, 1, 0)
household$qmobileoperator_Moov <- ifelse(grepl(3, household$qmobileoperator) == TRUE, 1, 0)
household$qmobileoperator_Comium <- ifelse(grepl(4, household$qmobileoperator) == TRUE, 1, 0)
household$qmobileoperator_GreenN <- ifelse(grepl(5, household$qmobileoperator) == TRUE, 1, 0)
household$qmobileoperator_Other <- ifelse(grepl(97, household$qmobileoperator) == TRUE, 1, 0)

#for language, I combined the people who spoke french and another 
#language into the french category for simplification since these
#people can communicate in french
household$language[ household$language == "1 2" ] = 1
household$language[ household$language == "1 5" ] = 1

household <- household %>%
  select(hhid,
         hhsize,
         qnationality,
         qethnicity,
         language,
         allkidsinschool,
         qmobileoperator_Orange,
         qmobileoperator_MTN,
         qmobileoperator_Moov,
         qmobileoperator_Comium,
         qmobileoperator_GreenN,
         qmobileoperator_Other,
         qsimcardnumber,
         qmobileoperator,
         qsmsuser)

#### Head of Household and Family Information ####
hh_info <- dat %>%
  select(hhid,
         sex,
         fam_reltoheadyou,
         fam_residencyyou,
         literate,
         educhead,
         qmaritalstatus,
         qprincipalemployment,
         secondaryemploymenthave,
         qsecondaryemployment,
         fam_ageyou,
         has_mobilephone
  )

hh_info$literate[ hh_info$literate == "Yes" ] = 1
hh_info$literate[ hh_info$literate == "No" ] = 0

hh_info$sex[ hh_info$sex == "male" ] = 1
hh_info$sex[ hh_info$sex == "female" ] = 2

hh_info$secondaryemploymenthave[ hh_info$secondaryemploymenthave == "2" ] = 0

#Family Information #
family_info <- dat %>%
  select(hhid, 
         fam_age1,
         fam_sex1,
         fam_reltohead1,
         fam_residency1,
         fam_educationlevel1,
         fam_age2,
         fam_sex2,
         fam_reltohead2,
         fam_residency2,
         fam_educationlevel2,
         fam_age3,
         fam_sex3,
         fam_reltohead3,
         fam_residency3,
         fam_educationlevel3,
         fam_age4,
         fam_sex4,
         fam_reltohead4,
         fam_residency4,
         fam_educationlevel4,
         fam_age5,
         fam_sex5,
         fam_reltohead5,
         fam_residency5,
         fam_educationlevel5,
         fam_age6,
         fam_sex6,
         fam_reltohead6,
         fam_residency6,
         fam_educationlevel6,
         fam_age7,
         fam_sex7,
         fam_reltohead7,
         fam_residency7,
         fam_educationlevel7,
         fam_age8,
         fam_sex8,
         fam_reltohead8,
         fam_residency8,
         fam_educationlevel8,
         fam_age9,
         fam_sex9,
         fam_reltohead9,
         fam_residency9,
         fam_educationlevel9,
         fam_age10,
         fam_sex10,
         fam_reltohead10,
         fam_residency10,
         fam_educationlevel10,
         fam_age11,
         fam_sex11,
         fam_reltohead11,
         fam_residency11,
         fam_educationlevel11,
         fam_age12,
         fam_sex12,
         fam_reltohead12,
         fam_residency12,
         fam_educationlevel12,
         fam_age13,
         fam_sex13,
         fam_reltohead13,
         fam_residency13,
         fam_educationlevel13,
         fam_age14,
         fam_sex14,
         fam_reltohead14,
         fam_residency14,
         fam_educationlevel14,
         fam_age15,
         fam_sex15,
         fam_reltohead15,
         fam_residency15,
         fam_educationlevel15,
         fam_age16,
         fam_sex16,
         fam_reltohead16,
         fam_residency16,
         fam_educationlevel16,
         fam_age17,
         fam_sex17,
         fam_reltohead17,
         fam_residency17,
         fam_educationlevel17,
         fam_age18,
         fam_sex18,
         fam_reltohead18,
         fam_residency18,
         fam_educationlevel18,
         fam_age19,
         fam_sex19,
         fam_reltohead19,
         fam_residency19,
         fam_educationlevel19,
         fam_age20,
         fam_sex20,
         fam_reltohead20,
         fam_residency20,
         fam_educationlevel20
  )

joint_family <- right_join(hh_info,family_info, 'hhid')

# For clean data, you need to have all the household heads in the same category
head1 <- subset(joint_family, fam_reltoheadyou == 1)
head2 <- subset(joint_family, fam_reltohead1 == 1)
head3 <- subset(joint_family, fam_reltohead2 == 1)
# need to go all the way through to find them all

#### Coop and Cargill Questions ####
coop_cargill <- dat %>%
  select(hhid,
         coop,
         X_91,
         X_92,
         X_93,
         X_94,
         X_95,
         X_96,
         X_97,
         X_915,
         X_916,
         X_916addedb,
         X_917,
         X_918,
         X_919,
         X_920,
         X_922)



coop_cargill$X_922_1 <- ifelse(grepl(1, coop_cargill$X_922) == TRUE, "1", "0")
coop_cargill$X_922_2 <- ifelse(grepl(2, coop_cargill$X_922) == TRUE, "1", "0")
coop_cargill$X_922_3 <- ifelse(grepl(3, coop_cargill$X_922) == TRUE, "1", "0")                            
coop_cargill$X_922_4 <- ifelse(grepl(4, coop_cargill$X_922) == TRUE, "1", "0")
coop_cargill$X_922_5 <- ifelse(grepl(5, coop_cargill$X_922) == TRUE, "1", "0")                            
coop_cargill$X_922_6 <- ifelse(grepl(6, coop_cargill$X_922) == TRUE, "1", "0")
coop_cargill$X_922_7 <- ifelse(grepl(7, coop_cargill$X_922) == TRUE, "1", "0")                            
coop_cargill$X_922_8 <- ifelse(grepl(8, coop_cargill$X_922) == TRUE, "1", "0")
coop_cargill$X_922 <- NULL

#### Feeling about farming ####
feelings <- dat %>% select(hhid,
                           X_86,
                           X_89,
                           X_810,
                           X_812)

#Seperating month values into their own colmns and adding
feelings$X_810_jan <- ifelse(grepl(1, feelings$X_810) == TRUE, 1, 0)
feelings$X_810_feb <- ifelse(grepl(2, feelings$X_810) == TRUE, 1, 0)
feelings$X_810_mar <- ifelse(grepl(3, feelings$X_810) == TRUE, 1, 0)                            
feelings$X_810_apr <- ifelse(grepl(4, feelings$X_810) == TRUE, 1, 0)
feelings$X_810_may <- ifelse(grepl(5, feelings$X_810) == TRUE, 1, 0)                            
feelings$X_810_jun <- ifelse(grepl(6, feelings$X_810) == TRUE, 1, 0)
feelings$X_810_jul <- ifelse(grepl(7, feelings$X_810) == TRUE, 1, 0)                            
feelings$X_810_aug <- ifelse(grepl(8, feelings$X_810) == TRUE, 1, 0)
feelings$X_810_sep <- ifelse(grepl(9, feelings$X_810) == TRUE, 1, 0)                            
feelings$X_810_oct <- ifelse(grepl(10, feelings$X_810) == TRUE, 1, 0)
feelings$X_810_nov <- ifelse(grepl(11, feelings$X_810) == TRUE, 1, 0)                            
feelings$X_810_dec <- ifelse(grepl(12, feelings$X_810) == TRUE, 1, 0)

feelings <- feelings %>% mutate(X_810_total = X_810_jan +
                                 X_810_feb + X_810_mar +
                                 X_810_apr + X_810_may +
                                 X_810_jun + X_810_jul +
                                 X_810_aug + X_810_sep +
                                 X_810_oct + X_810_nov +
                                 X_810_dec) %>%
  select(hhid,
         X_86,
         X_89,
         X_810_jan,
         X_810_feb,
         X_810_mar,
         X_810_apr,
         X_810_may,
         X_810_jun,
         X_810_jul,
         X_810_aug,
         X_810_sep,
         X_810_oct,
         X_810_nov,
         X_810_dec,
         X_810_total,
         X_812)

#### Financial ####
financial <- dat %>% select(hhid,
                            X_101,
                            X_102,
                            X_103,
                            X_104,
                            X_105,
                            X_106,
                            X_107,
                            X_108,
                            X_109,
                            X_1011,
                            X_1012,
                            X_1013,
                            X_1014,
                            X_1015,
                            X_1018,
                            X_1022,
                            X_1024,
                            X_1025,
                            X_1026,
                            X_1027,
                            X_1028,
                            X_1029,
                            X_1033,
                            X_1036,
                            X_1037,
                            X_1038,
                            X_1039,
                            X_1056,
                            X_1057,
                            X_1040,
                            X_1041,
                            X_1055,
                            X_1042,
                            X_1043,
                            X_1044,
                            X_1045,
                            X_1046,
                            X_1048,
                            X_1049,
                            X_1050,
                            X_1051,
                            X_1052,
                            X_1053,
                            X_1054,
                            X_1054_whynot,
                            X_1054_whynotb)

financial$X_102[ financial$X_102 == "1 2" ] = 1
financial$X_102[ financial$X_102 == "2 3" ] = 2
financial$X_102[ financial$X_102 == "2 97" ] = 2
financial$X_102[ financial$X_102 == "1 2 97" ] = 1
financial$X_102[ financial$X_102 == "1 97" ] = 1
financial$X_102[ financial$X_102 == "1 3" ] = 1

financial$X_105[ financial$X_105 == "1 2" ] = 5
financial$X_105[ financial$X_105 == "2 4" ] = 5
financial$X_105[ financial$X_105 == "3 4" ] = 5
financial$X_105[ financial$X_105 == "2 3" ] = 5
financial$X_105[ financial$X_105 == "2 3 4" ] = 5
financial$X_105[ financial$X_105 == "1 4" ] = 5
financial$X_105[ financial$X_105 == "1 2 3 4" ] = 5
financial$X_105[ financial$X_105 == "1 3" ] = 5
financial$X_105[ financial$X_105 == "3 97" ] = 3

financial$X_107[ financial$X_107 == "3 5" ] = 3
financial$X_107[ financial$X_107 == "5 97" ] = 5
financial$X_107[ financial$X_107 == "5 97" ] = 5
financial$X_107[ financial$X_107 == "4 5" ] = 4

financial$X_1025_1 <- ifelse(grepl(1, financial$X_1025) == TRUE, 1, 0)
financial$X_1025_2 <- ifelse(grepl(2, financial$X_1025) == TRUE, 1, 0)
financial$X_1025_3 <- ifelse(grepl(3, financial$X_1025) == TRUE, 1, 0)                            
financial$X_1025_4 <- ifelse(grepl(4, financial$X_1025) == TRUE, 1, 0)
financial$X_1025_5 <- ifelse(grepl(5, financial$X_1025) == TRUE, 1, 0)                            
financial$X_1025_97 <- ifelse(grepl(97, financial$X_1025) == TRUE, 1, 0)
#financial$X_1025 <- NULL

financial$X_1048_1 <- ifelse(grepl(1, financial$X_1048) == TRUE, 1, 0)
financial$X_1048_2 <- ifelse(grepl(2, financial$X_1048) == TRUE, 1, 0)
financial$X_1048_3 <- ifelse(grepl(3, financial$X_1048) == TRUE, 1, 0)                            
financial$X_1048_4 <- ifelse(grepl(4, financial$X_1048) == TRUE, 1, 0)
financial$X_1048_5 <- ifelse(grepl(5, financial$X_1048) == TRUE, 1, 0)
financial$X_1048_6 <- ifelse(grepl(6, financial$X_1048) == TRUE, 1, 0)
financial$X_1048_7 <- ifelse(grepl(7, financial$X_1048) == TRUE, 1, 0)
financial$X_1048_8 <- ifelse(grepl(8, financial$X_1048) == TRUE, 1, 0)                            
financial$X_1048_9 <- ifelse(grepl(9, financial$X_1048) == TRUE, 1, 0)
financial$X_1048_10 <- ifelse(grepl(10, financial$X_1048) == TRUE, 1, 0) 
financial$X_1048_97 <- ifelse(grepl(97, financial$X_1048) == TRUE, 1, 0)
#financial$X_1048 <- NULL

financial$X_1050_1 <- ifelse(grepl(1, financial$X_1050) == TRUE, 1, 0)
financial$X_1050_2 <- ifelse(grepl(2, financial$X_1050) == TRUE, 1, 0)
financial$X_1050_3 <- ifelse(grepl(3, financial$X_1050) == TRUE, 1, 0)                            
financial$X_1050_4 <- ifelse(grepl(4, financial$X_1050) == TRUE, 1, 0)
financial$X_1050_97 <- ifelse(grepl(97, financial$X_1050) == TRUE, 1, 0)
#financial$X_1050 <- NULL

financial <- financial %>%
  select(hhid,
         X_101,
         X_102,
         X_103,
         X_104,
         X_105,
         X_106,
         X_107,
         X_108,
         X_109,
         X_1011,
         X_1012,
         X_1013,
         X_1014,
         X_1015,
         X_1018,
         X_1022,
         X_1024,
         X_1025_1,
         X_1025_2,
         X_1025_3,
         X_1025_4,
         X_1025_5,
         X_1025_97,
         X_1026,
         X_1027,
         X_1028,
         X_1029,
         X_1033,
         X_1036,
         X_1037,
         X_1038,
         X_1039,
         X_1056,
         X_1057,
         X_1040,
         X_1041,
         X_1055,
         X_1042,
         X_1043,
         X_1044,
         X_1045,
         X_1046,
         X_1048,
         X_1048_1,
         X_1048_2,
         X_1048_3,
         X_1048_4,
         X_1048_5,
         X_1048_6,
         X_1048_7,
         X_1048_8,
         X_1048_9,
         X_1048_10,
         X_1048_97,
         X_1049,
         X_1050,
         X_1050_1,
         X_1050_2,
         X_1050_3,
         X_1050_4,
         X_1050_97,
         X_1051,
         X_1052,
         X_1053,
         X_1054,
         X_1054_whynot,
         X_1054_whynotb)

####Cocoa Production####
cocoa_production <- dat %>%
  select(hhid,
         X_21,
          number_plots,
          X_23a,
          X_24a,
          X_25a,
          X_26a,
          X_27a,
          X_28a,
          X_29a,
          X_23b,
          X_24b,
          X_25b,
          X_26b,
          X_27b,
          X_28b,
          X_29b,
          X_23c,
          X_24c,
          X_25c,
          X_26c,
          X_27c,
          X_28c,
          X_29c,
          X_23d,
          X_24d,
          X_25d,
          X_26d,
          X_27d,
          X_28d,
          X_23e,
          X_24e,
          X_25e,
          X_26e,
          X_27e,
          X_28e,
          X_23f,
          X_24f,
          X_25f,
          X_26f,
          X_27f,
          X_28f,
          X_23g,
          X_24g,
          X_25g,
          X_26g,
          X_27g,
          X_28g,
          X_31,
          X_32prior,
          X_32,
          X_32_free,
          X_34,
          everreplanted,
          everreplanted_why,
          X_35,
          X_36,
          X_37,
          X_38,
          X_39,
          X_310,
          X_310added_a,
          X_311,
          yieldbig,
          yieldsma,
          q6_2clean2,
          q6_3clean2,
          q6_4clean2,
          q6_5clean2,
          X_66
  )

cocoa_production$X_31_Ghana <- ifelse(grepl(1, cocoa_production$X_31) == TRUE, 1, 0)
cocoa_production$X_31_Mercedes <- ifelse(grepl(2, cocoa_production$X_31) == TRUE, 1, 0)
cocoa_production$X_31_Francais <- ifelse(grepl(3, cocoa_production$X_31) == TRUE, 1, 0)
cocoa_production$X_31_Other <- ifelse(grepl(97, cocoa_production$X_31) == TRUE, 1, 0)

cocoa_production$everreplanted_why_sun <- ifelse(grepl(1, cocoa_production$X_31) == TRUE, 1, 0)
cocoa_production$everreplanted_why_termites <- ifelse(grepl(2, cocoa_production$X_31) == TRUE, 1, 0)
cocoa_production$everreplanted_why_other <- ifelse(grepl(97, cocoa_production$X_31) == TRUE, 1, 0)

cocoa_production$X_66[ cocoa_production$X_66 == "1 2" ] = 1
cocoa_production$X_66[ cocoa_production$X_66 == "1 2 5" ] = 5
cocoa_production$X_66[ cocoa_production$X_66 == "2 5" ] = 5
cocoa_production$X_66[ cocoa_production$X_66 == "1 5" ] = 5
cocoa_production$X_66[ cocoa_production$X_66 == "2 3" ] = 3

cocoa_production <- cocoa_production %>%
  select(hhid,
         X_21,
         number_plots,
         X_23a,
         X_24a,
         X_25a,
         X_26a,
         X_27a,
         X_28a,
         X_29a,
         X_23b,
         X_24b,
         X_25b,
         X_26b,
         X_27b,
         X_28b,
         X_29b,
         X_23c,
         X_24c,
         X_25c,
         X_26c,
         X_27c,
         X_28c,
         X_29c,
         X_23d,
         X_24d,
         X_25d,
         X_26d,
         X_27d,
         X_28d,
         X_23e,
         X_24e,
         X_25e,
         X_26e,
         X_27e,
         X_28e,
         X_23f,
         X_24f,
         X_25f,
         X_26f,
         X_27f,
         X_28f,
         X_23g,
         X_24g,
         X_25g,
         X_26g,
         X_27g,
         X_28g,
         X_31_Ghana,
         X_31_Mercedes,
         X_31_Francais,
         X_31_Other,
         X_32prior,
         X_32,
         X_32_free,
         X_34,
         everreplanted,
         everreplanted_why_sun,
         everreplanted_why_termites,
         everreplanted_why_other,
         X_35,
         X_36,
         X_37,
         X_38,
         X_39,
         X_310,
         X_310added_a,
         X_311,
         yieldbig,
         yieldsma,
         q6_2clean2,
         q6_3clean2,
         q6_4clean2,
         q6_5clean2,
         X_66
  )

#### Employees ####
employees <- dat %>%
  select(hhid,
         X_41,
         X_42,
         X_44,
         X_45,
         X_46,
         X_43,
         X_47,
         X_48,
         X_423,
         X_425,
         X_426
         )

employees$X_44_1 <- ifelse(grepl(1, employees$X_44) == TRUE, 1, 0)
employees$X_44_2 <- ifelse(grepl(2, employees$X_44) == TRUE, 1, 0)
employees$X_44_3 <- ifelse(grepl(3, employees$X_44) == TRUE, 1, 0)
employees$X_44_4 <- ifelse(grepl(4, employees$X_44) == TRUE, 1, 0)
employees$X_44_5 <- ifelse(grepl(5, employees$X_44) == TRUE, 1, 0)
employees$X_44_6 <- ifelse(grepl(6, employees$X_44) == TRUE, 1, 0)
employees$X_44_7 <- ifelse(grepl(7, employees$X_44) == TRUE, 1, 0)
employees$X_44_8 <- ifelse(grepl(8, employees$X_44) == TRUE, 1, 0)
employees$X_44_9 <- ifelse(grepl(9, employees$X_44) == TRUE, 1, 0)
employees$X_44_10 <- ifelse(grepl(10, employees$X_44) == TRUE, 1, 0)
employees$X_44_12 <- ifelse(grepl(12, employees$X_44) == TRUE, 1, 0)

employees <- employees %>%
  mutate(X_44_familyworkers = X_44_1 +X_44_2 +X_44_3 +
           X_44_4 + X_44_5 + X_44_6 + X_44_7 +
           X_44_8 + X_44_9 + X_44_10 + X_44_12)

employees$X_423_num <- ifelse(grepl("ramassage", employees$X_423) == TRUE, 1, 
                            ifelse(grepl("Ramass", employees$X_423) == TRUE, 1,
                                   ifelse(grepl("RAMASSAGE", employees$X_423) == TRUE, 1,
                                   ifelse(grepl("sherbage", employees$X_423) == TRUE, 1,
                                   ifelse(grepl("ramass", employees$X_423) == TRUE, 1,
                                   ifelse(grepl("usine", employees$X_423) == TRUE, 2, 
                                      ifelse(grepl("ourriture", employees$X_423) == TRUE, 2,
                                        ifelse(grepl("Faire", employees$X_423) == TRUE, 2,
                                          ifelse(grepl("repas", employees$X_423) == TRUE, 2,
                                                 ifelse(grepl("lant", employees$X_423) == TRUE, 3,0))))))))))


# 1- Picking up pods
# 2- cooling food
# 3- planting
#0 - other

employees <- employees %>%
  select(hhid,
         X_41,
         X_42,
         X_44_1,
         X_44_2,
         X_44_3,
         X_44_4,
         X_44_5,
         X_44_6,
         X_44_7,
         X_44_8,
         X_44_9,
         X_44_10,
         X_44_12,
         X_44_familyworkers,
         X_45,
         X_46,
         X_43,
         X_47,
         X_48,
         X_423_num,
         X_425,
         X_426
         )

#### Other farming ####
farming <- dat %>%
  select(hhid,
         X_211,
         X_212,
         X_213,
         X_214,
         X_73,
         X_74
         )

farming$X_211_onlycocao <- ifelse(grepl(1, farming$X_211) == TRUE, 1, 0)
farming$X_211_2 <- ifelse(grepl(2, farming$X_211) == TRUE, 1, 0)
farming$X_211_3 <- ifelse(grepl(3, farming$X_211) == TRUE, 1, 0)
farming$X_211_4 <- ifelse(grepl(4, farming$X_211) == TRUE, 1, 0)
farming$X_211_5 <- ifelse(grepl(5, farming$X_211) == TRUE, 1, 0)
farming$X_211_6 <- ifelse(grepl(6, farming$X_211) == TRUE, 1, 0)
farming$X_211_7 <- ifelse(grepl(7, farming$X_211) == TRUE, 1, 0)
farming$X_211_8 <- ifelse(grepl(8, farming$X_211) == TRUE, 1, 0)
farming$X_211_9 <- ifelse(grepl(9, farming$X_211) == TRUE, 1, 0)
farming$X_211_10 <- ifelse(grepl(10, farming$X_211) == TRUE, 1, 0)
farming$X_211_11 <- ifelse(grepl(11, farming$X_211) == TRUE, 1, 0)
farming$X_211_12 <- ifelse(grepl(12, farming$X_211) == TRUE, 1, 0)
farming$X_211_13 <- ifelse(grepl(13, farming$X_211) == TRUE, 1, 0)
farming$X_211_14 <- ifelse(grepl(14, farming$X_211) == TRUE, 1, 0)
farming$X_211_15 <- ifelse(grepl(15, farming$X_211) == TRUE, 1, 0)
farming$X_211_16 <- ifelse(grepl(16, farming$X_211) == TRUE, 1, 0)
farming$X_211_17 <- ifelse(grepl(17, farming$X_211) == TRUE, 1, 0)
farming$X_211_97 <- ifelse(grepl(97, farming$X_211) == TRUE, 1, 0)
farming<- farming %>%
  mutate(X_211_othercrops = X_211_2 + X_211_3 + X_211_4 +
         X_211_5 + X_211_6 + X_211_7 + X_211_8 + X_211_9 + 
         X_211_10 + X_211_11 + X_211_12 + X_211_13 + X_211_14 +
           X_211_15 + X_211_16 + X_211_17 + X_211_97)

farming$X_74_1 <- ifelse(grepl(1, farming$X_74) == TRUE, 1, 0)
farming$X_74_2 <- ifelse(grepl(2, farming$X_74) == TRUE, 1, 0)
farming$X_74_3 <- ifelse(grepl(3, farming$X_74) == TRUE, 1, 0)
farming$X_74_4 <- ifelse(grepl(4, farming$X_74) == TRUE, 1, 0)
farming$X_74_5 <- ifelse(grepl(5, farming$X_74) == TRUE, 1, 0)
farming$X_74_6 <- ifelse(grepl(6, farming$X_74) == TRUE, 1, 0)
farming$X_74_7 <- ifelse(grepl(7, farming$X_74) == TRUE, 1, 0)
farming$X_74_97 <- ifelse(grepl(97, farming$X_74) == TRUE, 1, 0)

farming <- farming %>%
  select(X_211_onlycocao,
         X_211_2,
         X_211_3,
         X_211_4,
         X_211_5,
         X_211_6,
         X_211_7,
         X_211_8,
         X_211_9,
         X_211_10,
         X_211_11,
         X_211_12,
         X_211_13,
         X_211_14,
         X_211_15,
         X_211_16,
         X_211_17,
         X_211_97,
         X_212,
         X_213,
         X_214,
         X_73,
         X_74_1,
         X_74_2,
         X_74_3,
         X_74_4,
         X_74_5,
         X_74_6,
         X_74_7,
         X_74_97
  )

#### Other Income ####
other_income <- dat %>%
  select(hhid,
         X_71,
         X_72,
         X_77,
         X_78,
         X_79,
         X_710,
         X_711,
         X_712,
         X_713
  )

other_income$X_72[ other_income$X_72 == "4 97" ] = 4
other_income$X_72[ other_income$X_72 == "3 97" ] = 3
other_income$X_72[ other_income$X_72 == "2 97" ] = 2
other_income$X_72[ other_income$X_72 == "2 3" ] = 2
other_income$X_72[ other_income$X_72 == "1 3" ] = 1

#### Selling Coccoa ####
coccoa_sale <- dat %>%
  select(question6_2_clean,
         question6_3_clean,
         question6_4_clean,
         question6_5_clean)

#6_2 and 6_4 are the amount produced
#6_3 and 6_5 is the amount sold to the coop

