###### DATA PREPARATION #######
###### By: Elisa Ugarte #######
###### Sorry, it's not pretty ######

list.of.packages <- c("foreign", "psych", "arsenal", "dplyr", "ggplot2","tidyr", 
                      "haven", "readxl", "tidyverse",
                      "openxlsx", "Hmisc", "janitor", 
                      "sjlabelled", "labelled", "codebook",
                      "report", "data.table", "jcolors", "lessR", "report", "lavaan",
                      "correlation")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Loading packages
lapply(list.of.packages, require, character.only = TRUE)

# Set directory 
setwd("~/Box Sync/Contextual stressors - Mom Physio/Analysis/Code") 

######## Cort data ##############

cort2dat <- read.xlsx("EMA618.xlsx")
names(cort2dat)
length(unique(cort2dat$id))

########## DEMOGRAPHICS ##############

demodat <- read.csv("demo_all_021122.csv")
length(unique(demodat$FID))

######## Dealing with medications
## Find the list of medications
meds <- demodat %>% select(D_MEDR, D_MEDRB)
View(meds)
meds %>%
  mutate(across(D_MEDR = as.character)) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  filter(grepl('[[:alpha:]]', value)) %>%
  count(value) %>% print(n=170)

meds %>% mutate(nmed = case_when(D_MEDR %in% c("N/A", "N9", "No", "None", "-99",
                                                "N/a", "none", "none ", "nothing","0", ""," NA","n/a",
                                                "Na","Nine", "no","No\n\n","no medicamento",
                                                "no medicamento ","no medication","no medicine",
                                                "No use of medication", "Nome", "None ","Nine ") ~0,
                                 is.na(D_MEDR) ~ NA_real_,
                                 TRUE ~ 1)) %>% View()

####### Now do it with the og sample
demodat <- demodat %>% mutate(mmed = case_when(D_MEDR %in% c("N/A", "N9", "No", "None", "-99",
                                                             "N/a", "none", "none ", "nothing","0", ""," NA","n/a",
                                                             "Na","Nine", "no","No\n\n","no medicamento",
                                                             "no medicamento ","no medication","no medicine",
                                                             "No use of medication", "Nome", "None ","Nine ") ~0,
                                               is.na(D_MEDR) ~ NA_real_,
                                               TRUE ~ 1)) 

demodat <- demodat %>% mutate(cmed = case_when(D_MEDRB %in% c("N/A", "N9", "No", "None", "-99",
                                                              "N/a", "none", "none ", "nothing","0", ""," NA","n/a",
                                                              "Na","Nine", "no","No\n\n","no medicamento",
                                                              "no medicamento ","no medication","no medicine",
                                                              "No use of medication", "Nome", "None ","Nine ") ~0,
                                               is.na(D_MEDRB) ~ NA_real_,
                                               TRUE ~ 1)) 

###### Preparing income to needs
demodat$year <- as.numeric(format(as.POSIXct(demodat$RecordedDate, format = "%m/%d/%Y %H:%M"),"%Y")) 
itn <- demodat %>% select(ResponseId, year, D_INC, D_INCSUP) %>% dplyr::rename(income = D_INC, famsize = D_INCSUP)
itn <- na_if(itn, -99)

######## Calculating ItN ###########

### Since income is in categories, I'll select the midpoint within each category
table(itn$income,itn$famsize)
itn$incomec <- itn$income
head(itn)
itn$incomec <- NA
itn$incomec[itn$income==1] <- "2500"
itn$incomec[itn$income==2] <- "7500"
itn$incomec[itn$income==3] <- "12500"
itn$incomec[itn$income==4] <- "17500"
itn$incomec[itn$income==5] <- "22500"
itn$incomec[itn$income==6] <- "27500"
itn$incomec[itn$income==7] <- "32500"
itn$incomec[itn$income==8] <- "37500"
itn$incomec[itn$income==9] <- "42500"
itn$incomec[itn$income==10] <- "47500"
itn$incomec[itn$income==11] <- "52500"
itn$incomec[itn$income==12] <- "57500"
itn$incomec[itn$income==13] <- "62500"
itn$incomec[itn$income==14] <- "67500"
itn$incomec[itn$income==15] <- "72500"
itn$incomec[itn$income==16] <- "77500"
itn$incomec[itn$income==17] <- "82500"
itn$incomec[itn$income==18] <- "87500"
itn$incomec[itn$income==19] <- "92500"
itn$incomec[itn$income==20] <- "97500"
itn$incomec <- as.numeric(itn$incomec)
psych::describe(itn$incomec)
dim(itn)

### Thresholds FPL
#Retrieved from: https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
#Created the data set

thresholds <- read.xlsx("FLPThresholds.xlsx", colNames = TRUE)
itn <- itn %>% mutate(year = year+2000) %>% inner_join(thresholds, by = c("year", "famsize")) %>% mutate(incometh = as.numeric(incometh)) %>% 
  mutate(itn = (incomec/incometh)) %>% select(ResponseId,itn)
dim(itn)
itn %>% select(itn) %>% report_table()
hist(itn$itn)

#Now merge back

demodat <- merge(demodat, itn, by = c("ResponseId"), all.x = T)
head(demodat)
dim(demodat)

### Pregnancy
demodat <- demodat %>%
  dplyr::rename(pregnant = D_PREGN)

# Economic hardship
demodat <- na_if(demodat, 998)
demodat <- na_if(demodat, 997)
demodat <- na_if(demodat, 999)
demodat <- na_if(demodat, -99)
inv <- demodat %>% select(ResponseId, ECHD_1:ECHD_COV2,ECHD_3R:ECHD_FC)
#Cant make ends meet: 1,2
inv <- inv %>% rowwise() %>% mutate(ECHD_CMEMrs = sum(c(ECHD_1, ECHD_2), na.rm = F)) %>%
  mutate(ECHD_CMEMrm = mean(c(ECHD_1, ECHD_2), na.rm = T))
inv <- na_if(inv, "NaN")
#Unmet needs: RECODED 3-8
inv %>% select(ECHD_3:ECHD_8,ECHD_UMN) %>% print(n=100)
inv <- inv %>%rowwise() %>% mutate_at(vars(4:9), ~case_when(.==1~ 4,
                                                            .==2~3,
                                                            .==3~2,
                                                            .==4~1)) 
inv <- transform(inv, ECHD_UMNrs = rowSums(inv[,c(4:9)], na.rm = F))
inv <- transform(inv, ECHD_UMNrm = rowMeans(inv[,c(4:9)], na.rm = TRUE))
inv %>% select(ECHD_3:ECHD_8,ECHD_UMN,ECHD_UMNrs,ECHD_UMNrm)
inv <- na_if(inv, "NaN")  

#Economic adjustments
inv %>% select(ECHD_9:ECHD_17)
inv <- inv %>%rowwise() %>% mutate_at(vars(10:18), ~case_when(.==1~ 2,
                                                              .==2~1)) 
inv <- transform(inv, ECHD_EArs = rowSums(inv[,c(10:18)], na.rm = F))
inv <- transform(inv, ECHD_EArm = rowMeans(inv[,c(10:18)], na.rm = TRUE))
inv %>% select(ECHD_9:ECHD_17,ECHD_EArs, ECHD_EArm)
inv <- na_if(inv, "NaN")  

#Financial strain
inv <- inv %>% rowwise() %>% mutate(ECHD_ESrs = sum(c(ECHD_18, ECHD_19), na.rm = F)) %>%
  mutate(ECHD_ESrm = mean(c(ECHD_18, ECHD_19), na.rm = T))
inv <- na_if(inv, "NaN")

#Financial concern: 21-26
inv <- transform(inv, ECHD_FCrs = rowSums(inv[,c(24:28)], na.rm = F))
inv <- transform(inv, ECHD_FCrm = rowMeans(inv[,c(24:28)], na.rm = TRUE))
inv <- na_if(inv, "NaN")  
inv %>% select(ECHD_22:ECHD_26,ECHD_FC,ECHD_FCrs,ECHD_FCrm) 
inv %>% psych::describe()

#Composite financial concern and economic strain
inv <- transform(inv, ECHD_FCSrs = rowSums(inv[,c(20,21,24:28)], na.rm = F))
inv <- transform(inv, ECHD_FCSrm = rowMeans(inv[,c(20,21,24:28)], na.rm = TRUE))

#Composite cant make ends meet & unmet needs
inv <- transform(inv, ECHD_MNCrs = rowSums(inv[,c(2:9)], na.rm = F))
inv <- transform(inv, ECHD_MNCrm = rowMeans(inv[,c(2:9)], na.rm = TRUE))

#Alphas
umn <- inv[c(4:9)]
psych::alpha(umn, na.rm = T, n.iter = 1000)
#Alpha of .91 for unmet material needs
umn <- inv[c(10:18)]
psych::alpha(umn, na.rm = T, n.iter = 1000)
#Alpha of .73 for economic adjustments
fc<- inv[c(24:28)]
psych::alpha(fc, na.rm = T, n.iter = 1000, check.keys = T)
#Alpha of .89 for financial concern
fcs<- inv[c(20,21,24:28)]
psych::alpha(fcs, na.rm = T, n.iter = 1000, check.keys = T)
#Alpha of .86 for financial concern and strain composite
mnc<- inv[c(2:9)]
psych::alpha(mnc, na.rm = T, n.iter = 1000, check.keys = T)
#Alpha of .9 for material needs composite

## Now merge back
inv <- inv %>% select(ResponseId, ECHD_CMEMrs:ECHD_MNCrm)
demodat <- merge(demodat, inv, by = "ResponseId", all.x = T)
demodat %>% select(itn, ECHD_CMEM, ECHD_UMN, ECHD_FC,ECHD_CMEMrs,
                ECHD_UMNrs,ECHD_FCrs,ECHD_EArs, ECHD_ESrs) %>% correlation(p_adjust = "none")
demodat %>% select(itn, ECHD_EArm, ECHD_FCSrm, ECHD_MNCrm) %>% correlation(p_adjust = "none")
demodat <- demodat %>%
  dplyr::rename(id = FID)

#Before or after covid?
demodat %>% select(id, RecordedDate, year, Age) %>% filter(year == 20 | 21) %>% View()
demodat$date <- as.numeric(format(as.POSIXct(demodat$RecordedDate, format = "%m/%d/%Y %H:%M"), "%m-%d")) 
head(demodat$date)
demodat$date_1 = as.Date("2020-03-01")
demodat$date_v = as.Date(demodat$RecordedDate,format="%m/%d/%y")
demodat$diff_dates = difftime(demodat$date_v,demodat$date_1, units = "days")
demodat$diff_dates = as.numeric(demodat$diff_dates)
demodat <- demodat %>% mutate(covid = case_when(diff_dates >= 0 ~1,
                                     diff_dates < 0 ~ 0))

## Selecting demographics
demof <- demodat %>% filter(M_CM == 1) %>% select(id,Age,D_AGEP, D_EDU, D_SEXB,
                                                  Months_Age, mmed, cmed, year, itn, ECHD_CMEMrm, ECHD_UMNrm,ECHD_EArm,ECHD_FCrm,ECHD_FCSrm,ECHD_MNCrm, pregnant,year, covid) %>%
  dplyr::rename(Mage = D_AGEP, Medu = D_EDU, Csex = D_SEXB, Cage = Months_Age,
         EndsMeet = ECHD_CMEMrm, UnmetNeeds = ECHD_UMNrm, FinConcern = ECHD_FCrm,EconAdj = ECHD_EArm,
         FinConComp = ECHD_FCSrm, MatNeedsComp = ECHD_MNCrm) 
demof <- na_if(demof, "NaN")
demof <- demof %>% 
  dplyr::rename(age = Age)

#Now merge with cortisol long
final_long <- merge(cort2dat, demof, by = c("id", "age"), all.x = T)
length(unique(final_long$id))
final_final <- final_long %>% select(id,age, event,dayt, occasion, momcort:lnchildcort,
                              itn.x, itn.y, mmed.x,mmed.y, cmed, Mage.x, Mage.y, Medu.x, Medu.y,
                              Cage.x,Cage.y,Csex.x, Csex.y, EndsMeet.x, EndsMeet.y, UnmetNeeds.x, UnmetNeeds.y,
                              EconAdj.x, EconAdj.y, MC,CC, MW, CW, mstress, sdstress, mc_c, cc_c, mw_c,
                              cw_c, FinConcern, FinConComp, MatNeedsComp,
                              pregnant,covid)

final_final$timec <- final_final$event-1
View(final_final)

write.csv(final_final, paste0("Final_ISDP_72_Mplus_names",Sys.Date(),".csv"), row.names=FALSE, na="999", col.names = T)
write.csv(final_final, paste0("Final_ISDP_72_Mplus",Sys.Date(),".csv"), row.names=FALSE, na="999", col.names = F)

View(final_final)

# Number of samples of cort by participant
final_final %>% group_by(id, age) %>% filter(!is.na(lnmomcort)) %>% 
  count() %>% ungroup(id, age) %>% report_table()
# 6.19 average mom samples
# Number of samples of cort by participant
final_final %>% group_by(id, age) %>% filter(!is.na(lnchildcort)) %>% 
  count() %>% ungroup(id, age) %>% report_table()
# 5.67 average child samples

# All participants have at least one sample
final_final %>% group_by(id) %>% mutate(covid = as.factor(covid)) %>% 
  select(covid) %>% report_table()

wide <- final_final %>% group_by(id) %>% summarise_all(mean, na.rm = T)
psych::describe(wide)
wide %>% mutate(covid = as.factor(covid)) %>% 
  select(covid) %>% report_table()
wide %>% mutate(pregnant = as.factor(pregnant)) %>% 
  select(pregnant) %>% report_table()
wide %>% mutate(Csex.y = as.factor(Csex.y)) %>% 
  select(Csex.y) %>% report_table()
wide %>% select(itn.y,EconAdj.y, FinConComp, MatNeedsComp) %>% correlation()

#Covariates for final model:

final_final %>% select(lnmomcort, mmed.y, Mage.y, Medu.y, Csex.y,Cage.y,
                       mstress, mc_c, mw_c, pregnant,covid) %>% correlation(p_adjust = "none")
# lnmomcort  |     mmed.y |      0.10 | [ 0.00,  0.19] |  2.06 | 436 | 0.040*   
# lnmomcort  |     Mage.y |      0.14 | [ 0.05,  0.23] |  2.99 | 437 | 0.003**  
# lnmomcort  |     Medu.y |      0.02 | [-0.07,  0.11] |  0.43 | 444 | 0.669    
# lnmomcort  |     Csex.y |      0.02 | [-0.07,  0.12] |  0.47 | 444 | 0.637    
# lnmomcort  |     Cage.y |     -0.06 | [-0.15,  0.03] | -1.30 | 444 | 0.196    
# lnmomcort  |    mstress |      0.06 | [-0.03,  0.15] |  1.29 | 444 | 0.199    
# lnmomcort  |       mc_c |     -0.09 | [-0.18,  0.01] | -1.75 | 415 | 0.080    
# lnmomcort  |       mw_c | -5.44e-03 | [-0.10,  0.09] | -0.11 | 432 | 0.910    
# lnmomcort  |   pregnant |     -0.14 | [-0.23, -0.05] | -3.00 | 444 | 0.003**  
# lnmomcort  |      covid |      0.23 | [ 0.14,  0.32] |  4.99 | 444 | < .001***

# Covariances

#mmed.y     |     Mage.y |      0.20 | [ 0.12,  0.29] |  4.50 | 462 | < .001***
#mmed.y     |     Medu.y |      0.17 | [ 0.08,  0.25] |  3.69 | 469 | < .001***
#mmed.y     |     Csex.y |      0.15 | [ 0.06,  0.24] |  3.37 | 469 | < .001***
#mmed.y     |    mstress |      0.23 | [ 0.14,  0.31] |  5.07 | 469 | < .001***
#mmed.y     |   pregnant |     -0.12 | [-0.21, -0.03] | -2.56 | 469 | 0.011*   
#mmed.y     |      covid 
#Mage.y     |      covid 
#mstress    |      covid 
#Mage.y     |     Medu.y |      0.54 | [ 0.47,  0.60] | 13.88 | 471 | < .001***
#Mage.y     |     Csex.y |      0.17 | [ 0.08,  0.25] |  3.71 | 471 | < .001***
#Mage.y     |       mw_c |     -0.16 | [-0.24, -0.07] | -3.37 | 459 | < .001***
#Mage.y     |     Cage.y 
#Medu.y     |     Csex.y |      0.22 | [ 0.13,  0.30] |  4.85 | 478 | < .001***
#Medu.y     |       mw_c |     -0.19 | [-0.28, -0.10] | -4.20 | 466 | < .001***
#Medu.y     |   pregnant |     -0.11 | [-0.20, -0.02] | -2.40 | 478 | 0.017*   
#edu.y     |     Cage.y 
#mstress    |       mw_c |      0.11 | [ 0.02,  0.20] |  2.39 | 466 | 0.017*   
#mstress    |   pregnant |     -0.10 | [-0.18, -0.01] | -2.09 | 478 | 0.037*   
#Cage.y     |    mstress 
#mc_c       |       mw_c |     -0.12 | [-0.21, -0.02] | -2.46 | 449 | 0.014*   
#mc_c       |   pregnant |      0.19 | [ 0.10,  0.28] |  4.10 | 449 | < .001***
#Cage.y     |       mc_c 
#mw_c       |      covid 

#IN MPLUS
#MMED_Y             0.210      0.112      1.869      0.062
#MSTRESS           -0.007      0.004     -1.815      0.070
#PREGNANT          -0.320      0.146     -2.196      0.028
#MAGE_X             0.026      0.013      1.948      0.051
#PREGNANT          -0.362      0.108     -3.361      0.001
# MMED_Y   WITH
#MAGE_X             0.312      0.180      1.736      0.083
#COVID             -0.736      0.167     -4.402      0.000
#

final_final %>% select(lnmomcort, mmed.y, Mage.y,
                       mstress, pregnant, covid, itn.y) %>% correlation(p_adjust = "none")

#mmed.y     |      itn.y 
#Mage.y     |      itn.y
#mstress    |      itn.y


final_final %>% select(lnmomcort, mmed.y,
                       mstress, pregnant, covid,itn.y,MatNeedsComp) %>% correlation(p_adjust = "none")
#mstress    | MatNeedsComp
#itn.y      | MatNeedsComp 

final_final %>% select(lnmomcort, mmed.y, Mage.y,
                       mstress, pregnant,covid, itn.y,MatNeedsComp, EconAdj.y) %>% correlation(p_adjust = "none")
#mstress      |    EconAdj.y
#itn.y        |    EconAdj.y
#MatNeedsComp |    EconAdj.y 
#covid        |    EconAdj.y


final_final %>% select(lnmomcort, mmed.y, Mage.y,
                       mstress, pregnant, covid, itn.y,MatNeedsComp, EconAdj.y, FinConComp) %>% correlation(p_adjust = "none")
#mstress      |    FinConComp
#itn.y        |    FinConComp
#MatNeedsComp |    FinConComp 
#FinConComp |    EconAdj.y 
#Mage.y       |   FinConComp
#pregnant     |   FinConComp 
#covid        |   FinConComp |     0.29 

t <- final_final %>% filter(covid == 1)
length(unique(t$id))

#NOW + CHILD MODEL
final_final %>% select(lnmomcort, lnchildcort, mmed.y, Mage.y, Medu.y, Csex.y,Cage.y,
                       mstress, mc_c, mw_c, pregnant,covid, cmed, cc_c, cw_c) %>% correlation(p_adjust = "none")
#lnmomcort   | lnchildcort 
#lnchildcort |      Csex.y 
#lnchildcort |      Cage.y 
#lnchildcort |     mstress
#lnchildcort |       covid 
#lnchildcort |        cmed 
#mmed.y      |        cmed
#mmed.y      |        cw_c 
#Medu.y      |        cmed
#Medu.y      |        cw_c 
#mstress     |        cmed
#mc_c        |        cmed
#pregnant    |        cmed 
#Mage.y      |        cc_c |   
#Mage.y      |        cw_c 
#Csex.y      |        cc_c 
#Csex.y      |        cw_c 
#Cage.y      |        cc_c | 
#Cage.y      |        cw_c |
#mstress     |        cc_c 
#mc_c        |        cc_c 
#mw_c        |        cw_c 
#pregnant    |        cc_c 
#covid       |        cc_c 
#covid       |        cw_c


final_final %>% select(lnchildcort, cmed, Cage.y,
                       mstress, covid, itn.y) %>% correlation(p_adjust = "none")

final_final %>% select(lnchildcort, cmed, Cage.y,
                       mstress, covid, MatNeedsComp) %>% correlation(p_adjust = "none")

final_final %>% select(lnchildcort, cmed, Cage.y,
                       mstress, covid, itn.y, MatNeedsComp, EconAdj.y) %>% correlation(p_adjust = "none")

final_final %>% select(lnchildcort, cmed, Cage.y,
                       mstress, covid, itn.y, MatNeedsComp, EconAdj.y, FinConComp) %>% correlation(p_adjust = "none")

