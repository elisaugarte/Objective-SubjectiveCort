###### Poster graphs & Summ statistics #######
###### By: Elisa Ugarte #######
###### Sorry, it's not pretty ######

###Creating an income to needs ratio data set to make everything easier
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

theme_ins <-
  theme_minimal() +
  theme(
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(size = 18, hjust = .5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18), 
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    #panel.background = element_rect(fill = "white")
  )

theme_ins2 <-
  theme_minimal() +
  theme(
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(size = 18, hjust = .5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    #panel.background = element_rect(fill = "white")
  )

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#AE1A1A",
                "#E37721", "#159DF0","#000000")

altPalette0 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1", "skyblue2", "palegreen2", "#FDBF6F", "gray70", "maroon", "orchid1", "darkturquoise", "darkorange4", "brown")

altPalette2 <- c("gold1","maroon", "orchid1", "darkturquoise", "black", "darkorange4","green4", "#6A3D9A", "#FF7F00", "brown","dodgerblue2", "#E31A1C" )

altPalette <- c("gold1","maroon", "orchid1", "dodgerblue2", "black", "darkturquoise", "darkorange4","green4", "#6A3D9A", "#FF7F00", "brown", "#E31A1C" )

finPalette <- c("#009E73", "gold1", "#0072B2","#CC79A7", "#AE1A1A","#E69F00","maroon","#159DF0","#000000", "#F0E442","darkorange4" )

finPalette2 <- c("#009E73","#E69F00","#0072B2","#CC79A7", "gold1","maroon","#159DF0","#000000", "#F0E442","darkorange4" )


# Set directory 
setwd("~/Box Sync/Contextual stressors - Mom Physio/Analysis/Code") 

# Open data set
data <- read.csv("Final_ISDP_72_Mplus_names2022-07-05.csv")
data <- na_if(data, 999) 
length(unique(data$id))

# Variables we need
# Data structure: Age, event, dayt, ocassion, timec
# Data to make wide by day = momcort, childcort, lnmomcort, lnchildcort
# Covariates
# mmed.y, cmed, Mage.y, Medy.y, Cage.y, Csex.y, MC, CC, MW, CW, mstress, covid, pregnant
# Predictors
# itn.y, EconAdj, FinConComp, MatNeedsComp 

# Income-to-Needs ratio: Annual income-to-needs ratios were calculated by dividing the family’s reported i
#ncome by the income value corresponding with the poverty line for a family of that size that year, 
#as indicated by the U. S. Census Bureau 
#Incometo-needs ratios ranged from 0 to 5 with ratios of 1 or less indicating poverty.

# Material Needs: During the past three months
# Difficulty paying bills, money left by end of the month 
# 1 no difficulty/with more money 2 
# Had enough money to afford home, clothing, furniture, car, food, medical care
# 1 Not at all 2 Somewhat 3 Mostly true 4 Very true (Reversed)

# Economic adjustemnts:
# Change food shipping, shut down air, did not go doctor, fell behind, had to ask money, added another job
# Seeked government, sold some possessions, moved to another house
#1 yes 0 no

# Financial Concern & Strain
#In the next three months:
# how often do you expect that you and your family will experience bad times such as poor housing or not having enough food?
# how often do you expect that you will have to do without the basic things that your family needs?
# 1 almost never 2 sometimes 3 a lot of the time 4 almost always

#You have trouble sleeping because of your financial problems.
#You are concerned because you cannot afford adequate health insurance.
#You often worry about your poor financial situation
#Your financial situation is much worse this year than it was in the previous 12 months.
#You do not know how you will be able to support yourself these next 12 months.
# 1 strongly disagree 2 disagree 3 agree 4 strongly agree

### Select variables
library(dplyr)
finaldat <- data %>% select(id:lnchildcort, mmed.y, cmed, Mage.y, Medu.y, Cage.y, Csex.y, MC, CC, MW, 
                            CW, mstress, covid, pregnant, itn.y, EconAdj.y, FinConComp, MatNeedsComp)
head(finaldat)

# Create wide data set
wide <- finaldat %>% group_by(id) %>% summarise_all(mean, na.rm = T)
psych::describe(wide)

wide %>% mutate(covid = as.factor(covid)) %>% 
  select(covid) %>% report_table()

wide %>% mutate(pregnant = as.factor(pregnant)) %>% 
  select(pregnant) %>% report_table()

wide %>% mutate(Csex.y = as.factor(Csex.y)) %>% 
  select(Csex.y) %>% report_table()

wide %>% select(itn.y,EconAdj.y, FinConComp, MatNeedsComp) %>% correlation()
wide %>% select(itn.y,EconAdj.y, FinConComp, MatNeedsComp) %>% report_table

wide %>% filter(EconAdj.y >1) %>% count()
quantile(wide$MatNeedsComp, na.rm = T)
wide %>% filter(MatNeedsComp>1.75) %>% count()
quantile(wide$FinConComp, na.rm = T)

wide %>% filter(MatNeedsComp>2) %>% count() 
2100/55

#37

# GRAPHS

### Income to needs

ggplot(wide, aes(x = "", y = itn.y)) +
  geom_violin(alpha = 0.5, colour = "black", fill = "#009E73") +
  geom_point(position = position_jitter(seed = 1), size = 2, color = "darkgreen") +
  #geom_dotplot(aes(x = "0"), binaxis = "y", binwidth=0.1,stackdir="center") +
  geom_boxplot(width = 0.2, fill = "gold1", alpha = 0.2) +
  geom_hline(aes(yintercept = 1), size = 0.2) + 
  annotate("text", x = 1.5, y = 0.6, label = "Federal Poverty Line", vjust = -0.5, size = 5) +
  geom_hline(aes(yintercept = 1.04),linetype = "dotted", size = 0.8) + 
  annotate("text", x = 1.4, y = 1.35, label = "Sample median", vjust = -0.5, size = 5) +
  #geom_density(aes(x = itn.y), inherit.aes = FALSE) +
  coord_flip() + 
  scale_fill_manual(finPalette2) +
  labs(y = "", x = "") + ggtitle("Income-to-Needs") +
  theme_ins + scale_y_continuous(breaks = seq.int(0,5,0.5))


### Econ adjustments

ggplot(wide, aes(x = "", y = EconAdj.y)) +
  geom_violin(alpha = 0.4, colour = "black", fill = "darkorange1",
              draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_point(position = position_jitter(seed = 1), size = 2, color = "darkorange4") +
  #geom_dotplot(aes(x = "0"), binaxis = "y", binwidth=0.1,stackdir="center") +
  geom_boxplot(width = 0.2, fill = "gold1", alpha = 0.2) +
  #geom_hline(aes(yintercept = 1),linetype = "dotted", size = 1) + 
  #annotate("text", x = 1.5, y = 1.9, label = "Federal Poverty Line", vjust = -0.5, size = 5) +
  #geom_density(aes(x = itn.y), inherit.aes = FALSE) +
  coord_flip() + 
  scale_fill_manual(finPalette2) +
  labs(y = "", x = "") +
  theme_ins +  #+ scale_y_continuous(breaks = seq.int(1,2,1)) +
                             #    labels = c("No adj", "9 adj")) + 
  theme(axis.text.x = element_blank()) + ggtitle("Economic adjustments in the past 3 months") +
  annotate("text", x = 0.45, y = 1.05, label = "1 adjustment", vjust = -1.5, size = 3, angle =-45) +
  annotate("text", x = 0.5, y = 1.15, label = "2 adjustments", vjust = -1.5, size = 3, angle = -45) +
  annotate("text", x = 0.6, y = 1.25, label = "3-4 adjustments", vjust = -1.5, size = 3, angle = -45)+
  annotate("text", x = 0.75, y = 1.4, label = "5 +  adjustments", vjust = -1.5, size = 3, angle = -45)+
  geom_hline(aes(yintercept = 1.11),linetype = "dotted", size = 0.8) + 
  annotate("text", x = 1.45, y = 1.25, label = "Slightly above 1 adjustment", vjust = -0.5, size = 5)

### Material needs
ggplot(wide, aes(x = "", y = MatNeedsComp)) + 
  geom_violin(alpha = 0.5, colour = "black", fill = "dodgerblue2")+ 
  geom_boxplot(width = 0.2, fill = "gold1", alpha = 0.2) + 
  geom_point(position = position_jitter(seed = 1), size = 2, color = "darkblue")+
  coord_flip() + 
  geom_hline(aes(yintercept = 1.75),linetype = "dotted", size = 0.8) +
  annotate("text", x = 1.3, y = 1.8, 
           label = "Some difficulty in \naffording material needs", 
           vjust = -0.5, size = 5, hjust = 0) +
  labs(y = "", x = "") +
  theme_ins + ggtitle("Unmet material needs")

### Financial concern
ggplot(wide, aes(x = "", y = FinConComp)) + 
  geom_violin(alpha = 0.4, colour = "black", fill = "gold1")+ 
  geom_boxplot(width = 0.2, fill = "darkorange4", alpha = 0.3) + 
  geom_point(position = position_jitter(seed = 1), size = 2, color = "darkorange4")+
  coord_flip() + 
  geom_hline(aes(yintercept = 1.43),linetype = "dotted", size = 0.8) +
  annotate("text", x = 1.35, y = 1.45, 
           label = "Feel slightly concerned or\nstrained with their finances", 
           vjust = -0.5, size = 5, hjust = 0) +
  labs(y = "", x = "") +
  theme_ins + ggtitle("Financial Concern & Strain")


######################################
### PLOTTING ESTIMATED & RAW CORTISOL 
######################################

mplusraw <- read.csv("rawcortresults.csv", header = F, sep = "",na.strings = "*")
colnames(mplusraw) <- c("Mmed","Mstress","Pregnant","Cage","Covid","Cmed","ItN",
                     "MatNeeds","EcondAdj","FinCon","momcort", "childcort", "Time", "MomSlope",
                     "MomSlopeSE", "ChildSlope", "ChildSlopeSE", "MomWake", "MomWakeSE", "ChildWake",
                     "ChildWakeSE", "id")

############################## 
### Winsorizing raw cortisol outliers for graphing purposes
### Model were run with winsorized LN CORT
##############################
win <- mplusraw %>% select(id, Time, momcort, childcort) %>% group_by(id, Time) %>%
  summarise_all(mean, na.rm = T) %>% pivot_wider(names_from=Time, values_from = momcort:childcort)
z_scores <- scale(win[,-c(1)])
z_scores <- as.data.frame(z_scores)
colnames(z_scores) <- str_c( "Z", colnames(z_scores))
datwin <- cbind(win, z_scores)

#Highest within 3 sd mom time 0 = 0.6536667 (id = 1097)
#Highest within 3 sd mom time 1 = 0.40900000 (id = 1066)
#Highest within 3 sd mom time 2 = 0.16500000 (id = 1076)

#Highest within 3 sd child time 0 = 0.7965000 (id = 1015)
#Highest within 3 sd child time 1 = 0.50650000 (id = 1012)
#Highest within 3 sd child time 2 = 0.47700000 (id = 1014)


outliers <- datwin %>% filter(Zmomcort_0 >3 | Zmomcort_1 > 3 | Zmomcort_2 > 3 |
                                Zmomcort_0 < -3 | Zmomcort_1 < -3  |Zmomcort_2< -3 | 
                                Zchildcort_0 > 3 | Zchildcort_1 > 3 | Zchildcort_2 > 3 | 
                                Zchildcort_0 < -3 | Zchildcort_1 < -3 | Zchildcort_2 < -3)
View(outliers)

#### Winsorizing the data
winsorized <- mplusraw %>% group_by(id, Time) %>% summarise_all(mean, na.rm = T) %>%
  mutate(momcortW = momcort) %>% mutate(momcortW = case_when(id == 1015 & Time == 0 ~ 0.6636667,
                                                             id == 1021 & Time == 1 ~ 0.41900000,
                                                             id == 1021 & Time == 3 ~ 0.18500000,
                                                             id == 1070 & Time == 3 ~ 0.17500000,
                                                             TRUE ~ momcortW),
                                        childcortW = childcort, childcortW = case_when(id == 1058 & Time == 0 ~ 0.8065000,
                                                                                       id == 1019 & Time == 1 ~ 0.51650000,
                                                                                       id == 1070 & Time == 2 ~ 0.48700000,
                                                                                       TRUE ~ childcortW)) %>% select(id, Time, momcortW, childcortW)
#####################################
####### Raw graphs with regression line NON & WINSORIZED
#####################################

mplusraw %>% select(id, Time, momcort) %>% group_by(id, Time) %>% 
  summarise_all(mean, na.rm = T) %>%
  ggplot(aes(Time, momcort, group = id)) + geom_line(size=.2, alpha=.3)+
  stat_smooth(aes(group = 1),method = lm, colour = "black")+theme_ins+ggtitle("Raw cort non winsorized")

winsorized %>% select(id, Time, momcortW) %>% 
  ggplot(aes(Time, momcortW, group = id)) + geom_line(size=.2, alpha=.3)+
  stat_smooth(aes(group = 1),method = lm, colour = "black")+theme_ins+ggtitle("Raw cort winsorized")

mplusraw %>% select(id, Time, childcort) %>% group_by(id, Time) %>% 
  summarise_all(mean, na.rm = T) %>%
  ggplot(aes(Time, childcort, group = id)) + geom_line(size=.2, alpha=.3)+
  stat_smooth(aes(group = 1),method = lm, colour = "black")+theme_ins+ggtitle("Raw cort non winsorized")

winsorized %>% 
  ggplot(aes(Time, childcortW, group = id)) + geom_line(size=.2, alpha=.3)+
  stat_smooth(aes(group = 1),method = lm, colour = "black")+theme_ins+ggtitle("Raw cort  winsorized")

#########################################################
## Create estimated curves: Average diurnal decline
#########################################################
est <- mplusraw %>% select(id, Time, MomWake, MomWakeSE, ChildWake, ChildWakeSE,ChildSlope, 
                        ChildSlopeSE,MomSlope, MomSlopeSE) %>% group_by(id, Time) %>% 
  summarise_all(mean, na.rm = T) %>%
  mutate(EstMomSlope = MomSlope*Time+MomWake, EstChSlope = ChildSlope*Time+ChildWake) %>% 
  mutate(MomLL = EstMomSlope - 2*MomSlopeSE, MomUL = EstMomSlope + 2*MomSlopeSE,
         ChildLL = EstChSlope - 2*ChildSlopeSE, ChildUL = EstChSlope + 2*ChildSlopeSE) %>%
  select(id, Time, EstMomSlope,EstChSlope, MomLL, ChildLL, MomUL, ChildUL)

mplusraw <- left_join(mplusraw, est, by= c("id", "Time"))


short <- mplusraw %>% select(Time, id, momcort, childcort,Mstress,FinCon,MatNeeds,ItN) %>% group_by(id, Time) %>%
  summarise_all(mean, na.rm = T) 

short <- left_join(short, winsorized, by= c("id", "Time"))

##############################
### GRAPHING RAW CORT (Again)
#############################

short %>% ggplot(aes(Time, momcortW, group = id)) +
  geom_line(size = 0.5) + theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq.int(0.01, 0.65, 0.2), limits = c(0.01, 0.65)) +
  stat_smooth(method = 'lm', group = 1) +
  scale_x_continuous(breaks = seq.int(0,3,1), limits = c(0,2)) + theme_ins

short %>% ggplot(aes(Time, childcortW, group = id)) +
  geom_line(size = 0.5) + theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq.int(0.01, 0.65, 0.2), limits = c(0.01, 0.65)) +
  stat_smooth(method = 'lm', group = 1) +
  scale_x_continuous(breaks = seq.int(0,3,1), limits = c(0,2)) + theme_ins

########################
### Estimated diurnal decline based on model results  
########################

mplusraw %>% select(Time, EstMomSlope, MomLL, MomUL) %>% group_by(Time) %>% summarise_all(mean) %>%
  ggplot(aes(Time, EstMomSlope)) +
  geom_ribbon(
    aes(ymin = MomLL, ymax = MomUL),
    fill = "grey80",
    alpha = .6
  ) +
  geom_line(size = 2)+
  geom_point(
    data = short, 
    aes(x = Time, y= momcortW, group = id),
    size = .5, 
    alpha = .3
  ) +
  geom_line(
    data = short , 
    aes(x = Time, y = momcortW, group = id),
    size=.2, 
    alpha=.3) + 
  theme_ins2 + theme(panel.grid.minor.x = element_line()) + ggtitle("Average Maternal Diurnal Decline") +
  labs(y = "Raw cortisol", x = "Time of day") + theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq.int(0.01, 0.7, 0.1), limits = c(0.01, 0.7)) + 
  scale_x_continuous(breaks = seq.int(0,3,1), limits = c(0,2)) 

### CHILD
mplusraw %>% select(Time, EstChSlope, ChildLL, ChildUL) %>% group_by(Time) %>% summarise_all(mean) %>%
  ggplot(aes(Time, EstChSlope)) +
  geom_ribbon(
    aes(ymin  = ChildLL, ymax = ChildUL),
    fill = "grey80",
    alpha = .6
  ) +
  geom_line(size = 2)+
  geom_point(
    data = short, 
    aes(x = Time, y= childcortW, group = id),
    size = .5, 
    alpha = .3
  ) +
  geom_line(
    data = short , 
    aes(x = Time, y = childcortW, group = id),
    size=.2, 
    alpha=.3) + 
  theme_ins2 + theme(panel.grid.minor.x = element_line()) + ggtitle("Average Child Diurnal Decline") +
  labs(y = "Raw cortisol", x = "Time of day") + theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq.int(0.01, 0.7, 0.1), limits = c(0.01, 0.7)) + 
  scale_x_continuous(breaks = seq.int(0,3,1), limits = c(0,2))  

###### Model did really good!

#######################
## Estimated mother model (Use model results betas)
#######################

mplusraw <- mplusraw %>% mutate(bMatNeedsW = 0.044, bFinConS = 0.017, bMstress = -0.013) %>% 
  rowwise() %>%
  mutate(EstMomEconSlope1SD = sum(EstMomSlope,bMatNeedsW,bFinConS*Time,bMstress*Time, na.rm = T),
         EstMomEconSlopeMinSD= sum(EstMomSlope, bMatNeedsW*-1, bFinConS*-1*Time,bMstress*-1*Time ,na.rm = T)) 
mplusraw %>%select(EstMomSlope, EstMomEconSlope1SD, EstMomEconSlopeMinSD) %>%report_table()

estg <- mplusraw %>% select(id,Time, EstMomSlope, EstMomEconSlope1SD, EstMomEconSlopeMinSD) %>% 
  group_by(id, Time) %>% summarise_all(mean) %>%
  gather(Type, Cort, EstMomSlope:EstMomEconSlopeMinSD, -id,-Time, factor_key = T) %>%
  group_by(Time, Type) %>% summarise(meancort= mean(Cort)) 
#%>% mutate(Time = as.factor(Time))

short %>%  ggplot(aes(Time, momcortW)) +
  geom_point(
    # data = short, 
    aes(x = Time, y= momcortW, group = id),
    size = .5, 
    alpha = .5
  ) +
  geom_line(
    #data = short , 
    aes(x = Time, y = momcortW, group = id),
    size=.2, 
    alpha=.5) +
  geom_line(
    data = estg,
    aes(Time, meancort, color = Type),
    size =1.2,
    alpha=1) +
  geom_point(
    data = estg,
    aes(Time, meancort, color = Type),
    size = 2)  + theme_ins +
  labs(y = "Cortisol (μg/dl)", x = "Time of day") + ggtitle("Mother's Cortisol Diurnal Rythms\n& Estimated Model Results") +
  scale_x_continuous(breaks = seq.int(0,2,1), limits = c(0,2), labels = paste0(c("Wake", "Evening", "Bedtime"))) +
  scale_y_continuous(breaks = seq.int(0, 0.62, 0.1), limits = c(-0.01, 0.62)) + 
  scale_color_manual(values=c("dodgerblue2", "#FF9933", "#993366"), labels = c("Average", "+1 SD", "-1 SD"), name = "Material Needs\nFinancial Concern\nDaily Stress")+
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12))

#################
### Estimated child (Use model results betas)
#################

mplusraw <- mplusraw %>% mutate(bincneedsW = 0.050, bCMstress = 0.069) %>% 
  rowwise() %>%
  mutate(EstChildEconSlope1SD = sum(EstChSlope,bincneedsW, bCMstress, na.rm = T),
         EstChildEconSlopeMinSD= sum(EstChSlope, bincneedsW*-1, bCMstress*-1,na.rm = T)) 
mplusraw %>%select(EstChSlope, EstChildEconSlope1SD, EstChildEconSlopeMinSD) %>%report_table()

estg <- mplusraw %>% select(id,Time, EstChSlope, EstChildEconSlope1SD, EstChildEconSlopeMinSD) %>% 
  group_by(id, Time) %>% summarise_all(mean) %>%
  gather(Type, Cort, EstChSlope:EstChildEconSlopeMinSD, -id,-Time, factor_key = T) %>%
  group_by(Time, Type) %>% summarise(meancort= mean(Cort)) 


short %>%  ggplot(aes(Time, childcort)) +
  geom_point(
    # data = short, 
    aes(x = Time, y= childcort, group = id),
    size = .5, 
    alpha = .5
  ) +
  geom_line(
    #data = short , 
    aes(x = Time, y = childcort, group = id),
    size=.2, 
    alpha=.5) +
  geom_line(
    data = estg,
    aes(Time, meancort, color = Type),
    size =1) +
  geom_point(
    data = estg,
    aes(Time, meancort, color = Type),
    size = 2)  + theme_ins +
  labs(y = "Cortisol (μg/dl)", x = "Time of day") + ggtitle("Infant's Cortisol Diurnal Rythms\n& Estimated Model Results") +
  scale_x_continuous(breaks = seq.int(0,2,1), limits = c(0,2), labels = paste0(c("Wake", "Evening", "Bedtime"))) +
  scale_y_continuous(breaks = seq.int(0, 0.62, 0.1), limits = c(-0.01, 0.62)) + 
  scale_color_manual(values=c("dodgerblue2", "#FF9933", "#993366"), labels = c("Average", "+1 SD", "-1 SD"), name = "Daily Stress\nLow Income\nto Needs")+
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12))

