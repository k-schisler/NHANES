# Install package and dataset
install.packages("NHANES")
library(NHANES)
library(ggplot2)  # Loads the ggplot2 library
library(dplyr)  # Loads the dplyr library
library(mosaic) # Loads the mosaic package
View(NHANES)
names(NHANES)

#Make PulsePressure Variable, remove N/A marital Statuses, filter table to NHANES_mod
NHANES_mod <- NHANES %>% 
  select(ID, Gender, Age, AgeDecade, MaritalStatus, Pulse, BPSysAve, BPDiaAve, DirectChol, TotChol) %>%
  filter (!is.na(MaritalStatus)) %>%
  mutate(PulsePressure = BPSysAve - BPDiaAve)
names(NHANES_mod)

#View Fave Stats
favstats(Pulse ~ MaritalStatus, data = NHANES_mod)
favstats(PulsePressure ~ MaritalStatus, data = NHANES_mod)
favstats(BPDiaAve ~ MaritalStatus, data = NHANES_mod)
favstats(BPSysAve ~ MaritalStatus, data = NHANES_mod)
favstats(TotChol ~ MaritalStatus, data = NHANES_mod)
favstats(DirectChol ~ MaritalStatus, data = NHANES_mod)

#Extras Stats
favstats(Age ~ MaritalStatus, data = NHANES_mod)


my.ctable <- table(NHANES_mod$MaritalStatus, NHANES_mod$Gender,dnn=c("Gender", "Marital Status" ))
my.ctable
addmargins(my.ctable)
knitr::kable(addmargins(my.ctable))




#Histograms

##Pulse
ggplot(data = NHANES_mod, aes(x = Pulse)) + 
  geom_histogram(bins=25) + 
  ggtitle("Distribution of Pulse") +
  xlab("Pulse")

##BPDiaAve
ggplot(data = NHANES_mod, aes(x = BPDiaAve)) + 
  geom_histogram(bins=25) + 
  ggtitle("Distribution of Average Diastolic Blood Pressure") +
  xlab("Average Diastolic Blood Pressure")

##BPSysAve
ggplot(data = NHANES_mod, aes(x = BPSysAve)) + 
  geom_histogram(bins=25) + 
  ggtitle("Distribution of Average Systolic Blood Pressure") +
  xlab("Average Systolic Blood Pressure")

##PulsePressure
ggplot(data = NHANES_mod, aes(x = PulsePressure)) + 
  geom_histogram(bins=25) + 
  ggtitle("Distribution of Pulse Pressure") +
  xlab("Average Pulse Pressure")

qqnorm(NHANES_mod$PulsePressure)

##DirectChol
ggplot(data = NHANES_mod, aes(x = DirectChol)) + 
  geom_histogram(bins=25) + 
  ggtitle("Distribution of Direct Cholesterol Level") +
  xlab("Direct Cholesterol Level")

##TotChol
ggplot(data = NHANES_mod, aes(x = TotChol)) + 
  geom_histogram(bins=25) + 
  ggtitle("Distribution of Total Cholesterol Level") +
  xlab("Total Cholesterol Level")





#Boxplots
#example
#ggplot(stroke, aes(x=as.factor(stroke), y=bmi)) +
#  geom_boxplot() +
#  ggtitle("Distribution of BMI by Stroke status") +
#  stat_summary(aes(color="Mean"), fun=mean, geom="point", shape=20, size=5, fill="red") +
#  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +
#  theme(legend.position="right") +
#  scale_fill_brewer(palette="Set1") +
#  xlab("Stroke (1=yes, 0=no)") +
#  ylab("BMI")

##Pulse
NHANES_mod %>%
  filter(!is.na(BPDiaAve)) %>%
  ggplot(aes(x = as.factor(MaritalStatus), y = Pulse)) + 
  geom_boxplot() +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 20, size = 5) +  # Mean point
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +  
  ggtitle("Distribution of Pulse") + 
  ylab("Pulse") + 
  xlab("Marital Status")

##BPDiaAve
NHANES_mod %>%
  filter (!is.na(BPDiaAve)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=BPDiaAve)) + 
  geom_boxplot() +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 20, size = 5) +  # Mean point
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) + 
  ggtitle("Distribution of Average Diastolic Blood Pressure") +
  ylab("Average Diastolic Blood Pressure") + 
  xlab("Marital Status") 

##BPSysAve
NHANES_mod %>%
  filter (!is.na(BPSysAve)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=BPSysAve)) + 
  geom_boxplot() +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 20, size = 5) +  # Mean point
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +
  ggtitle("Distribution of Average Systolic Blood Pressure") +
  ylab("Average Systolic Blood Pressure") + 
  xlab("Marital Status") 

##PulsePressure
NHANES_mod %>%
  filter (!is.na(PulsePressure)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=PulsePressure)) + 
  geom_boxplot() +
  ggtitle("Distribution of Pulse Pressure") +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 20, size = 5) +  # Mean point
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +
  ylab("Pulse Pressure") + 
  xlab("Marital Status") 

##DirectChol
NHANES_mod %>%
  filter (!is.na(DirectChol)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=DirectChol)) + 
  geom_boxplot() +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 20, size = 5) +  # Mean point
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +
  ggtitle("Distribution of Direct Cholesterol") +
  ylab("Direct Choleserol") + 
  xlab("Marital Status") 

##TotChol
NHANES_mod %>%
  filter (!is.na(TotChol)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=TotChol)) + 
  geom_boxplot() +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 20, size = 5) +  # Mean point
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +
  ggtitle("Distribution of Total Cholesterol") +
  ylab("Total Cholesterol") + 
  xlab("Marital Status") 



#EXTRA

#Violin plots
##Pulse
NHANES_mod %>%
  filter (!is.na(BPDiaAve)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=Pulse, fill = Gender)) + 
  geom_violin() +
  ggtitle("Distribution of Pulse ") +
  ylab("Pulse") + 
  xlab("Marital Status") 

##BPDiaAve
NHANES_mod %>%
  filter (!is.na(BPDiaAve)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=BPDiaAve, fill = Gender)) + 
  geom_violin() +
  ggtitle("Distribution of Average Diastolic Blood Pressure") +
  ylab("Average Diastolic Blood Pressure") + 
  xlab("Marital Status") 

##BPSysAve
NHANES_mod %>%
  filter (!is.na(BPSysAve)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=BPSysAve, fill = Gender)) + 
  geom_violin() +
  ggtitle("Distribution of Average Systolic Blood Pressure") +
  ylab("Average Systolic Blood Pressure") + 
  xlab("Marital Status") 

##PulsePressure
NHANES_mod %>%
  filter (!is.na(PulsePressure)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=PulsePressure, fill = Gender)) + 
  geom_violin() +
  ggtitle("Distribution of Pulse Pressure") +
  ylab("Pulse Pressure") + 
  xlab("Marital Status") 

NHANES_mod %>%
  filter (!is.na(PulsePressure)) %>%
  filter (Age < 50) %>% #filter age to remove older individuals, since widows are more likely to be older, Pulse pressure increases with age
  ggplot(aes(x=as.factor(MaritalStatus), y=PulsePressure, fill = Gender)) + 
  geom_violin() +
  ggtitle("Distribution of Pulse Pressure") +
  ylab("Pulse Pressure") + 
  xlab("Marital Status") 

##DirectChol
NHANES_mod %>%
  filter (!is.na(DirectChol)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=DirectChol, fill = Gender)) + 
  geom_violin() +
  ggtitle("Distribution of Direct Cholesterol") +
  ylab("Direct Choleserol") + 
  xlab("Marital Status") 

##TotChol
NHANES_mod %>%
  filter (!is.na(TotChol)) %>%
  ggplot(aes(x=as.factor(MaritalStatus), y=TotChol, fill = Gender)) + 
  geom_violin() +
  ggtitle("Distribution of Total Cholesterol") +
  ylab("Total Cholesterol") + 
  xlab("Marital Status") 