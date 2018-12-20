###########################################################
###########################################################
##########                                       ##########
##########                                       ##########
##########      *** SIVEP DATA ANALYSIS ***      ##########
##########           *** TREATMEENT ***          ##########
##########                                       ##########
##########                                       ##########
###########################################################
###########################################################

########################################
## Set environment and working directory
########################################

# Environment: NN or RL
envNN=TRUE

# Set working directory on your desktop
if(envNN){
  setwd("C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/")
}else{
  setwd("~/")
}


############################
## Install and read packages
############################

# Load packages
library("ggplot2")
library("forecast")
library("tseries")
library("stringr")
library("gridExtra")
library("data.table")
library("fUnitRoots")
library("FitAR")
library("lubridate")
library("parallel")
library("TSA")
library("dplyr")
library("foreach")
library("tstools")
library("ggfortify")
library("RColorBrewer")
library("colorspace")
library("foreign")
library("tidyverse")
library("naniar")

#################
## Load Functions
#################

# Source functions script
source(paste0(getwd(),"/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Functions.R"))


##############
## Data Upload
##############

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Plot folder
Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/Treatment/")

# Data
SIVEP_TREAT=getSIVEP_MALARIA_TYPE_TREATMENT(FilePath)


#########
## COLORS
#########

getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=rev(getColors(length(levels(SIVEP_TREAT$ESQUEMA))))
names(Colors)=levels(SIVEP_TREAT$ESQUEMA)

options(scipen=999)

###########
## ANALYSIS
###########

# Treatment by number of cases
ggplot(data = SIVEP_TREAT, 
       aes(YEAR, CASES, fill = as.factor(ESQUEMA))) +
  scale_fill_manual(values=Colors,
                    labels=names(Colors))  +
  scale_x_continuous(breaks=unique(SIVEP_TREAT$YEAR)) +
  stat_summary(fun.y = sum, geom = "bar") +
  labs(title = "Treatment type by malaria cases, Brazil", x = "Year", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Save
dev.copy(png, paste0(Plot_Folder, "Treatment type by malaria cases.png"),
         width = 1600, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Treatment with PQ by malaria type
ggplot(data = SIVEP_TREAT, 
       aes(YEAR, CASES, fill = PQ)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~TYPE) +
  labs(title = "P.vivax and P.falciparum cases who received primaquine, Brazil", x = "Year", y = "Cases") +
  guides(fill=guide_legend(title="Primaquine")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "P.vivax and P.falciparum cases who received primaquine.png"),
         width = 1600, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Treatment with PQ amount by malaria type
ggplot(data = SIVEP_TREAT, 
       aes(YEAR, CASES, fill = PQ_Amount)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~TYPE) +
  labs(title = "P.vivax and P.falciparum cases who received primaquine by amount, Brazil", x = "Year", y = "Cases") +
  guides(fill=guide_legend(title="Primaquine amount")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "P.vivax and P.falciparum cases who received primaquine by amount.png"),
         width = 1600, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Severe malaria
ggplot(data = SIVEP_TREAT[which(SIVEP_TREAT$ESQUEMA == "Severe Pf"),], 
       aes(AGE_CAT, CASES, fill = TYPE)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~YEAR) +
  labs(title = "Patients treated for severe malaria cases by malaria type, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Plasmodium")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Patients treated for severe malaria cases by malaria type.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

# P.vivax cases with treatment
ggplot(data = subset(SIVEP_TREAT, !is.na(ESQUEMA) & TYPE == "Vivax"), 
       aes(AGE_CAT, CASES, fill = ESQUEMA)) +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~YEAR) +
  labs(title = "Treatment received for P.vivax cases, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Treatment received for P.vivax cases.png"),
         width = 1600, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

# P.falciparum cases with treatment
ggplot(data = subset(SIVEP_TREAT, !is.na(ESQUEMA) & TYPE == "Falciparum"), 
       aes(AGE_CAT, CASES, fill = ESQUEMA)) +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~YEAR) +
  labs(title = "Treatment received for P.falciparum cases, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Treatment received for P.falciparum cases.png"),
         width = 1600, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()


#############################
## TREATMENT AND PREGNANCY ##

# What treatment are pregnant women receiving? by type of malaria
ggplot(data = subset(SIVEP_TREAT, SEXO == "F" & PREGNANT == "Yes"), 
       aes(AGE_CAT, CASES, fill = ESQUEMA)) +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~TYPE) +
  labs(title = "Treatment received by pregnant women for P.vivax and P.falciparum, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Treatment received by pregnant women for P.vivax and P.falciparum.png"),
         width = 1600, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

# What treatment are pregnant women receiving? by year
ggplot(data = subset(SIVEP_TREAT, SEXO == "F" & PREGNANT == "Yes"), 
       aes(AGE_CAT, CASES, fill = ESQUEMA)) +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~YEAR) +
  labs(title = "Treatment received by pregnant women for malaria over time, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Treatment received by pregnant women for malaria over time.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

# What treatment are pregnant women receiving? by gestational age
ggplot(data = subset(SIVEP_TREAT, SEXO == "F" & PREGNANT == "Yes"), 
       aes(AGE_CAT, CASES, fill = ESQUEMA)) +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~GESTANTE) +
  labs(title = "Treatment received by pregnant women for malaria by gestational age, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Treatment received by pregnant women for malaria by gestational age.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Treatment of pregnant women (1-3rd trimester) only
pregnant_women_gestage=subset(SIVEP_TREAT, SEXO == "F" & PREGNANT == "Yes" & GESTANTE != "NA")
Colors_preg=Colors[which(names(Colors) %in% names(which(table(pregnant_women_gestage$ESQUEMA) > 0)))]

# What type of infection
ggplot(data = pregnant_women_gestage, 
       aes(AGE_CAT, CASES, fill = TYPE)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~YEAR) +
  labs(title = "Malaria type in treated pregnant women in 1st, 2nd, and 3rd trimester, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Mostly vivax

# What treatment
ggplot(data = pregnant_women_gestage, 
       aes(AGE_CAT, CASES, fill = ESQUEMA)) +
  scale_fill_manual(values=Colors_preg,
                    labels=names(Colors_preg)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~GESTANTE) +
  labs(title = "Treatment received by pregnant women in 1st, 2nd, and 3rd trimester, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Treatment scheme (* out of use)")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Treatment received by pregnant women in 1st, 2nd, and 3rd trimester.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Number of pregnant women (1-3rd trimester) taking PQ by amount
ggplot(data = pregnant_women_gestage, 
       aes(AGE_CAT, CASES, fill = PQ_Amount)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~YEAR) +
  labs(title = "Amount of primaquine received by pregnant women in 1st, 2nd, and 3rd trimester, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Primaquine")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Amount of primaquine received by pregnant women in 1st, 2nd, and 3rd trimester over time.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Number of pregnant women (1-3rd trimester) taking PQ by amount by gestional age
ggplot(data = pregnant_women_gestage, 
       aes(AGE_CAT, CASES, fill = PQ_Amount)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~GESTANTE) +
  labs(title = "Amount of primaquine received by pregnant women in 1st, 2nd, and 3rd trimester, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Primaquine")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Amount of primaquine received by pregnant women in 1st, 2nd, and 3rd trimester.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Who is taking CQ=3d for pregnant women and babies? with vivax
treatCQ=SIVEP_TREAT[which(SIVEP_TREAT$ESQUEMA=="Pv/Po preg & <6mo & Pm all ages: CQ-3d" & SIVEP_TREAT$TYPE == "Vivax"
                          & SIVEP_TREAT$SEXO!="I"),]
ggplot(data = treatCQ, 
       aes(AGE_CAT, CASES, fill = SEXO)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_wrap(~YEAR) +
  labs(title = "Treatment (Pv/Po preg & <6mo & Pm all ages: CQ-3d) for those with P.vivax, Brazil", x = "Age Groups", y = "Cases") +
  guides(fill=guide_legend(title="Gender")) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Save
dev.copy(png, paste0(Plot_Folder, "Treatment (Pv/Po preg & <6mo & Pm all ages: CQ-3d) for those with P.vivax.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

