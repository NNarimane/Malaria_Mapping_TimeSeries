###################################################
################ PTR Figures #####################
##################################################



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
library("sqldf")
library("taRifx")
library("reshape2")
library("malariaAtlas")
library("tools")

# install.packages("extrafont")
library("extrafont")
# font_import()
loadfonts(device = "win")


###############
## Load Data ##
###############

# Load case data
load(file="C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Data/SIVEP_byAll_Detection_byNotification_byType.RData")

# Load shape file
load("C:/Users/nnekkab/Desktop/malariaAtlas/BRA_shp_lvl2_df.RData")
# Rename
BRA_SHP_MU=BRA_shp_lvl2_df
rm(BRA_shp_lvl2_df)

# Load state abbreviations
ADMIN_NAMES=read.csv(file = "C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv", sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)

# Assign names
TS$STATE = ADMIN_NAMES[match(TS$CODE, ADMIN_NAMES$Code),"UF"] 
TS$NAME = ADMIN_NAMES[match(TS$CODE, ADMIN_NAMES$Code),"Name"]


################
## Parameters ##
################

breaks=c(-Inf, 1, 100, 500, 1000, 5000, 10000, 20000, 30000)
labels=factor(c("0", "1-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-20000", "20000-30000"))

# Colors
getColors=colorRampPalette(brewer.pal(6,"Blues"))
Colors=c("gray85", getColors(length(labels)-1))
names(Colors)=labels

# Plot parameters
Level="MU"
Measure="CASES"
title="Notified Plasmodium falciparum and Plasmodium vivax malaria cases, Brazil 2017"
fill_label="Cases"
Year="2017"


#####################################

# Change type labels
TS[which(TS$TYPE == "Vivax"),"TYPE"] = "Reported P. vivax cases"
TS[which(TS$TYPE == "Falciparum"),"TYPE"] = "Reported P. falciparum cases"

# Select data to plot
TS_MU=TS[which(TS$LEVEL == Level),]

if(Measure == "CASES"){
  TS_MU$YEAR=year(TS_MU$DATE)
  TS_MU=subset(TS_MU, select = -c(DATE))
  # Aggregate data
  TS_MU = aggregate(CASES~., TS_MU, FUN = sum)
}else{
  TS_MU$YEAR=year(TS_MU$DATE)
  TS_MU=subset(TS_MU, select = -c(CASES,DATE))
  # Aggregate data
  TS_MU = aggregate(API~., TS_MU, FUN = sum)
}

# Make TS data wide by year and by type
wTS_MU=reshape(TS_MU, idvar = c("LEVEL","CODE","TYPE","STATE","NAME"), 
               timevar = "YEAR", direction = "wide")

# Step 1: make names into character temporarily
BRA_SHP_MU$name_2=as.character(BRA_SHP_MU$name_2)

# Step 2: merge (left_join will help keep in order)
BRA_SHP_MU_SIVEP=left_join(BRA_SHP_MU, wTS_MU, by = c("name_2" = "NAME"))
BRA_SHP_MU_SIVEP$name_2=factor(BRA_SHP_MU_SIVEP$name_2)

# Select year to map
mBRA_SHP_MU_SIVEP=subset(BRA_SHP_MU_SIVEP, select = c(colnames(BRA_SHP_MU_SIVEP)[1:27], paste0(Measure,".",Year)))
names(mBRA_SHP_MU_SIVEP)[names(mBRA_SHP_MU_SIVEP)==paste0(Measure,".",Year)] <- Measure

# Create population categorical variable
mBRA_SHP_MU_SIVEP[is.na(mBRA_SHP_MU_SIVEP$CASES),"CASES"] = 0
mBRA_SHP_MU_SIVEP$CAT <- cut(as.numeric(mBRA_SHP_MU_SIVEP$CASES),
                             breaks = breaks, labels = labels)

# Remove 
rm(BRA_SHP_MU, BRA_SHP_MU_SIVEP)
# Fix NA
Vivax_NA_BRA_SHP_MU_SIVEP=mBRA_SHP_MU_SIVEP[is.na(mBRA_SHP_MU_SIVEP$TYPE),]
Falci_NA_BRA_SHP_MU_SIVEP=mBRA_SHP_MU_SIVEP[is.na(mBRA_SHP_MU_SIVEP$TYPE),]
Vivax_NA_BRA_SHP_MU_SIVEP$TYPE = "Reported P. vivax cases"
Falci_NA_BRA_SHP_MU_SIVEP$TYPE="Reported P. falciparum cases"
Combined_mBRA_SHP_MU_SIVEP=rbind(Vivax_NA_BRA_SHP_MU_SIVEP,
                                 Falci_NA_BRA_SHP_MU_SIVEP,
                                 mBRA_SHP_MU_SIVEP[!is.na(mBRA_SHP_MU_SIVEP$TYPE),])
rm(mBRA_SHP_MU_SIVEP, Falci_NA_BRA_SHP_MU_SIVEP, Vivax_NA_BRA_SHP_MU_SIVEP)
  
# Plot
ggplot(data=Combined_mBRA_SHP_MU_SIVEP) +
  geom_polygon(color=NA, aes(long, lat, group=group, fill=CAT)) +
  scale_fill_manual(values = Colors) +
  facet_wrap(facets= TYPE ~.) +
  coord_equal() +
  theme_minimal() +
  theme_gray() +
  labs(fill = fill_label) +  
  theme(text=element_text(family="Arial", size=18),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_text(face="bold", size = 15),
        legend.text = element_text(size=12),
        title = element_text(face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color="gray80", fill = "gray80"),
        panel.background = element_rect(color="gray80", fill = NA))

# Save
dev.copy(png, "C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/PTR Figure 1v2.png",
         width = 3300, height = 1500, units = "px", pointsize = 1,
         res = 300)
dev.off()

  

###################################################################
## Time Series Plots for BR, Para state, and Anajas municipality ##

# Fix type
TS[which(TS$TYPE == "Reported P. vivax cases"),"TYPE"] = "Vivax"
TS[which(TS$TYPE == "Reported P. falciparum cases"),"TYPE"] = "Falciparum"

# Aggregate to get BR-level
TS_BR=aggregate(CASES~DATE+TYPE, TS, FUN = sum)
TS_BR$LEVEL="BR"
TS_BR$CODE="0"
TS_BR$STATE="NA"
TS_BR$NAME="Brazil"
TS_BR=TS_BR[,names(TS)]

# Get Para state
TS_PARA=TS[which(TS$LEVEL == "UF" & TS$STATE == "PA"),]
TS_PARA$STATE=as.character(TS_PARA$STATE)
TS_PARA$NAME="Pará state"

# Get Anajas municipality
TS_Anajas=TS[which(TS$NAME == "Anajas"),]
TS_Anajas$STATE=as.character(TS_Anajas$STATE)
TS_Anajas$NAME="Anajás municipality in Pará state"

# Merge data
TS_BR_Para_Anajas=rbind(TS_BR, TS_PARA, TS_Anajas)
TS_BR_Para_Anajas$NAME=factor(TS_BR_Para_Anajas$NAME, levels = c("Brazil","Pará state","Anajás municipality in Pará state"))
TS_BR_Para_Anajas$TYPE=factor(TS_BR_Para_Anajas$TYPE, levels = c("Vivax","Falciparum"))


##################
# Plot together ##

# title="Notified Plasmodium falciparum and Plasmodium vivax malaria cases, Brazil 2003-2017"

ggplot(data = TS_BR_Para_Anajas[which(TS_BR_Para_Anajas$DATE < "2017-12-31" &
                                        TS_BR_Para_Anajas$DATE > "2003-01-01"),], 
       aes(DATE, CASES, color = TYPE)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(breaks = "year", 
               date_labels = "%Y") +
  facet_wrap(.~NAME, nrow = 3, scales="free_y") +
  scale_color_manual(values = c("#3182bd","#31a354")) +
  labs(y = "Number of Cases") +
  guides(color=guide_legend(title="Plasmodium species")) +
  # theme_gray() +
  theme_minimal() +
  theme(text=element_text(family="Arial", size=18),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.background = element_rect(fill="gray96", color=NA),
        legend.title = element_text(face="bold", size = 15),
        legend.text = element_text(size = 13),
        legend.position = c(0.855, 0.92),
        strip.background = element_rect(color="gray80", fill = "gray80"),
        panel.background = element_rect(color="gray80", fill = NA),
        title = element_text(face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color="gray93"),
        panel.grid.minor.y = element_line(color="gray93"))

# Save
dev.copy(png, "C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/PTR Figure 2v2.png",
         width = 2700, height = 2700, units = "px", pointsize = 1,
         res = 300)
dev.off()



#####################################################
## Cumulative Plots for Para state by municipality ##


# Aggregate by year
TS_MU_PA=subset(TS, LEVEL == "MU" & STATE == "PA")
TS_MU_PA$YEAR=year(TS_MU_PA$DATE)
TS_MU_PA[which(TS_MU_PA$YEAR == 2002),"YEAR"] = 2003
TS_MU_PA=TS_MU_PA[,-1]
TS_MU_PA_Year=aggregate(CASES~., TS_MU_PA, FUN = sum)
TS_MU_PA_Year_Vivax=subset(TS_MU_PA_Year, TYPE == "Vivax" & CASES > 0)
TS_MU_PA_Year_Falci=subset(TS_MU_PA_Year, TYPE == "Falciparum" & CASES > 0)

# Rank states by year
VIVAX_RANKS_PA = TS_MU_PA_Year_Vivax %>%
  group_by(YEAR) %>%
  mutate(PROP = CASES / sum(CASES))  %>%
  arrange(YEAR, CASES, PROP) %>%
  mutate(RANKS = order(order(PROP, decreasing=TRUE))) %>%
  arrange(YEAR, RANKS) %>%
  mutate(CSUM = cumsum(PROP)) %>%
  arrange(RANKS, YEAR, .by_group = T) 
VIVAX_RANKS_PA=as.data.frame(VIVAX_RANKS_PA)

# Order
VIVAX_RANKS_PA = VIVAX_RANKS_PA[order(VIVAX_RANKS_PA$YEAR, VIVAX_RANKS_PA$RANKS, VIVAX_RANKS_PA$CSUM),]



#########
## Plots

# Colors
getColors=colorRampPalette(brewer.pal(11,"Spectral"))
# getColors=colorRampPalette(c("#FEF4AD","#F6FBB2","#E6F598","#BEE5A0","#94D4A4","#66C2A5","#439BB5","#4075B4"))
Colors_Points=getColors(length(unique(VIVAX_RANKS_PA$YEAR))+1)
names(Colors_Points) = as.character(2003:2017)
Colors_Lines = c(Colors_Points[-1],Colors_Points[1])
names(Colors_Lines) = as.character(2003:2017)

# Plot
ggplot(VIVAX_RANKS_PA, aes(factor(RANKS), CSUM, color = factor(YEAR), fill = factor(YEAR), group = YEAR)) +
  geom_point(size = 2) + 
  geom_path(size = 1.1, alpha = 0.9) +
  scale_fill_manual(values = Colors_Points) +
  scale_color_manual(values = Colors_Lines) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_discrete(breaks=seq(0, 140, 5)) +
  annotate("segment", x=8,xend=-Inf,y=0.8,yend=0.8, color="grey30", size=1.1, alpha=0.9) +
  annotate("segment", x=30,xend=30,y=0.8,yend=-Inf, color="grey30", size=1.1, alpha=0.9) +
  annotate("segment", x=30,xend=8,y=0.8,yend=0.8, color="grey30", size=1.1, alpha=0.9) +
  annotate("segment", x=8,xend=8,y=0.8,yend=-Inf, color="grey30", size=1.1, alpha=0.9) +
  annotate("text", x=34,y=0.79, label=" 80%", color = "grey30", fontface = "bold", size =5) +
  theme_minimal() +
  labs(y = "P. vivax / total annual cases", x = "Municipality Rank", color = "Year", fill = "Year") +
  theme(text=element_text(family="Arial", size=18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.title = element_text(face="bold", size = 16),
        legend.text = element_text(size = 13),
        legend.position = c(0.9, 0.33),
        legend.background = element_rect(fill="gray96", color=NA),
        title = element_text(face="bold"),
        panel.grid.minor.y = element_line(size=0.5),
        plot.subtitle = element_text(face="plain"))

# Save
dev.copy(png, "C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/PTR Figure 3v2.png",
         width = 2500, height = 2500, units = "px", pointsize = 1,
         res = 300)
dev.off()


####################################################
###############################
## Age, gender and pregnancy ##


###########################
## Upload data and clean ##
###########################

# Upload data
load("C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/SIVEP_clean_v1.RData")

# Select data only for 2017
SIVEP_2017=df[which(df$DT_NOTIF > "2017-01-01" & df$DT_NOTIF < "2018-01-01"),]
rm(df, l1)


############
## Gender ##

# Label male and female gender
SIVEP_2017$GENDER=as.character(SIVEP_2017$SEXO)
SIVEP_2017$GENDER=ifelse(SIVEP_2017$GENDER == "F", "Female", "Male")
# Check table
prop.table(table(SIVEP_2017$GENDER, useNA="ifany"))


###############
## Pregnancy ##

### Create pregnancy binary variable
### YES: female and pregnant
### NO: female and not pregnant
### NA: male or < 6 months


# Make the pregnancy variable
SIVEP_2017$PREGNANT=NA
# table(SIVEP_2017$PREGNANT, useNA = "ifany")

# Make missing or unknown pregnancy status NA
# SIVEP_2017[which(SIVEP_2017$GESTANTE_ == "Ignored" | SIVEP_2017$GESTANTE_ == "NA" | is.na(SIVEP_2017$GESTANTE_) | SIVEP_2017$GESTANTE_ == "NO"),"PREGNANT"] = NA
# table(SIVEP_2017$GESTANTE_, useNA = "ifany")

# Label all with 1qtr, 2qtr, 3qtr women over 12 as "Pregnant"
SIVEP_2017[which(SIVEP_2017$GESTANTE_ == "1qtr" & SIVEP_2017$GENDER == "Female" & SIVEP_2017$AGE_CONT >= 10
                 | SIVEP_2017$GESTANTE_ == "2qtr" & SIVEP_2017$GENDER == "Female" & SIVEP_2017$AGE_CONT >= 10
                 | SIVEP_2017$GESTANTE_ == "3qtr" & SIVEP_2017$GENDER == "Female" & SIVEP_2017$AGE_CONT >= 10),"PREGNANT"] = "Pregnant"
# Remove errors
# SIVEP_2017[which(SIVEP_2017$GESTANTE_ == "1qtr" & SIVEP_2017$GENDER == "Female" & SIVEP_2017$AGE_CONT < 10
#                  | SIVEP_2017$GESTANTE_ == "2qtr" & SIVEP_2017$GENDER == "Female" & SIVEP_2017$AGE_CONT < 10
#                  | SIVEP_2017$GESTANTE_ == "3qtr" & SIVEP_2017$GENDER == "Female" & SIVEP_2017$AGE_CONT < 10),"PREGNANT"] = NA
# Check table
prop.table(table(SIVEP_2017$PREGNANT,SIVEP_2017$GENDER, useNA = "ifany"))
table(SIVEP_2017$PREGNANT,SIVEP_2017$GENDER, useNA = "ifany")

# Label all other women "Not pregnant"
SIVEP_2017[which(is.na(SIVEP_2017$PREGNANT) & SIVEP_2017$GENDER == "Female" & SIVEP_2017$AGE_CONT > 10),"PREGNANT"] = "Not pregnant"
# Check table
# prop.table(table(SIVEP_2017$PREGNANT))
# prop.table(table(SIVEP_2017$PREGNANT, useNA = "ifany"))


# UF - level
UF_CQ_PQ_ELIGIBLE_2017=SIVEP_2017 %>%
  group_by(UF_NOTIF, AGE_CAT, GENDER, PREGNANT) %>%
  count(RES_EXAM)%>%
  mutate(LEVEL = "BR",
         CODE = "1") %>%
  select(UF_NOTIF, AGE_CAT, GENDER, PREGNANT, RES_EXAM, n) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "Falciparum") %>%
  rename(FV = "V+F") %>%
  rename(VIVAX = "Vivax") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(UF_NOTIF, AGE_CAT, GENDER, PREGNANT, Vivax) %>%
  gather(key = 'TYPE', value = 'CASES', -c(UF_NOTIF, AGE_CAT, GENDER, PREGNANT)) %>%
  select(UF_NOTIF, AGE_CAT, GENDER, PREGNANT, CASES)


# Add state and municiplity names
ADMIN_NAMES=read.csv(file = "C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv", sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)
# States
# SIVEP_2017$STATE = ADMIN_NAMES[match(SIVEP_2017$UF_NOTIF, ADMIN_NAMES$Code),"UF"] 
# SIVEP_2017$NAME = ADMIN_NAMES[match(SIVEP_2017$UF_NOTIF, ADMIN_NAMES$Code),"Name"]
UF_CQ_PQ_ELIGIBLE_2017$STATE = ADMIN_NAMES[match(UF_CQ_PQ_ELIGIBLE_2017$UF_NOTIF, ADMIN_NAMES$Code),"UF"] 
UF_CQ_PQ_ELIGIBLE_2017$NAME = ADMIN_NAMES[match(UF_CQ_PQ_ELIGIBLE_2017$UF_NOTIF, ADMIN_NAMES$Code),"Name"]

# Select Para
PARA_Age_Gender_Preg=UF_CQ_PQ_ELIGIBLE_2017[which(UF_CQ_PQ_ELIGIBLE_2017$STATE == "PA"),]
# PARA_Age_Gender_Preg[which(PARA_Age_Gender_Preg$PREGNANT=="Not pregnant"),"PREGNANT"]="NA"
# PARA_Age_Gender_Preg$PREGNANT2=PARA_Age_Gender_Preg$PREGNANT
# PARA_Age_Gender_Preg[is.na(PARA_Age_Gender_Preg$PREGNANT2),"PREGNANT2"]="NA"

########
# Plot

PARA_Age_Gender_Preg2=PARA_Age_Gender_Preg
PARA_Age_Gender_Preg2[is.na(PARA_Age_Gender_Preg2$PREGNANT),"PREGNANT"]="NA"
PARA_Age_Gender_Preg2$PREGNANT=factor(PARA_Age_Gender_Preg2$PREGNANT)
PARA_Age_Gender_Preg2$AGE_CAT_NUM=as.integer(PARA_Age_Gender_Preg2$AGE_CAT)

PARA_Age_Gender_Preg2$GROUP=PARA_Age_Gender_Preg2$GENDER
PARA_Age_Gender_Preg2[which(PARA_Age_Gender_Preg2$GROUP=="Female" & PARA_Age_Gender_Preg2$PREGNANT=="Pregnant"),"GROUP"] = "Pregnant female"
PARA_Age_Gender_Preg2[which(PARA_Age_Gender_Preg2$GROUP == "Female"),"GROUP"]="Women"
PARA_Age_Gender_Preg2[which(PARA_Age_Gender_Preg2$GROUP == "Male"),"GROUP"]="Men"
PARA_Age_Gender_Preg2[which(PARA_Age_Gender_Preg2$GROUP == "Pregnant female"),"GROUP"]="Pregnant women"
PARA_Age_Gender_Preg2$GROUP=factor(PARA_Age_Gender_Preg2$GROUP, levels = c("Men","Women","Pregnant women"))

barwidth = 0.535
ggplot() + 
  geom_bar(data = subset(PARA_Age_Gender_Preg2, GENDER == "Female"), 
           mapping = aes(x = AGE_CAT_NUM, y = CASES, fill = as.factor(GROUP)), 
           stat="identity", 
           position='stack', 
           width = barwidth) + 
  geom_bar(data = subset(PARA_Age_Gender_Preg2, GENDER=="Male"), 
           mapping = aes(x = AGE_CAT_NUM + barwidth + 0.0001, y = CASES, fill = as.factor(GROUP)), 
           stat="identity", 
           position='stack' , 
           width = barwidth) + 
  theme_minimal() +
  scale_x_continuous(breaks=PARA_Age_Gender_Preg2$AGE_CAT_NUM+0.25,
                   labels=as.character(PARA_Age_Gender_Preg2$AGE_CAT)) +
  scale_y_continuous(breaks=seq(0,3500,500)) +
  scale_fill_manual(values = c("#3182bd","#22723a","#31a354")) +
  labs(fill  = "Gender and pregnancy", y="Cases",x="Age")  +
  theme(text=element_text(family="Arial", size=18),
      legend.title = element_text(face="bold", size = 25),
      legend.text = element_text(size = 22),
      legend.position = c(0.8, 0.85),
      legend.background = element_rect(fill="gray96", color=NA),
      title = element_text(face="bold"),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.subtitle = element_text(face="plain")) +
  guides(fill=guide_legend(keywidth = 0.35, keyheight = 0.35, default.unit = "inch"))


# Save
dev.copy(png, "C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/PTR Figure 4v2.png",
         width = 3800, height = 2200, units = "px", pointsize = 1,
         res = 300)
dev.off()
