###########################################################
###########################################################
##########                                       ##########
##########                                       ##########
##########      *** SIVEP DATA ANALYSIS ***      ##########
##########          *** IMPORTATION ***          ##########
##########                                       ##########
##########                                       ##########
###########################################################
###########################################################

library("data.table")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("igraph")
library("circlize")
library("devtools")
library("chorddiag")
library("RColorBrewer")
library("foreach")
library("colorspace")

###########################################
## Set environment and working directory ##
###########################################

# Environment: NN or RL
envNN=TRUE

# Set working directory on your desktop
if(envNN){
  setwd("C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/")
}else{
  setwd("~/")
}

# Plot folder
Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/Importations/")

# Load state abbreviations
ADMIN_NAMES=read.csv(file = paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv"), sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)



###################
## COUNTRY-LEVEL ##
###################

loadData = T

if(loadData){
  # Load vivax tables by residence
  SIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
  pSIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  pwbSIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  
  # Load vivax tables by infection
  SIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
  pSIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  pwbSIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  
  # Load falciparum tables by residence
  SIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
  pSIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  pwbSIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  
  # Load falciparum tables by infection
  SIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
  pSIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  pwbSIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  
}else{
  
  ############################
  ## Run data upload script ##
  ############################
  
  cat("Run data upload script\n")
  
  # Set file path
  if(envNN){
    FilePath=paste0(getwd(),"/SIVEP_clean.RData")
  }else{
    
  }
  
  # Get SIVEP raw notification data
  load(FilePath)
  
  # Read country codes files
  PAIS_CODE=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_CODES.csv"), header = TRUE, stringsAsFactors = FALSE)
  PAIS_CODE$PAIS_CODE=as.character(PAIS_CODE$PAIS_CODE)
  
  
  ###############
  ## RESIDENCE ##
  
  ###########
  ## VIVAX ##
  
  # Aggregate data by country of residence and malaria type
  SIVEP_PAIS_RES_VIVAX = df %>%
    select(DT_NOTIF, PAIS_RES, RES_EXAM) %>% 
    mutate(YEAR = year(DT_NOTIF)) %>% 
    select(-DT_NOTIF) %>%
    group_by(YEAR, PAIS_RES) %>%
    count(RES_EXAM) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "F") %>%
    rename(FV = "F+V") %>%
    rename(VIVAX = "V") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(YEAR, PAIS_RES, Vivax) %>%
    spread(key = YEAR, value = Vivax)
  
  # Get names
  SIVEP_PAIS_RES_VIVAX[is.na(SIVEP_PAIS_RES_VIVAX)] = 0
  SIVEP_PAIS_RES_VIVAX$PAIS_RES = PAIS_CODE[match(SIVEP_PAIS_RES_VIVAX$PAIS_RES, PAIS_CODE$PAIS_CODE),"PAIS"]
  SIVEP_PAIS_RES_VIVAX=SIVEP_PAIS_RES_VIVAX[complete.cases(SIVEP_PAIS_RES_VIVAX$PAIS_RES),]
  
  # Get prop table with Brazil
  pSIVEP_PAIS_RES_VIVAX=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_RES_VIVAX[,-1]), 2))
  pSIVEP_PAIS_RES_VIVAX=cbind(PAIS_RES = SIVEP_PAIS_RES_VIVAX$PAIS_RES,pSIVEP_PAIS_RES_VIVAX)
  
  # Get prop table without Brazil
  pwbSIVEP_PAIS_RES_VIVAX=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_RES_VIVAX[-1,-1]), 2))
  pwbSIVEP_PAIS_RES_VIVAX=cbind(PAIS_RES = SIVEP_PAIS_RES_VIVAX$PAIS_RES[-1],pwbSIVEP_PAIS_RES_VIVAX)
  
  # Save tables
  write.csv(SIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_COUNTS.csv"), row.names = F)
  write.csv(pSIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
  write.csv(pwbSIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
  
  ################
  ## FALCIPARUM ##
  
  # Aggregate data by country of residence and malaria type
  SIVEP_PAIS_RES_FALCI = df %>%
    select(DT_NOTIF, PAIS_RES, RES_EXAM) %>% 
    mutate(YEAR = year(DT_NOTIF)) %>% 
    select(-DT_NOTIF) %>%
    group_by(YEAR, PAIS_RES) %>%
    count(RES_EXAM) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "F") %>%
    rename(FV = "F+V") %>%
    rename(VIVAX = "V") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(YEAR, PAIS_RES, Falciparum) %>%
    spread(key = YEAR, value = Falciparum)
  
  # Get names
  SIVEP_PAIS_RES_FALCI[is.na(SIVEP_PAIS_RES_FALCI)] = 0
  SIVEP_PAIS_RES_FALCI$PAIS_RES = PAIS_CODE[match(SIVEP_PAIS_RES_FALCI$PAIS_RES, PAIS_CODE$PAIS_CODE),"PAIS"]
  SIVEP_PAIS_RES_FALCI=SIVEP_PAIS_RES_FALCI[complete.cases(SIVEP_PAIS_RES_FALCI$PAIS_RES),]
  
  # Get prop table with Brazil
  pSIVEP_PAIS_RES_FALCI=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_RES_FALCI[,-1]), 2))
  pSIVEP_PAIS_RES_FALCI=cbind(PAIS_RES = SIVEP_PAIS_RES_FALCI$PAIS_RES,pSIVEP_PAIS_RES_FALCI)
  
  # Get prop table without Brazil
  pwbSIVEP_PAIS_RES_FALCI=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_RES_FALCI[-1,-1]), 2))
  pwbSIVEP_PAIS_RES_FALCI=cbind(PAIS_RES = SIVEP_PAIS_RES_FALCI$PAIS_RES[-1],pwbSIVEP_PAIS_RES_FALCI)
  
  # Save tables
  write.csv(SIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_COUNTS.csv"), row.names = F)
  write.csv(pSIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
  write.csv(pwbSIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
  
  
  ###############
  ## INFECTION ##
  
  ###########
  ## VIVAX ##
  
  # Aggregate data by country of residence and malaria type
  SIVEP_PAIS_INF_VIVAX = df %>%
    select(DT_NOTIF, PAIS_INF, RES_EXAM) %>% 
    mutate(YEAR = year(DT_NOTIF)) %>% 
    select(-DT_NOTIF) %>%
    group_by(YEAR, PAIS_INF) %>%
    count(RES_EXAM) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "F") %>%
    rename(FV = "F+V") %>%
    rename(VIVAX = "V") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(YEAR, PAIS_INF, Vivax) %>%
    spread(key = YEAR, value = Vivax)
  
  # Get names
  SIVEP_PAIS_INF_VIVAX[is.na(SIVEP_PAIS_INF_VIVAX)] = 0
  SIVEP_PAIS_INF_VIVAX$PAIS_INF = PAIS_CODE[match(SIVEP_PAIS_INF_VIVAX$PAIS_INF, PAIS_CODE$PAIS_CODE),"PAIS"]
  SIVEP_PAIS_INF_VIVAX=SIVEP_PAIS_INF_VIVAX[complete.cases(SIVEP_PAIS_INF_VIVAX$PAIS_INF),]
  
  # Get prop table with Brazil
  pSIVEP_PAIS_INF_VIVAX=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_INF_VIVAX[,-1]), 2))
  pSIVEP_PAIS_INF_VIVAX=cbind(PAIS_INF = SIVEP_PAIS_INF_VIVAX$PAIS_INF,pSIVEP_PAIS_INF_VIVAX)
  
  # Get prop table without Brazil
  pwbSIVEP_PAIS_INF_VIVAX=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_INF_VIVAX[-1,-1]), 2))
  pwbSIVEP_PAIS_INF_VIVAX=cbind(PAIS_INF = SIVEP_PAIS_INF_VIVAX$PAIS_INF[-1],pwbSIVEP_PAIS_INF_VIVAX)
  
  # Save tables
  write.csv(SIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_COUNTS.csv"), row.names = F)
  write.csv(pSIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
  write.csv(pwbSIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
  
  ################
  ## FALCIPARUM ##
  
  # Aggregate data by country of residence and malaria type
  SIVEP_PAIS_INF_FALCI = df %>%
    select(DT_NOTIF, PAIS_INF, RES_EXAM) %>% 
    mutate(YEAR = year(DT_NOTIF)) %>% 
    select(-DT_NOTIF) %>%
    group_by(YEAR, PAIS_INF) %>%
    count(RES_EXAM) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "F") %>%
    rename(FV = "F+V") %>%
    rename(VIVAX = "V") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(YEAR, PAIS_INF, Falciparum) %>%
    spread(key = YEAR, value = Falciparum)
  
  # Get names
  SIVEP_PAIS_INF_FALCI[is.na(SIVEP_PAIS_INF_FALCI)] = 0
  SIVEP_PAIS_INF_FALCI$PAIS_INF = PAIS_CODE[match(SIVEP_PAIS_INF_FALCI$PAIS_INF, PAIS_CODE$PAIS_CODE),"PAIS"]
  SIVEP_PAIS_INF_FALCI=SIVEP_PAIS_INF_FALCI[complete.cases(SIVEP_PAIS_INF_FALCI$PAIS_INF),]
  
  # Get prop table with Brazil
  pSIVEP_PAIS_INF_FALCI=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_INF_FALCI[,-1]), 2))
  pSIVEP_PAIS_INF_FALCI=cbind(PAIS_INF = SIVEP_PAIS_INF_FALCI$PAIS_INF,pSIVEP_PAIS_INF_FALCI)
  
  # Get prop table without Brazil
  pwbSIVEP_PAIS_INF_FALCI=as.data.frame(prop.table(as.matrix(SIVEP_PAIS_INF_FALCI[-1,-1]), 2))
  pwbSIVEP_PAIS_INF_FALCI=cbind(PAIS_INF = SIVEP_PAIS_INF_FALCI$PAIS_INF[-1],pwbSIVEP_PAIS_INF_FALCI)
  
  # Save tables
  write.csv(SIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_COUNTS.csv"), row.names = F)
  write.csv(pSIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
  write.csv(pwbSIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
  
}

###################
## TOP COUNTRIES ##
###################

# Top resident countries (vivax)
RES_VIVAX=rowSums(SIVEP_PAIS_RES_VIVAX[,2:ncol(SIVEP_PAIS_RES_VIVAX)])
names(RES_VIVAX)=as.character(SIVEP_PAIS_RES_VIVAX$PAIS_RES)
RES_VIVAX=RES_VIVAX[rev(order(RES_VIVAX))]
# Get countries with at least 15 cases
RES_VIVAX=RES_VIVAX[RES_VIVAX > 15]
RES_VIVAX

# Top resident countries (falciparum)
RES_FALCI=rowSums(SIVEP_PAIS_RES_FALCI[,2:ncol(SIVEP_PAIS_RES_FALCI)])
names(RES_FALCI)=as.character(SIVEP_PAIS_RES_FALCI$PAIS_RES)
RES_FALCI=RES_FALCI[rev(order(RES_FALCI))]
# Get countries with at least 15 cases
RES_FALCI=RES_FALCI[1:length(RES_VIVAX)]
RES_FALCI

# Top infection countries (vivax)
INF_VIVAX=rowSums(SIVEP_PAIS_INF_VIVAX[,2:ncol(SIVEP_PAIS_INF_VIVAX)])
names(INF_VIVAX)=as.character(SIVEP_PAIS_INF_VIVAX$PAIS_INF)
INF_VIVAX=INF_VIVAX[rev(order(INF_VIVAX))]
# Get countries with at least 15 cases
INF_VIVAX=INF_VIVAX[INF_VIVAX > 15]
INF_VIVAX

# Top infection countries (falciparum)
INF_FALCI=rowSums(SIVEP_PAIS_INF_FALCI[,2:ncol(SIVEP_PAIS_INF_FALCI)])
names(INF_FALCI)=as.character(SIVEP_PAIS_INF_FALCI$PAIS_INF)
INF_FALCI=INF_FALCI[rev(order(INF_FALCI))]
# Get countries with at least 15 cases
INF_FALCI=INF_FALCI[1:length(INF_VIVAX)]
INF_FALCI

############
## TOP 10 ##
############

# Top 10 countries including other
Top_Countries_BR=names(INF_VIVAX[1:10])
Top_Countries=names(INF_VIVAX[2:10])

# Subset vivax res
OTHER=as.vector(colSums(SIVEP_PAIS_RES_VIVAX[!(SIVEP_PAIS_RES_VIVAX$PAIS_RES %in% Top_Countries_BR),2:ncol(SIVEP_PAIS_RES_VIVAX)]))
OTHER=c(NA, as.integer(OTHER))
names(OTHER)=colnames(SIVEP_PAIS_RES_VIVAX)
sSIVEP_PAIS_RES_VIVAX=rbind(SIVEP_PAIS_RES_VIVAX[which(SIVEP_PAIS_RES_VIVAX$PAIS_RES %in% Top_Countries),],
                            OTHER)
sSIVEP_PAIS_RES_VIVAX$PAIS_RES=as.character(sSIVEP_PAIS_RES_VIVAX$PAIS_RES)
sSIVEP_PAIS_RES_VIVAX[nrow(sSIVEP_PAIS_RES_VIVAX),"PAIS_RES"] = "OTHER"

pwbSIVEP_PAIS_RES_VIVAX=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS_RES_VIVAX[,-1]), 2))
pwbSIVEP_PAIS_RES_VIVAX=cbind(PAIS_RES = sSIVEP_PAIS_RES_VIVAX$PAIS_RES,pwbSIVEP_PAIS_RES_VIVAX)

# Subset falciparum res
OTHER=as.vector(colSums(SIVEP_PAIS_RES_FALCI[!(SIVEP_PAIS_RES_FALCI$PAIS_RES %in% Top_Countries_BR),2:ncol(SIVEP_PAIS_RES_FALCI)]))
OTHER=c(NA, as.integer(OTHER))
names(OTHER)=colnames(SIVEP_PAIS_RES_FALCI)
sSIVEP_PAIS_RES_FALCI=rbind(SIVEP_PAIS_RES_FALCI[which(SIVEP_PAIS_RES_FALCI$PAIS_RES %in% Top_Countries),],
                            OTHER)
sSIVEP_PAIS_RES_FALCI$PAIS_RES=as.character(sSIVEP_PAIS_RES_FALCI$PAIS_RES)
sSIVEP_PAIS_RES_FALCI[nrow(sSIVEP_PAIS_RES_FALCI),"PAIS_RES"] = "OTHER"

pwbSIVEP_PAIS_RES_FALCI=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS_RES_FALCI[,-1]), 2))
pwbSIVEP_PAIS_RES_FALCI=cbind(PAIS_RES = sSIVEP_PAIS_RES_FALCI$PAIS_RES,pwbSIVEP_PAIS_RES_FALCI)

# Subset vivax INF
OTHER=as.vector(colSums(SIVEP_PAIS_INF_VIVAX[!(SIVEP_PAIS_INF_VIVAX$PAIS_INF %in% Top_Countries_BR),2:ncol(SIVEP_PAIS_INF_VIVAX)]))
OTHER=c(NA, as.integer(OTHER))
names(OTHER)=colnames(SIVEP_PAIS_INF_VIVAX)
sSIVEP_PAIS_INF_VIVAX=rbind(SIVEP_PAIS_INF_VIVAX[which(SIVEP_PAIS_INF_VIVAX$PAIS_INF %in% Top_Countries),],
                            OTHER)
sSIVEP_PAIS_INF_VIVAX$PAIS_INF=as.character(sSIVEP_PAIS_INF_VIVAX$PAIS_INF)
sSIVEP_PAIS_INF_VIVAX[nrow(sSIVEP_PAIS_INF_VIVAX),"PAIS_INF"] = "OTHER"

pwbSIVEP_PAIS_INF_VIVAX=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS_INF_VIVAX[,-1]), 2))
pwbSIVEP_PAIS_INF_VIVAX=cbind(PAIS_INF = sSIVEP_PAIS_INF_VIVAX$PAIS_INF,pwbSIVEP_PAIS_INF_VIVAX)

# Subset falciparum INF
OTHER=as.vector(colSums(SIVEP_PAIS_INF_FALCI[!(SIVEP_PAIS_INF_FALCI$PAIS_INF %in% Top_Countries_BR),2:ncol(SIVEP_PAIS_INF_FALCI)]))
OTHER=c(NA, as.integer(OTHER))
names(OTHER)=colnames(SIVEP_PAIS_INF_FALCI)
sSIVEP_PAIS_INF_FALCI=rbind(SIVEP_PAIS_INF_FALCI[which(SIVEP_PAIS_INF_FALCI$PAIS_INF %in% Top_Countries),],
                            OTHER)
sSIVEP_PAIS_INF_FALCI$PAIS_INF=as.character(sSIVEP_PAIS_INF_FALCI$PAIS_INF)
sSIVEP_PAIS_INF_FALCI[nrow(sSIVEP_PAIS_INF_FALCI),"PAIS_INF"] = "OTHER"

pwbSIVEP_PAIS_INF_FALCI=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS_INF_FALCI[,-1]), 2))
pwbSIVEP_PAIS_INF_FALCI=cbind(PAIS_INF = sSIVEP_PAIS_INF_FALCI$PAIS_INF,pwbSIVEP_PAIS_INF_FALCI)

############
## COLORS ##
############

Colors=c(colorRampPalette(c("deeppink4",
                            "darkorchid3",
                            "royalblue",
                            "cyan3",
                            "aquamarine3",
                            "darkolivegreen2",
                            "gold",
                            "darkorange",
                            "indianred2",
                            "firebrick1"))
         (nrow(pwbSIVEP_PAIS_RES_VIVAX)))
names(Colors)=levels(pwbSIVEP_PAIS_RES_VIVAX$PAIS_RES)

getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=rev(getColors(10))
names(Colors)=levels(pwbSIVEP_PAIS_RES_VIVAX$PAIS_RES)

#######################
## STACKED-BOX PLOTS ##


###############
## RESIDENCE ##

###########
## VIVAX ##

# melt the data frame for plotting
mRES_VIVAX <- melt(pwbSIVEP_PAIS_RES_VIVAX, id.vars='PAIS_RES')
levels(mRES_VIVAX$PAIS_RES) = levels(pwbSIVEP_PAIS_RES_VIVAX$PAIS_RES) 
mRES_VIVAX$value=mRES_VIVAX$value*100

# Stacked
mRES_VIVAX_Plot=ggplot(mRES_VIVAX, aes(y=value, x=variable, fill = PAIS_RES)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="Proportion of imported P. vivax cases by case residence country", y="Proportion (%)", x="") +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Country of residence"))

mRES_VIVAX_Plot
# Save
dev.copy(png, paste0(Plot_Folder,"Proportion of imported P. vivax cases by case residence country.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

################
## FALCIPARUM ##

# melt the data frame for plotting
mRES_FALCI <- melt(pwbSIVEP_PAIS_RES_FALCI, id.vars='PAIS_RES')
levels(mRES_FALCI$PAIS_RES) = levels(pwbSIVEP_PAIS_RES_VIVAX$PAIS_RES)
mRES_FALCI$value=mRES_FALCI$value*100

# Stacked
mRES_FALCI_Plot=ggplot(mRES_FALCI, aes(y=value, x=variable, fill = PAIS_RES)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="Proportion of imported P. falciparum cases by case residence country", y="Proportion (%)", x="") +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Country of residence"))

mRES_FALCI_Plot
# Save
dev.copy(png, paste0(Plot_Folder,"Proportion of falciparum P. vivax cases by case residence country.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

###############
## INFECTION ##

###########
## VIVAX ##

# melt the data frame for plotting
mINF_VIVAX <- melt(pwbSIVEP_PAIS_INF_VIVAX, id.vars='PAIS_INF')
levels(mINF_VIVAX$PAIS_INF) = levels(pwbSIVEP_PAIS_RES_VIVAX$PAIS_RES) 
mINF_VIVAX$value=mINF_VIVAX$value*100

# Stacked
mINF_VIVAX_Plot=ggplot(mINF_VIVAX, aes(y=value, x=variable, fill = PAIS_INF)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="Proportion of imported P. vivax cases by probable infection country", y="Proportion (%)", x="")+
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Country of infection"))

mINF_VIVAX_Plot
# Save
dev.copy(png, paste0(Plot_Folder,"Proportion of imported P. vivax cases by probable infection country.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()


################
## FALCIPARUM ##

# melt the data frame for plotting
mINF_FALCI <- melt(pwbSIVEP_PAIS_INF_FALCI, id.vars='PAIS_INF')
levels(mINF_FALCI$PAIS_INF) = levels(pwbSIVEP_PAIS_INF_VIVAX$PAIS_INF) 
mINF_FALCI$value=mINF_FALCI$value*100

# Stacked
mINF_FALCI_Plot=ggplot(mINF_FALCI, aes(y=value, x=variable, fill = PAIS_INF)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="Proportion of imported P. falciparum cases by probable infection country", y="Proportion (%)", x="")+
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Country of infection"))

mINF_FALCI_Plot
# Save
dev.copy(png, paste0(Plot_Folder,"Proportion of imported P. falciparum cases by probable infection country.png"),
         width = 1600, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()


################
## CORD GRAPH ##

###############
## RESIDENCE ##

###########
## VIVAX ##

# Get edgelist by residence
Years=2003:2018
COUNTRY_EDGELIST_RES_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("COUNTRY_EDGELIST_RES_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
         setNames(data.frame(from = SIVEP_PAIS_RES_VIVAX["PAIS_RES"],
                                        to = "BRASIL",
                                        values = SIVEP_PAIS_RES_VIVAX[,as.character(Years[i])]),
                             c("to","from","weight")))
}

COUNTRY_EDGELIST_RES_VIVAX=lapply(COUNTRY_EDGELIST_RES_VIVAX, function(x) x[-1,])

################
## Cord graph ##

# Colors
Colors=c("slategray4", colorRampPalette(c("deeppink4","deeppink3",
                                         "darkorchid4","darkorchid3",
                                         "royalblue","cyan3",
                                         "aquamarine3","springgreen3",
                                         "mediumseagreen","darkolivegreen2",
                                         "gold","darkgoldenrod2",
                                         "darkorange","coral1","indianred2",
                                         "firebrick1","firebrick"))
         (nrow(COUNTRY_EDGELIST_RES_VIVAX[[1]])))
names(Colors)=c("BRASIL", as.character(COUNTRY_EDGELIST_RES_VIVAX[[1]]$to))

# Plot 
Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/Importations/")
plot.new()
# m=matrix(c(1,2,3,4,17,
#                 5,6,7,8,17,
#                 9,10,11,12,17,
#                 13,14,15,16,17),
#               nrow = 4, 
#               ncol = 5,
#               byrow = TRUE)
# layout(mat = m, heights = c(1,1))

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(COUNTRY_EDGELIST_RES_VIVAX[[i]], 
               grid.col = Colors, 
               directional = -1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Residence of Imported P. vivax Cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Residence of Imported P. vivax Cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}
# plot.new()
# legend(title = "Residence of Imported P. vivax Cases",
#        x = "center",inset = 0,
#        legend = names(Colors), bty = "n",
#        col=Colors, lwd=5, cex=1, 
#        ncol=1)


################
## FALCIPARUM ##

# Get edgelist by residence
Years=2003:2018
COUNTRY_EDGELIST_RES_FALCI=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("COUNTRY_EDGELIST_RES_FALCI_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_PAIS_RES_FALCI["PAIS_RES"],
                                      to = "BRASIL",
                                      values = SIVEP_PAIS_RES_FALCI[,as.character(Years[i])]),
                           c("to","from","weight")))
}

COUNTRY_EDGELIST_RES_FALCI=lapply(COUNTRY_EDGELIST_RES_FALCI, function(x) x[-1,])

################
## Cord graph ##

# Colors
Colors=c("slategray4", colorRampPalette(c("deeppink4","deeppink3",
                                          "darkorchid4","darkorchid3",
                                          "royalblue","cyan3",
                                          "aquamarine3","springgreen3",
                                          "mediumseagreen","darkolivegreen2",
                                          "gold","darkgoldenrod2",
                                          "darkorange","coral1","indianred2",
                                          "firebrick1","firebrick"))
         (nrow(COUNTRY_EDGELIST_RES_FALCI[[1]])))
names(Colors)=c("BRASIL", as.character(COUNTRY_EDGELIST_RES_FALCI[[1]]$to))

# Plot 
Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/Importations/")
plot.new()
# m=matrix(c(1,2,3,4,17,
#                 5,6,7,8,17,
#                 9,10,11,12,17,
#                 13,14,15,16,17),
#               nrow = 4, 
#               ncol = 5,
#               byrow = TRUE)
# layout(mat = m, heights = c(1,1))

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(COUNTRY_EDGELIST_RES_FALCI[[i]], 
               grid.col = Colors, 
               directional = -1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Residence of Imported P. falciparum Cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Residence of Imported P. falciparum Cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}
# plot.new()
# legend(title = "Residence of Imported P. vivax Cases",
#        x = "center",inset = 0,
#        legend = names(Colors), bty = "n",
#        col=Colors, lwd=5, cex=1, 
#        ncol=1)

###############
## INFECTION ##

###########
## VIVAX ##

# Get edgelist by residence
Years=2003:2018
COUNTRY_EDGELIST_INF_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("COUNTRY_EDGELIST_INF_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_PAIS_INF_VIVAX["PAIS_INF"],
                                      to = "BRASIL",
                                      values = SIVEP_PAIS_INF_VIVAX[,as.character(Years[i])]),
                           c("to","from","weight")))
}

COUNTRY_EDGELIST_INF_VIVAX=lapply(COUNTRY_EDGELIST_INF_VIVAX, function(x) x[-1,])

################
## Cord graph ##

# Colors
Colors=c("slategray4", colorRampPalette(c("deeppink4","deeppink3",
                                          "darkorchid4","darkorchid3",
                                          "royalblue","cyan3",
                                          "aquamarine3","springgreen3",
                                          "mediumseagreen","darkolivegreen2",
                                          "gold","darkgoldenrod2",
                                          "darkorange","coral1","indianred2",
                                          "firebrick1","firebrick"))
         (nrow(COUNTRY_EDGELIST_INF_VIVAX[[1]])))
names(Colors)=c("BRASIL", as.character(COUNTRY_EDGELIST_INF_VIVAX[[1]]$to))

# Plot 
Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/Importations/")
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(COUNTRY_EDGELIST_INF_VIVAX[[i]], 
               grid.col = Colors, 
               directional = -1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Probable Infection of Imported P. vivax Cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Probable Infection of Imported P. vivax Cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}



################
## FALCIPARUM ##

# Get edgelist by infection
Years=2003:2018
COUNTRY_EDGELIST_INF_FALCI=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("COUNTRY_EDGELIST_INF_FALCI_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_PAIS_INF_FALCI["PAIS_INF"],
                                      to = "BRASIL",
                                      values = SIVEP_PAIS_INF_FALCI[,as.character(Years[i])]),
                           c("to","from","weight")))
}

COUNTRY_EDGELIST_INF_FALCI=lapply(COUNTRY_EDGELIST_INF_FALCI, function(x) x[-1,])

################
## Cord graph ##

# Colors
Colors=c("slategray4", colorRampPalette(c("deeppink4","deeppink3",
                                          "darkorchid4","darkorchid3",
                                          "royalblue","cyan3",
                                          "aquamarine3","springgreen3",
                                          "mediumseagreen","darkolivegreen2",
                                          "gold","darkgoldenrod2",
                                          "darkorange","coral1","indianred2",
                                          "firebrick1","firebrick"))
         (nrow(COUNTRY_EDGELIST_INF_FALCI[[1]])))
names(Colors)=c("BRASIL", as.character(COUNTRY_EDGELIST_INF_FALCI[[1]]$to))

# Plot 
Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/Importations/")
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(COUNTRY_EDGELIST_INF_FALCI[[i]], 
               grid.col = Colors, 
               directional = -1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Probable Infection of Imported P. falciparum Cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Probable Infection of Imported P. falciparum Cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}


###############################################################################


############################
## Run data upload script ##
############################

cat("Run data upload script\n")

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Get SIVEP raw notification data
load(FilePath)


##################
## UF-RES-LEVEL ##
##################


# Aggregate data by country of residence and malaria type
SIVEP_UF_RES_VIVAX = df %>%
  select(DT_NOTIF, UF_NOTIF, UF_RESID, RES_EXAM) %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF) %>%
  group_by(YEAR, UF_NOTIF, UF_RESID) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, UF_NOTIF, UF_RESID, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Remove df
# rm(df)

# Assign names
SIVEP_UF_RES_VIVAX$UF_NOTIF = as.character(ADMIN_NAMES[match(SIVEP_UF_RES_VIVAX$UF_NOTIF, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_RES_VIVAX$UF_RESID = as.character(ADMIN_NAMES[match(SIVEP_UF_RES_VIVAX$UF_RESID, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_RES_VIVAX[is.na(SIVEP_UF_RES_VIVAX)] = 0

unique(SIVEP_UF_RES_VIVAX$UF_NOTIF)
unique(SIVEP_UF_RES_VIVAX$UF_RESID)

# Remove if RES = 0
SIVEP_UF_RES_VIVAX=SIVEP_UF_RES_VIVAX[-which(SIVEP_UF_RES_VIVAX$UF_RESID == "0"),]

# Remove self-links
SIVEP_UF_RES_VIVAX=SIVEP_UF_RES_VIVAX[-which(SIVEP_UF_RES_VIVAX$UF_RESID == SIVEP_UF_RES_VIVAX$UF_NOTIF),]

# Get edgelist by residence
Years=2003:2018
UF_EDGELIST_RES_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("UF_EDGELIST_RES_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_UF_RES_VIVAX$UF_RESID,
                                      to = SIVEP_UF_RES_VIVAX$UF_NOTIF,
                                      values = SIVEP_UF_RES_VIVAX[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_UF_RES_VIVAX$UF_NOTIF)))),rep("gray",length(unique(SIVEP_UF_RES_VIVAX$UF_RESID))-length(unique(SIVEP_UF_RES_VIVAX$UF_NOTIF))))
names(Colors)=c(unique(SIVEP_UF_RES_VIVAX$UF_NOTIF),unique(SIVEP_UF_RES_VIVAX$UF_RESID[!SIVEP_UF_RES_VIVAX$UF_RESID %in% SIVEP_UF_RES_VIVAX$UF_NOTIF]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(UF_EDGELIST_RES_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 0,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("State of residence of reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"State of residence of reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}


##################
## UF-INF-LEVEL ##
##################

# Aggregate data by state of infection and malaria type
SIVEP_UF_INF_VIVAX = df %>%
  select(DT_NOTIF, UF_NOTIF, UF_INFEC, RES_EXAM) %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF) %>%
  group_by(YEAR, UF_NOTIF, UF_INFEC) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, UF_NOTIF, UF_INFEC, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Remove df
# rm(df)

# Assign names
SIVEP_UF_INF_VIVAX$UF_NOTIF = as.character(ADMIN_NAMES[match(SIVEP_UF_INF_VIVAX$UF_NOTIF, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_INF_VIVAX$UF_INFEC = as.character(ADMIN_NAMES[match(SIVEP_UF_INF_VIVAX$UF_INFEC, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_INF_VIVAX[is.na(SIVEP_UF_INF_VIVAX)] = 0

unique(SIVEP_UF_INF_VIVAX$UF_NOTIF)
unique(SIVEP_UF_INF_VIVAX$UF_INFEC)

# Remove if INF = 0
SIVEP_UF_INF_VIVAX=SIVEP_UF_INF_VIVAX[-which(SIVEP_UF_INF_VIVAX$UF_INFEC == "0"),]

# Remove self-links
SIVEP_UF_INF_VIVAX=SIVEP_UF_INF_VIVAX[-which(SIVEP_UF_INF_VIVAX$UF_INFEC == SIVEP_UF_INF_VIVAX$UF_NOTIF),]

# Get edgelist by infection
Years=2003:2018
UF_EDGELIST_INF_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("UF_EDGELIST_INF_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_UF_INF_VIVAX$UF_INFEC,
                                      to = SIVEP_UF_INF_VIVAX$UF_NOTIF,
                                      values = SIVEP_UF_INF_VIVAX[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_UF_INF_VIVAX$UF_NOTIF)))),rep("gray",length(unique(SIVEP_UF_INF_VIVAX$UF_INFEC))-length(unique(SIVEP_UF_INF_VIVAX$UF_NOTIF))))
names(Colors)=c(unique(SIVEP_UF_INF_VIVAX$UF_NOTIF),unique(SIVEP_UF_INF_VIVAX$UF_INFEC[!SIVEP_UF_INF_VIVAX$UF_INFEC %in% SIVEP_UF_INF_VIVAX$UF_NOTIF]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(UF_EDGELIST_INF_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 0,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("State of probable infection of reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"State of probable infection of reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}



###############################################################################


############################
## Run data upload script ##
############################

cat("Run data upload script\n")

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Get SIVEP raw notification data
load(FilePath)


##################
## AC-RES-LEVEL ##
##################

# Aggregate data by country of residence and malaria type
SIVEP_MU_RES_VIVAX_AC = df %>%
  select(DT_NOTIF, UF_NOTIF, MUN_NOTI, MUN_RESI, RES_EXAM) %>% 
  filter(UF_NOTIF == "12") %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, UF_NOTIF) %>%
  group_by(YEAR, MUN_NOTI, MUN_RESI) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, MUN_NOTI, MUN_RESI, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Remove df
# rm(df)

# Assign names
SIVEP_MU_RES_VIVAX_AC$MUN_NOTI = as.character(ADMIN_NAMES[match(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_RES_VIVAX_AC$MUN_RESI = as.character(ADMIN_NAMES[match(SIVEP_MU_RES_VIVAX_AC$MUN_RESI, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_RES_VIVAX_AC[is.na(SIVEP_MU_RES_VIVAX_AC)] = 0

unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI)
unique(SIVEP_MU_RES_VIVAX_AC$MUN_RESI)

# Remove if INF = 0
SIVEP_MU_RES_VIVAX_AC=SIVEP_MU_RES_VIVAX_AC[-which(SIVEP_MU_RES_VIVAX_AC$MUN_RESI == "0"),]

# Remove self-links
SIVEP_MU_RES_VIVAX_AC=SIVEP_MU_RES_VIVAX_AC[-which(SIVEP_MU_RES_VIVAX_AC$MUN_RESI == SIVEP_MU_RES_VIVAX_AC$MUN_NOTI),]

# Get edgelist by infection
Years=2003:2018
MU_EDGELIST_RES_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("MU_EDGELIST_RES_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_MU_RES_VIVAX_AC$MUN_RESI,
                                      to = SIVEP_MU_RES_VIVAX_AC$MUN_NOTI,
                                      values = SIVEP_MU_RES_VIVAX_AC[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI)))),rep("gray",length(unique(SIVEP_MU_RES_VIVAX_AC$MUN_RESI))-length(unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI))))
names(Colors)=c(unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI),unique(SIVEP_MU_RES_VIVAX_AC$MUN_RESI[!SIVEP_MU_RES_VIVAX_AC$MUN_RESI %in% SIVEP_MU_RES_VIVAX_AC$MUN_NOTI]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(MU_EDGELIST_RES_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Municipality of residence of Acre reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Municipality of residence of Acre reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}

##################
## AC-INF-LEVEL ##
##################

# Aggregate data by country of infection and malaria type
SIVEP_MU_INF_VIVAX_AC = df %>%
  select(DT_NOTIF, UF_NOTIF, MUN_NOTI, MUN_INFE, RES_EXAM) %>% 
  filter(UF_NOTIF == "12") %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, UF_NOTIF) %>%
  group_by(YEAR, MUN_NOTI, MUN_INFE) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, MUN_NOTI, MUN_INFE, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Remove df
# rm(df)

# Assign names
SIVEP_MU_INF_VIVAX_AC$MUN_NOTI = as.character(ADMIN_NAMES[match(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_INF_VIVAX_AC$MUN_INFE = as.character(ADMIN_NAMES[match(SIVEP_MU_INF_VIVAX_AC$MUN_INFE, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_INF_VIVAX_AC[is.na(SIVEP_MU_INF_VIVAX_AC)] = 0

unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI)
unique(SIVEP_MU_INF_VIVAX_AC$MUN_INFE)

# Remove if INF = 0
SIVEP_MU_INF_VIVAX_AC=SIVEP_MU_INF_VIVAX_AC[-which(SIVEP_MU_INF_VIVAX_AC$MUN_INFE == "0"),]

# Remove self-links
SIVEP_MU_INF_VIVAX_AC=SIVEP_MU_INF_VIVAX_AC[-which(SIVEP_MU_INF_VIVAX_AC$MUN_INFE == SIVEP_MU_INF_VIVAX_AC$MUN_NOTI),]

# Get edgelist by infection
Years=2003:2018
MU_EDGELIST_INF_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("MU_EDGELIST_INF_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_MU_INF_VIVAX_AC$MUN_INFE,
                                      to = SIVEP_MU_INF_VIVAX_AC$MUN_NOTI,
                                      values = SIVEP_MU_INF_VIVAX_AC[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI)))),rep("gray",length(unique(SIVEP_MU_INF_VIVAX_AC$MUN_INFE))-length(unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI))))
names(Colors)=c(unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI),unique(SIVEP_MU_INF_VIVAX_AC$MUN_INFE[!SIVEP_MU_INF_VIVAX_AC$MUN_INFE %in% SIVEP_MU_INF_VIVAX_AC$MUN_NOTI]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(MU_EDGELIST_INF_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Municipality of probable infection of Acre reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Municipality of probable infection of Acre reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}

#################################################################


# Where are venezuelans going?


############################
## Run data upload script ##
############################

cat("Run data upload script\n")

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Get SIVEP raw notification data
load(FilePath)

# Read country codes files
PAIS_CODE=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_CODES.csv"), header = TRUE, stringsAsFactors = FALSE)
PAIS_CODE$PAIS_CODE=as.character(PAIS_CODE$PAIS_CODE)


###############
## INFECTION ##

###########
## VIVAX ##

#UF
SIVEP_VENEZ_UF_INF_VIVAX = df %>%
  select(DT_NOTIF, PAIS_INF, UF_NOTIF, RES_EXAM) %>% 
  filter(PAIS_INF == "216") %>%
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, -PAIS_INF) %>%
  group_by(YEAR, UF_NOTIF) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, UF_NOTIF, Vivax) %>%
  spread(key = YEAR, value = Vivax)
#MU
SIVEP_VENEZ_MU_INF_VIVAX = df %>%
  select(DT_NOTIF, PAIS_INF, MUN_NOTI, RES_EXAM) %>% 
  filter(PAIS_INF == "216") %>%
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, -PAIS_INF) %>%
  group_by(YEAR, MUN_NOTI) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, MUN_NOTI, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Assign names
SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF = as.character(ADMIN_NAMES[match(SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF, ADMIN_NAMES$Code),"UF"])
SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF=as.factor(SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF)
SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI = as.character(ADMIN_NAMES[match(SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI, ADMIN_NAMES$Code),"Name"])
SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI=as.factor(SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI)
SIVEP_VENEZ_UF_INF_VIVAX[is.na(SIVEP_VENEZ_UF_INF_VIVAX)] = 0
SIVEP_VENEZ_MU_INF_VIVAX[is.na(SIVEP_VENEZ_MU_INF_VIVAX)] = 0

# UF: melt the data frame for plotting
mVENEZ_VIVAX_UF <- melt(SIVEP_VENEZ_UF_INF_VIVAX, id.vars='UF_NOTIF')
levels(mVENEZ_VIVAX_UF$UF_NOTIF) = levels(SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF) 

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=rev(getColors(length(levels(mVENEZ_VIVAX_UF$UF_NOTIF))))
names(Colors)=levels(mVENEZ_VIVAX_UF$UF_NOTIF)

# Stacked
mVENEZ_VIVAX_UF_Plot=ggplot(mVENEZ_VIVAX_UF, aes(y=value, x=variable, fill = UF_NOTIF)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="Proportion of imported P. vivax cases by case residence country", y="Proportion (%)", x="") +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Country of residence"))

mVENEZ_VIVAX_UF_Plot
