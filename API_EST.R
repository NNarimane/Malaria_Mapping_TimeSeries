######################################
###                                ###
### SIVEP 2003-2018                ###
### Annual Parasite Index          ###
### Calculations                   ###
###                                ###
######################################

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

#################
## Load Functions
#################

# Source functions script
source(paste0(getwd(),"/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Functions.R"))


#################
## Analysis Level
#################

# Which administrative level ?
byNotification=TRUE
byResidence=FALSE
byInfection=FALSE

# Which variable(s) ?
byType=TRUE
byGender=FALSE
# byAge=FALSE
# byImportation=FALSE
# bySetting=FALSE


##############
## Data Upload
##############

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Load population estimates, 2003-2018
POP_EST=read.csv("Malaria_Mapping_TimeSeries_Data/BRA_POP_EST.csv", stringsAsFactors = F, row.names = NULL, check.names = F)
POP_EST$CODE=as.character(POP_EST$CODE)

# Load state abbreviations
ADMIN_NAMES=read.csv(file = paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv"), sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)

# Choose time period
StartYear="2003"
EndYear="2017"

# Melted data
Melted=TRUE

# Get time series data by administrative level and by variable of interest
loadCleanData=TRUE
if(loadCleanData){
  if(byNotification){
    if(byType){load(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byType_byNotification.RData"))}
    if(byGender){load()}
  }
  
  if(byResidence){
    if(byType){load(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byType_byResidence.RData"))}
    if(byGender){load()}
  }
  
  if(byInfection){
    if(byType){load(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byType_byInfection.RData"))}
    if(byGender){load()}
  }
  
}


################
## Calculate API
################

# Give BR code "0"
TS[which(TS$LEVEL == "BR"),"CODE"] = "0"

# Add year
TS$YEAR=year(TS$DATE)

# Get function that calculates API by year
getAPI=function(TS, LEVEL, DATE_TYPE){
  
  # Get annual aggregate of cases
  TS=TS[which(TS$DATE_TYPE == DATE_TYPE),]
  TS$YEAR=year(TS$DATE)
  TS=subset(TS, select = -c(DATE_TYPE))
  
  # Level of time series
  TS=TS[which(TS$LEVEL == LEVEL),]
  
  # Get population estimates by level (state or municipality)
  POP_EST_LEVEL=POP_EST[which(POP_EST$LEVEL == LEVEL),]
  
  # Aggregate 
  TS_API=aggregate(CASES~., data = TS, FUN = sum, na.action = na.omit)
  
  # Get population of each level by year
  TS_API$POP=foreach(i=1:nrow(TS_API), .combine = "rbind") %do% {
    POP=ifelse(is.null(POP_EST_LEVEL[which(TS_API[i,"CODE"] == POP_EST_LEVEL[,"CODE"]),as.character(TS_API[i,"YEAR"])]),NA, POP_EST_LEVEL[which(TS_API[i,"CODE"] == POP_EST_LEVEL[,"CODE"]),as.character(TS_API[i,"YEAR"])])
  }
  
  # Get API
  TS_API$API=(TS_API$CASES/TS_API$POP)*1000
  
  # Assign names
  TS_API$STATE = ADMIN_NAMES[match(TS_API$CODE, ADMIN_NAMES$Code),"UF"] 
  TS_API$NAME = ADMIN_NAMES[match(TS_API$CODE, ADMIN_NAMES$Code),"Name"] 
  
  
  return(TS_API)
}

# # Daily
# DATE_TYPE="Daily"
# # By MU
# TS_MU_API=getAPI(TS, "MU", DATE_TYPE)
# # By UF
# TS_UF_API=getAPI(TS, "UF", DATE_TYPE)

# Weekly
DATE_TYPE="Weekly"
# By MU
TS_MU_API=getAPI(TS, "MU", DATE_TYPE)
# By UF
TS_UF_API=getAPI(TS, "UF", DATE_TYPE)

# Monthly
DATE_TYPE="Monthly"
# By MU
TS_MU_API=getAPI(TS, "MU", DATE_TYPE)
# By UF
TS_UF_API=getAPI(TS, "UF", DATE_TYPE)

# Yearly
DATE_TYPE="Yearly"
# By MU
TS_MU_API=getAPI(TS, "MU", DATE_TYPE)
# By UF
TS_UF_API=getAPI(TS, "UF", DATE_TYPE)



############
## Save Data
############

if(byNotification){
  write.csv(TS_MU_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_", DATE_TYPE, "_API_MU_byNotification.csv"), row.names = F)
  write.csv(TS_UF_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_", DATE_TYPE, "_API_UF_byNotification.csv"), row.names = F)
  
}

if(byResidence){
  write.csv(TS_MU_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_", DATE_TYPE, "_API_MU_byResidence.csv"), row.names = F)
  write.csv(TS_UF_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_", DATE_TYPE, "_API_UF_byResidence.csv"), row.names = F)
  
}

if(byInfection){
  write.csv(TS_MU_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_", DATE_TYPE, "_API_MU_byInfection.csv"), row.names = F)
  write.csv(TS_UF_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_", DATE_TYPE, "_API_UF_byInfection.csv"), row.names = F)
  
}


















