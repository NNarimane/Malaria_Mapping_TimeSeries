##########################################
##########################################
#####                                #####
#####  Brazilian P.vivax Time Series #####
#####              MAPS              #####
#####                                #####
##########################################
##########################################

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


library("sqldf")
library("taRifx")
library("reshape2")
library("malariaAtlas")
library("data.table")

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
byAge=FALSE
byImportation=FALSE
bySetting=FALSE


##############
## Data Upload
##############

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Load state abbreviations
ADMIN_NAMES=read.csv(file = paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv"), sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)

# Choose time period
StartYear="2003"
EndYear="2017"

# Melted data
Melted=TRUE

# Get time series data by administrative level and by variable of interest
if(byNotification){
  if(byType){TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)}
  if(byGender){TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)}
}

if(byResidence){
  if(byType){TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)}
  if(byGender){TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)}
}

if(byInfection){
  if(byType){TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)}
  if(byGender){TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)}
}


#############
## Save Plots
#############

# Save plots
SavePlots=TRUE

# Set folder for saving plots
if(SavePlots){
  if(envNN){
    if(byNotification){
      if(byType){Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byNotification/byType/")}
      if(byGender){Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byNotification/byGender/")}
    }
    
    if(byResidence){
      if(byType){Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byResidence/byType/")}
      if(byGender){Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byResidence/byGender/")}
    }
    
    if(byInfection){
      if(byType){Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byInfection/byType/")}
      if(byGender){Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byInfection/byGender/")}
    }
  }else{
    Plot_Folder="~/"
  }
}
