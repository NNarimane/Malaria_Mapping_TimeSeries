##########################################################
##########################################################
##########                                      ##########
##########                                      ##########
##########      *** SIVEP DATA ANALYSIS ***     ##########
##########            *** HEADER ***            ##########
##########                                      ##########
##########                                      ##########
##########################################################
##########################################################


#################################
## Load all necessary packages ##
#################################

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

####################
## Load Functions ##
####################

# Source functions script
source(paste0(getwd(),"/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Functions.R"))


#################################################################
####################### 1. ANALYSIS LEVEL #######################
#################################################################

# Load data or run code?
loadCleanData = FALSE

# Which administrative level ?
byNotification = TRUE
byResidence = FALSE
byInfection = FALSE

# Which variable(s) ?
byType = TRUE
byGender = FALSE
byAge = FALSE
byTreatment = FALSE

# Choose time period
StartYear = "2003"
EndYear = "2017"

# Load state abbreviations
ADMIN_NAMES=read.csv(file = paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv"), sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)


#####################
## By cases or API ##
#####################

# If API = TRUE, data will also include case numbers
API = FALSE

# Melted data
Melted=TRUE


################
## File Paths ##
################

# Administrative level
if(byNotification){
  Admin_Level="byNotification"
}
if(byResidence){
  Admin_Level="byResidence"
}
if(byInfection){
  Admin_Level="byInfection"
}

# Variable level
if(byType){
  Variable_Level="byType"
}
if(byGender){
  Variable_Level="byGender"
}
if(byAge){
  Variable_Level="byAge"
}
if(byTreatment){
  Variable_Level="byTreatment"
}


##############################################################
####################### 2. DATA UPLOAD #######################
##############################################################

if(API){
  
  ########################
  ## Load data with API ## 
  ########################
  
  cat("Load data with API\n")
  
  TS_MU_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_MU_",Admin_Level,"_",Variable_Level, ".csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS_UF_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_UF_",Admin_Level,"_",Variable_Level, ".csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS=rbind(TS_UF_API, TS_MU_API)
  TS$DATE=as.Date(TS$DATE)
  rm(TS_MU_API, TS_UF_API)
 
}else{
  
  #################
  ## Load or Run ##
  #################
  
  if(loadCleanData){
    
    ####################
    ## Load case data ##
    ####################
    
    cat("Load case data\n")
    
    load(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_",Admin_Level,"_",Variable_Level, ".RData"))
    
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
    
    # Get time series data by administrative level and by variable of interest
    if(byType){
      
      cat("Get data by malaria type\n")
      TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)
      
      cat("Save\n")
      save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_",Admin_Level,"_",Variable_Level, ".RData"))
      
    }
    if(byGender){
      
      cat("Get data by gender\n")
      TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)
      
      cat("Save\n")
      save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_",Admin_Level,"_",Variable_Level, ".RData"))
      
    }
    if(byAge){
      
      cat("Get data by age\n")
      TS=getDAILY_SIVEP_MALARIA_AGE(FilePath, StartYear, EndYear, Melted)
      
      cat("Save\n")
      save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_",Admin_Level,"_",Variable_Level, ".RData"))
      
    }
  }
}



########################################################
####################### 3. PLOTS #######################
########################################################

# Scientific notation
options(scipen=999)

# Save plots
SavePlots=TRUE

# Get path based on cases or API plots
if(API){
  Plot_Level="/byAPI/"
}else{
  Plot_Level="/byCases/"
}


########################
## Assign Plot Folder ##
########################

if(SavePlots){
  Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots",Plot_Level,Admin_Level,"/",Variable_Level,"/")
}

