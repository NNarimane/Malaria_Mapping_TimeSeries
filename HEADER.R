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
library("naniar")

####################
## Load Functions ##
####################

# Source functions script
source(paste0(getwd(),"/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Functions.R"))


#################################################################
####################### 1. ANALYSIS LEVEL #######################
#################################################################

# Load data or run code?
loadCleanData = TRUE

# Melted data
Melted=TRUE

# Choose time period
StartYear = "2003"
EndYear = "2017"

# Load state abbreviations
ADMIN_NAMES=read.csv(file = paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv"), sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)


###############
## DETECTION ##
###############

# Get all data or by detection type?
byAll_Detection = TRUE
if(!byAll_Detection){
  byPCD = TRUE
  byACD = FALSE
  byCV = FALSE
}


##################
## CASES OR API ##
##################

# If API = TRUE, data will also include case numbers
API = FALSE

# Merge UF and MU
Merged = FALSE


#################
## ADMIN LEVEL ##
#################

# Which administrative level ?
byNotification = TRUE
byResidence = FALSE
byInfection = FALSE


###############
## VARIABLES ##
###############

# Which variable(s) ?
byType = TRUE
byGender = FALSE
byAge = FALSE
byTreatment = FALSE


################
## File Paths ##
################

# Detection type
if(byAll_Detection){
  Detection_Level="byAll_Detection"
}else{
  if(byPCD){
    Detection_Level="byPCD"
  }
  if(byACD){
    Detection_Level="byACD"
  }
  if(byCV){
    Detection_Level="byCV"
  }
}

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
  
  # By MU
  Level="MU"
  TS_MU_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Level,"_", Date_Type,"_API","per",Denominator,"_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".csv"))
  
  # By UF
  Level="UF"
  TS_UF_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Level,"_", Date_Type,"_API","per",Denominator,"_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".csv"))
  
  if(Merged){
    TS=rbind(TS_UF_API, TS_MU_API)
    TS$DATE=as.Date(TS$DATE)
    rm(TS_MU_API, TS_UF_API)
  }
  
 
}else{
  
  #################
  ## Load or Run ##
  #################
  
  if(loadCleanData){
    
    ####################
    ## Load case data ##
    ####################
    
    cat("Load case data\n")
    
    load(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".RData"))
    
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
      save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".RData"))
      
    }
    if(byGender){
      
      cat("Get data by gender\n")
      TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)
      
      cat("Save\n")
      save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".RData"))
      
    }
    if(byAge){
      
      cat("Get data by age\n")
      TS=getDAILY_SIVEP_MALARIA_AGE(FilePath, StartYear, EndYear, Melted)
      
      cat("Save\n")
      save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".RData"))
      
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

# Log-transformed
Log = FALSE

# Seasonality
# Seasonality = TRUE

# Get path based on cases or API plots
if(API){
  if(!Log){
    Plot_Level="/byAPI/"
  }else{
    Plot_Level="/byAPI/Log/"
  }
}else{
  Plot_Level="/byCases/"
}


########################
## Assign Plot Folder ##
########################

if(SavePlots){
  Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots",Detection_Level,"_",Plot_Level,Admin_Level,"/",Variable_Level,"/")
}

