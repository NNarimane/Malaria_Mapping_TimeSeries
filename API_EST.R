##########################################################
##########################################################
##########                                      ##########
##########                                      ##########
##########      *** SIVEP DATA ANALYSIS ***     ##########
##########        *** API CALCULATIONS ***      ##########
##########                                      ##########
##########                                      ##########
##########################################################
##########################################################

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

######################
## Read header file ##
######################

source(paste0(getwd(),"/Malaria_Mapping_TimeSeries/HEADER.R"))

############################
## Upload Population Data ##
############################

# Load population estimates, 2003-2018
POP_EST=read.csv("Malaria_Mapping_TimeSeries_Data/BRA_POP_EST.csv", stringsAsFactors = F, row.names = NULL, check.names = F)
POP_EST$CODE=as.character(POP_EST$CODE)


###################
## Calculate API ##
###################

# Choose denominator
Denominator = 1000

# By date type
Date_Type="Daily"

# By MU
Level="MU"
TS_MU_API=getAPI(TS, Level, Date_Type, Denominator)
# Save
write.csv(TS_MU_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Level,"_", Date_Type,"_API","per",Denominator,"_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".csv"), row.names = F)


# By UF
Level="UF"
TS_UF_API=getAPI(TS, Level, Date_Type, Denominator)
# Save
write.csv(TS_UF_API, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_",Level,"_", Date_Type,"_API","per",Denominator,"_",Detection_Level,"_",Admin_Level,"_",Variable_Level, ".csv"), row.names = F)
 
 
