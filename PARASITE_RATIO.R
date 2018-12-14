########################################################
########################################################
##########                                    ##########
##########                                    ##########
##########    *** SIVEP DATA ANALYSIS ***     ##########
##########        *** Pf:Pv Ratio ***         ##########
##########                                    ##########
##########                                    ##########
########################################################
########################################################

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


##############################
## Rehsape by Cases by Type ##
##############################

TS_MU_API_Wide=reshape(TS_MU_API, idvar = c("LEVEL","CODE","DATE","YEAR","CASES","POP","API","STATE","NAME"), timevar = c("TYPE"), direction = "wide")



