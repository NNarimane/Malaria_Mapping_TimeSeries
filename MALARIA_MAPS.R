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

# Choose time period
StartYear="2003"
EndYear="2017"


#################
## Load Data
#################

if(byNotification){
  TS_MU_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_MU_byNotification.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS_UF_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_UF_byNotification.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS=rbind(TS_UF_API, TS_MU_API)
  TS$DATE=as.Date(TS$DATE)
  rm(TS_MU_API,TS_UF_API)
}

if(byResidence){
  TS_MU_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_MU_byResidence.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS_UF_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_UF_byNotification.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS=rbind(TS_UF_API, TS_MU_API)
}

if(byResidence){
  TS_MU_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_MU_byResidence.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS_UF_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_UF_byNotification.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
  TS=rbind(TS_UF_API, TS_MU_API)
}

#############
## Save Plots
#############

# Save plots
SavePlots=TRUE
options(scipen=999)

# Set folder for saving plots
API=F
if(SavePlots){
  if(envNN){
    if(byNotification){
      if(byType){
        if(!API){
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byNotification/byType/")
        }else{
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byNotification/byType/")
        }
        if(byGender){
          if(!API){
            Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byNotification/byGender/")
          }else{
            Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byNotification/byGender/")
          }
        }
      }
    }
    if(byResidence){
      if(byType){
        if(!API){
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byResidence/byType/")
        }else{
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byResidence/byType/")
        }
        if(byGender){
          if(!API){
            Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byResidence/byGender/")
          }else{
            Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byResidence/byGender/")
          }
        }
      }
    }
    
    if(byInfection){
      if(byType){
        if(!API){
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byInfection/byType/")
        }else{
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byInfection/byType/")
        }
        if(byGender){
          if(!API){
            Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byInfection/byGender/")
          }else{
            Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byInfection/byGender/")
          }
        }
      }
    }
  }
}else{}

###################
### Map Shape Files
###################

#################
## State-Level ##

# State-level shape files
BRA_SHP_UF=getShp(country = "Brazil", admin_level = "admin1", format = "df")

# Plot
autoplot(BRA_SHP_UF)

# Get function
getSTATE_MAPS=function(Level, Type, Measure){
  # Select data to plot
  TS_UF=TS[which(TS$LEVEL == Level & TS$TYPE == Type),]
  TS_UF=subset(TS_UF, select = -c(API,POP,DATE))
  
  # Make TS data wide by year and by type
  wTS_UF=reshape(TS_UF, idvar = c("LEVEL","CODE","TYPE","STATE","NAME"), 
                 timevar = "YEAR", direction = "wide")
  
  # Step 1: make names into character temporarily
  BRA_SHP_UF$name_1=as.character(BRA_SHP_UF$name_1)
  
  # Step 2: merge (left_join will help keep in order)
  BRA_SHP_UF_SIVEP=left_join(BRA_SHP_UF, wTS_UF, by = c("name_1" = "NAME"))
  
  # Step 3: reorder and change back to factors
  BRA_SHP_UF_SIVEP$name_1=factor(BRA_SHP_UF_SIVEP$name_1)
  
  # Melt data
  setDT(BRA_SHP_UF_SIVEP)
  mBRA_SHP_UF_SIVEP=melt(BRA_SHP_UF_SIVEP,
                         measure.vars = list(paste0("CASES.",seq(2003,2017,1))),
                         variable.name = "YEAR", 
                         value.name = "CASES",
                         id.vars = 1:23)
  
  # Create population categorical variable
  mBRA_SHP_UF_SIVEP$CASES_CAT <- cut(as.numeric(mBRA_SHP_UF_SIVEP$CASES), 
                                     breaks = breaks, labels = labels)
  
  # Fix Year labels
  levels(mBRA_SHP_UF_SIVEP$YEAR)=as.character(seq(2003,2017,1))
  
  # Plot
  PLOT=ggplot(mBRA_SHP_UF_SIVEP) +
    aes(long, lat, group=group, fill = CASES_CAT) +
    scale_fill_brewer(palette="Blues", na.value = "white") +
    coord_equal() +
    geom_polygon(colour = "black", size = 0.5) +
    facet_wrap(facets= YEAR ~.) 
  
  return(PLOT)
}

# Set break points
breaks=c(1000, seq(25000,225000,25000))
labels=paste("<", breaks[2:length(breaks)])

# Plot
Plot=getSTATE_MAPS("UF", "Vivax", "CASES")
Plot + labs(title = "Plasmodium vivax cases, Brazil 2003-2017")

# Save
dev.copy(png, paste0(Plot_Folder,"Map of Plasmodium vivax cases, Brazil ", StartYear, "-", EndYear, ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()
