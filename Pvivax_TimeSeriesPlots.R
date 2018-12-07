##########################################
##########################################
#####                                #####
#####  Brazilian P.vivax Time Series #####
#####             Plots              #####
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

# Install once
# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("stringr")
# install.packages("gridExtra")
# install.packages("data.table")
# install.packages("fUnitRoots")
# install.packages("FitAR")

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
byNotification=FALSE
byResidence=TRUE
byInfection=FALSE

# Which variable(s)
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
      if(byType){Plot_Folder=paste0(getwd(),"/byNotification/byType/Malaria_Mapping_TimeSeries_Plots/")}
      if(byGender){Plot_Folder=paste0(getwd(),"/byNotification/byGender/Malaria_Mapping_TimeSeries_Plots/")}
    }
    
    if(byResidence){
      if(byType){Plot_Folder=paste0(getwd(),"/byResidence/byType/Malaria_Mapping_TimeSeries_Plots/")}
      if(byGender){Plot_Folder=paste0(getwd(),"/byResidence/byGender/Malaria_Mapping_TimeSeries_Plots/")}
    }
    
    if(byInfection){
      if(byType){Plot_Folder=paste0(getwd(),"/byInfection/byType/Malaria_Mapping_TimeSeries_Plots/")}
      if(byGender){Plot_Folder=paste0(getwd(),"/byInfection/byGender/Malaria_Mapping_TimeSeries_Plots/")}
    }
  }else{
    Plot_Folder="~/"
  }
}


################
## General Plots
################

if(!Melted){
  # Daily number of cases, bar graph
  DailyPlot=ggplot(data = TS, aes(DT_NOTIF, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    labs(title = paste0("Daily P. vivax and P. falciparum cases in Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species"))
  
  
  # Weekly number of cases, bar graph
  WeeklyPlot=ggplot(data = TS, aes(DT_WEEK, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line", size = 1.05) +
    scale_x_date(date_breaks= "year", 
                 date_labels = "%Y") +
    labs(title = paste0("Weekly P. vivax and P. falciparum cases in Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") +
    guides(color=guide_legend(title="Plasmodium species")) + theme_light()
  
  # Monthly number of cases, line graph
  MonthlyPlot=ggplot(data = TS, aes(DT_MONTH, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line", size = 1.05) +
    scale_x_date(date_breaks= "year", 
                 date_labels = "%Y") +
    labs(title = paste0("Monthly P. vivax and P. falciparum cases in Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") +
    guides(color=guide_legend(title="Plasmodium species"))
  
  # Yearly number of cases, line graph
  YearlyPlot=ggplot(data = TS, aes(DT_YEAR, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line", size = 1.05) +
    scale_x_date(date_breaks= "year", 
                 date_minor_breaks = "year", 
                 date_labels = "%Y") +
    labs(title = paste0("Yearly P. vivax and P. falciparum cases in Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") +
    guides(color=guide_legend(title="Plasmodium species"))
  
  # Plot together
  grid.arrange(DailyPlot, WeeklyPlot, MonthlyPlot, YearlyPlot, nrow=2)
  
  
  #######
  ## Save
  #######
  
  if(SavePlots){
    # Plot
    grid.arrange(DailyPlot, WeeklyPlot, MonthlyPlot, YearlyPlot, nrow=2)
    
    # Save
    dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases in Brazil ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
  }
}else{
  
  # Plot together (avoid double counting with level)
  TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU"), 
                         aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~DATE_TYPE, scales = "free_y") +
    labs(title = paste0("P. vivax and P. falciparum cases in Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species"))
  
  # Gender
  TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU" & !is.na(GENDER)), 
                         aes(DATE, CASES, color = GENDER)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~DATE_TYPE, scales = "free_y") +
    labs(title = paste0("P. vivax and P. falciparum cases in Brazil by gender, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Gender"))
  
  
  #######
  ## Save
  #######
  
  if(SavePlots){
    # Plot
    TS_CombinedPlot
    
    # Save
    dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases in Brazil by gender, ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
  }
}



###############################
## Get WEEKLY DATA
###############################

# Keep only weekly date type for plotting
TS_WEEKLY=TS[which(TS$DATE_TYPE == "Weekly"),]
TS_WEEKLY=TS_WEEKLY[,-"DATE_TYPE"]

# Assign names
TS_WEEKLY$STATE = ADMIN_NAMES[match(TS_WEEKLY$CODE, ADMIN_NAMES$Code),"UF"] 
TS_WEEKLY$NAME = ADMIN_NAMES[match(TS_WEEKLY$CODE, ADMIN_NAMES$Code),"Name"] 

###############################
## Plots by State (WEEKLY DATA)
###############################

# Plot together (avoid double counting with level)
TS_CombinedPlot_UF=ggplot(data = subset(TS_WEEKLY, LEVEL == "UF"), aes(DATE, CASES, color = TYPE)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(breaks = "year", 
               date_labels = "%Y") +
  facet_wrap(~STATE) +
  labs(title = paste0("P. vivax and P. falciparum cases in Brazil by state, ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
  guides(color=guide_legend(title="Plasmodium species"))

# Gender
TS_CombinedPlot_UF=ggplot(data = subset(TS_WEEKLY, LEVEL == "UF" & !is.na(GENDER)), 
                          aes(DATE, CASES, color = GENDER)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(breaks = "year", 
               date_labels = "%Y") +
  facet_wrap(~STATE) +
  labs(title = paste0("P. vivax and P. falciparum cases in Brazil by state and gender, ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
  guides(color=guide_legend(title="Gender"))


#######
## Save
#######

if(SavePlots){
  # Plot
  TS_CombinedPlot_UF
  
  # Save
  dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases in Brazil by state and gender, ", StartYear, "-", EndYear, ".png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}


######################################
## Plots by Municipality (WEEKLY DATA)
######################################

# Count number of municipalities per state
aggregate(Name~UF, ADMIN_NAMES, FUN = length)

# Plot together (avoid double counting with level) by each state
byGender=TRUE

if(!byGender){
  ### AC ####
  TS_Plot_AC=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "AC"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Acre state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### AM ####
  TS_Plot_AM=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "AM"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Amazonas state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### PA ####
  TS_Plot_PA=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "PA"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Para state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### RO ####
  TS_Plot_RO=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "RO"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Rondonia state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### RR ####
  TS_Plot_RR=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "RR"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Roraima state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
}else{
  ### AC ####
  TS_Plot_AC=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "AC" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Acre state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Gender")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### AM ####
  TS_Plot_AM=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "AM" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Amazonas state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Gender")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### PA ####
  TS_Plot_PA=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "PA" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Para state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Gender")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### RO ####
  TS_Plot_RO=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "RO" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Rondonia state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Gender")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  ### RR ####
  TS_Plot_RR=ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == "RR" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(~NAME) +
    labs(title = paste0("P. vivax and P. falciparum cases in Roraima state by municipality, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Gender")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
}


#######
## Save
#######


if(SavePlots){
  if(byGender){
    Plot_Folder=paste0(Plot_Folder,"byGender/")
  }
  # Acre
  TS_Plot_AC
  # Save
  dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Acre state by municipality ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  
  # Amazonas
  TS_Plot_AM
  # Save
  dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Amazonas state by municipality ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  
  # Para
  TS_Plot_PA
  # Save
  dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Para state by municipality ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  
  # Rondonia
  TS_Plot_RO
  # Save
  dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Rondonia state by municipality ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 72)
  dev.off()
  
  # Roraima
  TS_Plot_RR
  # Save
  dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Roraima state by municipality ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 72)
  dev.off()
}
