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

#############
## Save Plots
#############

# Save plots
SavePlots=TRUE

# Set folder for saving plots
if(SavePlots){
  if(envNN){
    Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/")
  }else{
    Plot_Folder="~/"
  }
}


##############
## Data Upload
##############

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Choose time period
StartYear="2003"
EndYear="2017"

# Get time series data
TS=getDaily_SIVEP_Species(FilePath)


################
## General Plots
################


# Daily number of cases, bar graph
DailyPlot=ggplot(data = TS, aes(DT_NOTIF, CASES_N, color = MALARIA_TYPE)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(breaks = "year", 
               date_labels = "%Y") +
  labs(title = paste0("Daily P. vivax cases in 10 BAR Municipalities ", StartYear, "-", EndYear), x = "Year", y = "Number of Cases")


# Weekly number of cases, bar graph
WeeklyPlot=ggplot(data = TS, aes(DT_WEEK, VIVAX)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(date_breaks= "year", 
               date_labels = "%Y") +
  labs(title = paste0("Weekly P. vivax cases in 10 BAR Municipalities ", StartYear, "-", EndYear), x = "Year", y = "Number of Cases")

# Monthly number of cases, line graph
MonthlyPlot=ggplot(data = TS, aes(DT_MONTH, VIVAX)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(date_breaks= "year", 
               date_labels = "%Y") +
  labs(title = paste0("Monthly P. vivax cases in 10 BAR Municipalities ", StartYear, "-", EndYear), x = "Year", y = "Number of Cases")

# Yearly number of cases, line graph
YearlyPlot=ggplot(data = TS, aes(DT_YEAR, VIVAX)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(date_breaks= "year", 
               date_minor_breaks = "year", 
               date_labels = "%Y") +
  labs(title = paste0("Yearly P. vivax cases in 10 BAR Municipalities ", StartYear, "-", EndYear), x = "Year", y = "Number of Cases")

# Plot together
grid.arrange(DailyPlot, WeeklyPlot, MonthlyPlot, YearlyPlot, nrow=2)


#######
## Save
#######

if(SavePlots){
  # Plot
  grid.arrange(DailyPlot, WeeklyPlot, MonthlyPlot, YearlyPlot, nrow=2)
  
  # Save
  dev.copy(png, paste0(Plot_Folder, "P. vivax Cases in 10 BAR Municipalities ", StartYear, "-", EndYear, ".png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}


#################
## Plots by State
#################

DailyPlot_ST=DailyPlot + facet_wrap(~STATE, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
WeeklyPlot_ST=WeeklyPlot + facet_wrap(~STATE, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
MonthlyPlot_ST=MonthlyPlot + facet_wrap(~STATE, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
YearlyPlot_ST=YearlyPlot + facet_wrap(~STATE, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot together
grid.arrange(DailyPlot_ST, WeeklyPlot_ST, MonthlyPlot_ST, YearlyPlot_ST, nrow=2)


#######
## Save
#######

if(SavePlots){
  # Plot
  grid.arrange(DailyPlot_ST, WeeklyPlot_ST, MonthlyPlot_ST, YearlyPlot_ST, nrow=2)
  
  # Save
  dev.copy(png, paste0(Plot_Folder, "P. vivax Cases in 10 BAR Municipalities ", StartYear, "-", EndYear, ".png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}


########################
## Plots by Municipality
########################

# Daily
DailyPlot_MU = DailyPlot + facet_wrap(~MU_NAME, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Weekly
WeeklyPlot_MU = WeeklyPlot + facet_wrap(~MU_NAME, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Monthly
MonthlyPlot_MU = MonthlyPlot + facet_wrap(~MU_NAME, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Yearly
YearlyPlot_MU = YearlyPlot + facet_wrap(~MU_NAME, ncol = 5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))



#######
## Save
#######


if(SavePlots){
  # Plot daily
  DailyPlot_MU
  # Save
  dev.copy(png, paste0(Plot_Folder,"Daily P. vivax Cases in 10 BAR Municipalities ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  
  # Plot weekly
  WeeklyPlot_MU
  # Save
  dev.copy(png, paste0(Plot_Folder,"Weekly P. vivax Cases in 10 BAR Municipalities ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  
  # Plot monthly
  MonthlyPlot_MU
  # Save
  dev.copy(png, paste0(Plot_Folder,"Monthly P. vivax Cases in 10 BAR Municipalities ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  
  # Plot yearly
  YearlyPlot_MU
  # Save
  dev.copy(png, paste0(Plot_Folder,"Yearly P. vivax Cases in 10 BAR Municipalities ", StartYear, "-", EndYear, "by MU.png"),
           width = 1800, height = 800, units = "px", pointsize = 12,
           res = 72)
  dev.off()
}
