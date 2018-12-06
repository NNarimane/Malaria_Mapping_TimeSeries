##########################################
##########################################
#####                                #####
#####  Brazilian P.vivax Time Series #####
#####        Decomposition           #####
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

# For plots
Lsize=1.2
Y_Scale=c(-200,200)


##############
## Data Upload
##############

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TimeSeriesMalaria.csv")
}else{
  
}

################
## Parameters ##

# Start year
StartYear="2003"

# End year
EndYear="2017"

# Period of data
Period="DT_WEEK"

# Given period, how many in a year
Frequency=52

# How many years
Subset=15

##############
## Raw data ##

# Get raw time series data
TS_Raw=getPV_Raw_Data(FilePath, StartYear, EndYear)


##############
## Time Series 
##############

# Get data
TS=getPV_TS_Format(TS_Raw = TS_Raw, 
                 StartYear = StartYear,
                 EndYear = EndYear,
                 Period = Period,
                 Frequency = Frequency)

TS_State=getPV_TS_Format_Stratified(TS_Raw = TS_Raw,
                          StartYear = StartYear,
                          EndYear = EndYear,
                          Stratification = "STATE",
                          Period = Period,
                          Frequency = Frequency)

TS_Municipality=getPV_TS_Format_Stratified(TS_Raw = TS_Raw,
                                 StartYear = StartYear,
                                 EndYear = EndYear,
                                 Stratification = "MU_NAME",
                                 Period = Period,
                                 Frequency = Frequency)

################
## Decomposition
################

# Trends
TS_Seasonal=getPV_TS_Trend(TS, 1, 2, "All Data")
TS_Trend=getPV_TS_Trend(TS, 2, 2, "All Data")
TS_Remainder=getPV_TS_Trend(TS, 3, 2, "All Data")

# Trends
TS_State_Seasonal=getPV_TS_Trend(TS_State, 1, 2, "State")
TS_State_Trend=getPV_TS_Trend(TS_State, 2, 2, "State")
TS_State_Remainder=getPV_TS_Trend(TS_State, 3, 2, "State")

# Trends
TS_Municipality_Seasonal=getPV_TS_Trend(TS_Municipality, 1, 2, "Municipality")
TS_Municipality_Trend=getPV_TS_Trend(TS_Municipality, 2, 2, "Municipality")
TS_Municipality_Remainder=getPV_TS_Trend(TS_Municipality, 3, 2, "Municipality")

#######
## Melt 
#######

# Order by group
GroupOrder=c("All Data", "State", "Municipality")

# Group all data
Melted_Seasonal_Data=rbind(TS_Seasonal, TS_State_Seasonal, TS_Municipality_Seasonal)
Melted_Seasonal_Data$Group=factor(Melted_Seasonal_Data$Group,levels=GroupOrder)

Melted_Trend_Data=rbind(TS_Trend, TS_State_Trend, TS_Municipality_Trend)
Melted_Trend_Data$Group=factor(Melted_Trend_Data$Group,levels=GroupOrder)

Melted_Remainder_Data=rbind(TS_Remainder, TS_State_Remainder, TS_Municipality_Remainder)
Melted_Remainder_Data$Group=factor(Melted_Remainder_Data$Group,levels=GroupOrder)


####################
## Seasonality Plots
####################

# Seasonality Pattern Plot
Seasonality_Plot=ggplot(data = Melted_Seasonal_Data, aes(Year, Cases, color = Name)) +
  geom_line(size=Lsize) +
  facet_wrap(~Group, scales = "free_x", ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  labs(title = "Seasonality Trends of P. Vivax Cases", subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
       x = "Month", y = "Seasonal Pattern") +
  guides(color=guide_legend(title="Data stratification")) +
  theme_gray() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#######
## Save
#######

if(SavePlots){
  # Plot
  Seasonality_Plot
  # Save
  dev.copy(png,paste0(Plot_Folder,"Decomposition/Seasonality/Seasonality Patterns of P. vivax cases ", StartYear,"-", EndYear, ".png"),
           width = 1200, height = 1000, units = "px", pointsize = 12, res = 72)
  dev.off()
}


##############
## Trend Plots
##############

Trend_Plot=ggplot(data = Melted_Trend_Data, aes(Year, Cases, color = Name)) +
  geom_line(size=Lsize) +
  facet_wrap(~Group, scales = "free_x", ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  labs(title = "Trend Lines of P. Vivax Cases", subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
       x = "Month", y = "Number of Cases") +
  guides(color=guide_legend(title="Data stratification")) +
  theme_gray() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#######
## Save
#######

if(SavePlots){
  # Plot
  Trend_Plot
  # Save
  dev.copy(png,paste0(Plot_Folder,"Decomposition/Trend/Trend Lines of P. vivax cases ", StartYear,"-", EndYear, ".png"),
           width = 1200, height = 1000, units = "px", pointsize = 12, res = 72)
  dev.off()
}

##################
## Remainder Plots
##################

Remainder_Plot=ggplot(data = Melted_Remainder_Data, aes(Year, Cases, fill = Name)) +
  # geom_line(size=Lsize) +
  geom_bar(stat = "identity") +
  facet_wrap(~Group, scales = "free_x", ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  labs(title = "Remainder of P. Vivax Cases", subtitle = "Brazil, 2003-2017",
       x = "Month", y = "Number of Cases") +
  guides(fill=guide_legend(title="Data stratification")) +
  theme_gray() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

#######
## Save
#######

if(SavePlots){
  # Plot
  Remainder_Plot
  # Save
  dev.copy(png,paste0(Plot_Folder,"Decomposition/Remainder/Trend Lines of P. vivax cases ", StartYear,"-", EndYear, ".png"),
           width = 1200, height = 1000, units = "px", pointsize = 12, res = 72)
  dev.off()
}

################################
## Test for seasonality patterns
################################

## Autocorrelation: 
## Can indicate seasonality but not conclusive

acf(TS)
pacf(TS)
# Plots by states
par(mar=c(3,3,3,3))
par(mfrow=c(length(TS_State),2))
for(i in 1:length(TS_State)){
  acf(TS_State[[i]])
  pacf(TS_State[[i]])
}
dev.off()
# Plots by municiaplity
par(mar=c(1.2,1.2,1.2,1.2))
par(mfrow=c(length(TS_Municipality),2))
for(i in 1:length(TS_Municipality)){
  acf(TS_Municipality[[i]])
  pacf(TS_Municipality[[i]])
}
dev.off()


## TBATS model: 
## handles weekly seasonality and will automatically 
## determine if a seasonal pattern is present

# Combine time series data into a list
List_Seasonal_Data=c(AllData=as.list(TS), TS_State, TS_Municipality)

# Get TBATS model fit results
TBATS_Fitted_Models=lapply(List_Seasonal_Data, tbats)

# Create data.frame for TBATS model results
TBATS_Seasonality_Results=as.data.frame(cbind(Data=names(List_Seasonal_Data),
                                        Seasonality=sapply(TBATS_Fitted_Models, function(X) !is.null(X$seasonal))),
                                        row.names = F)


# Plot
foreach(i = 1:length(TBATS_Fitted_Models)) %do% table(c(!is.null(TBATS_Fitted_Models[[i]]$seasonal)))

#######
## Save
#######

write.csv(TBATS_Seasonality_Results, file = paste0(Plot_Folder,"Decomposition/TBATS_Seasonality_Results.csv"))

