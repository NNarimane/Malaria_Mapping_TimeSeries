###########################################
###########################################
#####                                 #####
#####  Brazilian Malaria Time Series  #####
#####             Plots               #####
#####                                 #####
###########################################
###########################################

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


##########################################
## Load header file: set-up analysis level
##########################################

# Source header file
source(paste0(getwd(),"/Malaria_Mapping_TimeSeries/HEADER.R"))


###################################################
## Get data by date type and top incidence by level
###################################################

if(!API){
  # Keep only weekly date type for plotting
  TS=TS[which(TS$DATE_TYPE == "Monthly"),]
  TS=TS[,-"DATE_TYPE"]
}

# Assign names
TS$STATE = ADMIN_NAMES[match(TS$CODE, ADMIN_NAMES$Code),"UF"] 
TS$NAME = ADMIN_NAMES[match(TS$CODE, ADMIN_NAMES$Code),"Name"]

# Top states
HighIncidenceStates=c("AC","AM","AP","MA","MT","PA","RO","RR","TO")

# Count number of municipalities per state
aggregate(Name~UF, ADMIN_NAMES, FUN = length)

# Get highest incidence municipalities by state: get total cases for all years for each municipality
TS_MU=TS[which(TS$LEVEL == "MU"),]
TS_MU=TS_MU[,c("LEVEL","CODE","TYPE","CASES","STATE","NAME")]
TS_MU_AGGREGATE=aggregate(CASES~., TS_MU, FUN = sum)
# Keep only municipalities with at least 1500 vivax cases (100 cases per year * 15 years)
HighestIncidenceMU=as.character(TS_MU_AGGREGATE[which(TS_MU_AGGREGATE$TYPE == "Vivax" &
                                                        TS_MU_AGGREGATE$CASES > 1500),"NAME"])

# MCMC fitting
Candidate_MU=c("Candeias do Jamari","Itapua do Oeste",
               "Mancio Lima",
               "Alvaraes","Guajara","Uarini",
               "Caracarai",
               "Calcoene")


############################################
## Get MCMC fitting candidate municipalities

# Get data
CAND_DATA=subset(TS, LEVEL == "MU" & NAME %in% Candidate_MU & TYPE == "Vivax")
mCAND_DATA=melt(CAND_DATA, id.vars = c("CODE","DATE","TYPE","LEVEL","YEAR","POP","RATIO","STATE","NAME"))

# Plot
P1=ggplot(data = subset(mCAND_DATA,variable == "CASES"),aes(DATE, value, color = variable)) +
  stat_summary(fun.y = sum, geom = "line", size = 1.005) +
  # scale_color_manual(values = c("#3182bd","#a621ce")) +
  scale_x_date(breaks = "year", 
               date_labels = "%Y") +
  facet_wrap(~NAME) +
  labs(title = "P. vivax monthly cases and API in candidate municipalities for MCMC fitting", 
       x = "Year", y = "Cases") + 
  guides(color=guide_legend(title="Data")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
P1

P1 + geom_line(data = subset(mCAND_DATA,variable == "API"), aes(y = value*10, color = variable), size = 1.005) + 
  scale_y_continuous(sec.axis = sec_axis(~. /10, name = "API per 1000")) +
  scale_color_manual(values = c("#a621ce","#3182bd"))


# Save
# dev.copy(png, "C:/Users/nnekkab/Desktop/MCMC_Fitting/P. vivax monthly cases and API in candidate municipalities for MCMC fitting.png",
#          width = 1800, height = 800, units = "px", pointsize = 12,
#          res = 100)
# dev.off()

####################
## Get seasonalilty

# Given period, how many in a year
Frequency=12

# How many years
Subset=15

##############
## Raw data ##

# Time series
TS=CAND_DATA

# Get type
TS = TS[which(TS$TYPE == "Vivax"),]

# By level
TS = TS[which(TS$LEVEL == "MU"),]

# Get state by state time series
TS_Sum = aggregate(CASES ~ CODE+DATE, TS, sum)

# Format
TS_Formatted=by(TS, TS$CODE, FUN=function(TS) ts(TS$CASES, start = as.integer(StartYear), end = as.integer(EndYear), frequency=Frequency))  

# Trends
# TS_Municipality_Seasonal=getPV_TS_Trend(TS_Formatted, 1, 2, "Municipality")
TS_Subset=foreach(i = 1:length(TS_Formatted)) %do% {
  #################
  ## Decompose Data
  #################
  
  # Use stl method to decompose data
  TS_decomposed=stl(TS_Formatted[[i]], s.window="periodic")
  
  # Exract trends
  TS_Trend=TS_decomposed$time.series[,1]
  
  # Subset by number of years
  # TS_Subset=subset(TS_Trend, start = ((as.numeric(EndYear)-as.numeric(StartYear)+1)*Frequency)-Frequency*Subset, end=(as.numeric(EndYear)-as.numeric(StartYear)+1)*Frequency)
  
}
names(TS_Subset)=names(TS_Formatted)

# Format data and dates
TS_Subset_DF=lapply(TS_Subset, as.numeric)
TS_Subset_DF=data.frame(Date=as.yearmon(time(TS_Subset[[1]]), "%b %Y"),
                        TS_Subset_DF)
# TS_Subset_DF$Date=as.Date(format(date_decimal(TS_Subset_DF$Date), "%Y-%m-%d"))
# Melt
TS_Subset_DF_Melt=melt(TS_Subset_DF, id.vars="Date")
colnames(TS_Subset_DF_Melt)=c("Year", "Name", "Cases")
TS_Municipality_Seasonal=TS_Subset_DF_Melt

# Cleanup
TS_Municipality_Seasonal=TS_Municipality_Seasonal[order(TS_Municipality_Seasonal$Name, TS_Municipality_Seasonal$Year),]
TS_Municipality_Seasonal$Date=as.Date(TS_Municipality_Seasonal$Year)
TS_Municipality_Seasonal$YEAR=year(TS_Municipality_Seasonal$Date)
TS_Municipality_Seasonal$MONTH=month(TS_Municipality_Seasonal$Date)
TS_Municipality_Seasonal=subset(TS_Municipality_Seasonal, YEAR < 2004)
# Names
TS_Municipality_Seasonal$NAME = substr(TS_Municipality_Seasonal$Name,2,7)
TS_Municipality_Seasonal$NAME = ADMIN_NAMES[match(TS_Municipality_Seasonal$NAME, ADMIN_NAMES$Code),"Name"]
# Selet variables
names(TS_Municipality_Seasonal)
MCMC_VIVAX_SEASONAL_DATA = TS_Municipality_Seasonal[,c("Cases","YEAR","MONTH","NAME")]
colnames(MCMC_VIVAX_SEASONAL_DATA) = c("CASES_DIFF_FROM_MEAN","YEAR","MONTH","NAME")

# For plots
Lsize=1.2
Y_Scale=c(-200,200)

# Seasonality Pattern Plot by state
ggplot(data = MCMC_VIVAX_SEASONAL_DATA, aes(MONTH, CASES_DIFF_FROM_MEAN, color = NAME)) +
  geom_line(size=Lsize) +
  # facet_wrap(~Name, scales = "free_x", ncol = 1) +
  # scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  labs(title = "Seasonality Trends of P. Vivax Cases", subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
       x = "Month", y = "Seasonal Pattern") +
  guides(color=guide_legend(title="Data stratification")) +
  theme_gray() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


# Export data for these MUs
MU_MCMC_VIVAX_DATA=subset(TS, LEVEL == "MU" & NAME %in% Candidate_MU & TYPE == "Vivax")
# Save csv
write.table(MU_MCMC_VIVAX_DATA,"C:/Users/nnekkab/Desktop/MCMC_Fitting/MCMC_TS_VIVAX_DATA.csv",sep = ",",
            row.names = F)
write.table(MCMC_VIVAX_SEASONAL_DATA,"C:/Users/nnekkab/Desktop/MCMC_Fitting/MCMC_VIVAX_SEASONAL_DATA.csv",sep = ",",
            row.names = F)
