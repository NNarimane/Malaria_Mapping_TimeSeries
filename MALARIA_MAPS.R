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
library("tools")

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

# Select data
if(!API){
  # Keep only weekly date type for plotting
  TS=TS[which(TS$DATE_TYPE == "Monthly"),]
  TS=TS[,-"DATE_TYPE"]
}

# Assign names
TS$STATE = ADMIN_NAMES[match(TS$CODE, ADMIN_NAMES$Code),"UF"] 
TS$NAME = ADMIN_NAMES[match(TS$CODE, ADMIN_NAMES$Code),"Name"]

###################
### Map Shape Files
###################

#################
## State-Level ##

# State-level shape files
BRA_SHP_UF=getShp(country = "Brazil", admin_level = "admin1", format = "df")

# Get function
getSTATE_MAPS=function(Level, Type, Measure, title, breaks, labels, Colors, fill_label){
  # Select data to plot
  TS_UF=TS[which(TS$LEVEL == Level & TS$TYPE == Type),]
  if(Measure == "CASES"){
    TS_UF$YEAR=year(TS_UF$DATE)
    TS_UF=subset(TS_UF, select = -c(DATE))
  }else{
    TS_UF$YEAR=year(TS_UF$DATE)
    TS_UF=subset(TS_UF, select = -c(CASES,POP,DATE))
  }
  
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
                         measure.vars = list(paste0(Measure,".",seq(2003,2017,1))),
                         variable.name = "YEAR", 
                         value.name = Measure,
                         id.vars = 1:23)
  
  # Create population categorical variable
  if(Measure == "CASES"){
    mBRA_SHP_UF_SIVEP[is.na(mBRA_SHP_UF_SIVEP$CASES),"CASES"] = 0
    mBRA_SHP_UF_SIVEP$CAT <- cut(as.numeric(mBRA_SHP_UF_SIVEP$CASES), 
                                 breaks = breaks, labels = labels)
  }else{
    mBRA_SHP_UF_SIVEP[is.na(mBRA_SHP_UF_SIVEP$API),"API"] = 0
    mBRA_SHP_UF_SIVEP$CAT <- cut(as.numeric(mBRA_SHP_UF_SIVEP$API), 
                                 breaks = breaks, labels = labels)
  }
  
  # Fix Year labels
  levels(mBRA_SHP_UF_SIVEP$YEAR)=as.character(seq(2003,2017,1))
  
  # Plot
  PLOT=ggplot(mBRA_SHP_UF_SIVEP) +
    aes(long, lat, group=group, fill = CAT) +
    scale_fill_manual(values = Colors) +
    coord_equal() +
    geom_polygon(colour = "black", size = 0.5) +
    facet_wrap(facets= YEAR ~.) +
    labs(title = title, fill = fill_label) +  
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  return(PLOT)
}

#################
## VIVAX CASES ##

# Set break points
breaks=c(-Inf, 0, 500, 1000, 10000, seq(25000,225000,25000))
labels=c("0", paste("<", breaks[3:length(breaks)]))

# Colors
getColors=colorRampPalette(brewer.pal(9,"Blues"))
Colors=c("gray85", getColors(length(labels)-1))

# Title
if(byNotification){title="Plasmodium vivax notified cases, Brazil 2003-2017"}
if(byResidence){title="Plasmodium vivax cases by residence, Brazil 2003-2017"}
if(byInfection){title="Plasmodium vivax cases by probable infection, Brazil 2003-2017"}
fill_label="Cases"

# Plot CASES
VIVAX_CASES_MAP=getSTATE_MAPS("UF", "Vivax", "CASES", title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP                                                                                     

# Save
dev.copy(png, paste0(Plot_Folder,title, ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

###############
## VIVAX API ##

# Set break points
breaks=c(-Inf, 0, 10, 50, 100, Inf)
labels=c("0", paste("<", breaks[3:(length(breaks)-1)]), paste(">", breaks[(length(breaks)-1)]))

# Colors
getColors=colorRampPalette(brewer.pal(6,"Blues"))
Colors=c("gray85", getColors(length(labels)-1))

# Title
title="Plasmodium vivax Annual Parasite Index (API), Brazil 2003-2017"
fill_label="API"

# Plot CASES
VIVAX_API_MAP=getSTATE_MAPS("UF", "Vivax", "API", title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP                                                                                     

# Save
dev.copy(png, paste0(Plot_Folder,title, ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

######################
## FALCIPARUM CASES ##

# Set break points
breaks=c(-Inf, 0, 500, 1000, 10000, seq(25000,225000,25000))
labels=c("0", paste("<", breaks[3:length(breaks)]))

# Colors
getColors=colorRampPalette(brewer.pal(9,"Greens"))
Colors=c("gray85", getColors(length(labels)-1))

# Title
if(byNotification){title="Plasmodium faciparum notified cases, Brazil 2003-2017"}
if(byResidence){title="Plasmodium faciparum cases by residence, Brazil 2003-2017"}
if(byInfection){title="Plasmodium faciparum cases by probable infection, Brazil 2003-2017"}
fill_label="Cases"

# Plot CASES
FALCI_CASES_MAP=getSTATE_MAPS("UF", "Falciparum", "CASES", title, breaks, labels, Colors, fill_label)
FALCI_CASES_MAP                                                                                     

# Save
dev.copy(png, paste0(Plot_Folder,title, ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

####################
## FALCIPARUM API ##

# Set break points
breaks=c(-Inf, 0, 10, 50, 100, Inf)
labels=c("0", paste("<", breaks[3:(length(breaks)-1)]), paste(">", breaks[(length(breaks)-1)]))

# Colors
getColors=colorRampPalette(brewer.pal(6,"Greens"))
Colors=c("gray85", getColors(length(labels)-1))

# Title
title="Plasmodium falciparum Annual Parasite Index (API), Brazil 2003-2017"
fill_label="API"

# Plot CASES
FALCI_API_MAP=getSTATE_MAPS("UF", "Falciparum", "API", title, breaks, labels, Colors, fill_label)
FALCI_API_MAP                                                                                     

# Save
dev.copy(png, paste0(Plot_Folder,title, ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()


#########################################################################################

########################
## Municipality-Level ##

# MUNI-level shape files
BRA_SHP_MU=getShp(country = "Brazil", admin_level = "admin2", format = "df")

Level="MU"
Type="Vivax"
Measure="Cases"

# Get function
getMUNI_MAPS=function(Level, Type, Measure, Year, title, breaks, labels, Colors, fill_label){
  # Select data to plot
  TS_MU=TS[which(TS$LEVEL == Level & TS$TYPE == Type),]
  if(Measure == "CASES"){
    TS_MU$YEAR=year(TS_MU$DATE)
    TS_MU=subset(TS_MU, select = -c(DATE))
    # Aggregate data
    TS_MU = aggregate(CASES~., TS_MU, FUN = sum)
  }else{
    TS_MU$YEAR=year(TS_MU$DATE)
    TS_MU=subset(TS_MU, select = -c(CASES,POP,DATE,RATIO))
    # Aggregate data
    TS_MU = aggregate(API~., TS_MU, FUN = sum)
  }
  
  # Make TS data wide by year and by type
  wTS_MU=reshape(TS_MU, idvar = c("LEVEL","CODE","TYPE","STATE","NAME"), 
                 timevar = "YEAR", direction = "wide")
  
  # Step 1: make names into character temporarily
  BRA_SHP_MU$name_2=as.character(BRA_SHP_MU$name_2)
  
  # Step 2: merge (left_join will help keep in order)
  BRA_SHP_MU_SIVEP=left_join(BRA_SHP_MU, wTS_MU, by = c("name_2" = "NAME"))
  BRA_SHP_MU_SIVEP$name_2=factor(BRA_SHP_MU_SIVEP$name_2)
  
  # Select year to map
  mBRA_SHP_MU_SIVEP=subset(BRA_SHP_MU_SIVEP, select = c(colnames(BRA_SHP_MU_SIVEP)[1:27], paste0(Measure,".",Year)))
  names(mBRA_SHP_MU_SIVEP)[names(mBRA_SHP_MU_SIVEP)==paste0(Measure,".",Year)] <- Measure
    
  # Create population categorical variable
  if(Measure == "CASES"){
    mBRA_SHP_MU_SIVEP[is.na(mBRA_SHP_MU_SIVEP$CASES),"CASES"] = 0
    mBRA_SHP_MU_SIVEP$CAT <- cut(as.numeric(mBRA_SHP_MU_SIVEP$CASES), 
                                 breaks = breaks, labels = labels)
  }else{
    mBRA_SHP_MU_SIVEP[is.na(mBRA_SHP_MU_SIVEP$API),"API"] = 0
    mBRA_SHP_MU_SIVEP$CAT <- cut(as.numeric(mBRA_SHP_MU_SIVEP$API), 
                                 breaks = breaks, labels = labels)
  }
  
  # Plot
  PLOT=ggplot(data=mBRA_SHP_MU_SIVEP) +
    geom_polygon(color=NA, aes(long, lat, group=group, fill=CAT)) +
    scale_fill_manual(values = Colors) +
    coord_equal() +
    theme_minimal() + 
    labs(title = title, fill = fill_label) +  
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  return(PLOT)
}

#################
## VIVAX CASES ##

# Set break points
# breaks=c(-Inf, 0, 500, 1000, seq(10000,80000,10000), Inf)
# labels=factor(c("0", paste("<", breaks[3:(length(breaks)-1)]), paste(">", breaks[(length(breaks)-1)])))

breaks=c(-Inf, 1, 100, 500, 1000, 5000, 10000, 20000, 30000)
labels=factor(c("0", "1-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-20000", "20000-30000"))

# Colors
getColors=colorRampPalette(brewer.pal(6,"Blues"))
Colors=c("gray85", getColors(length(labels)-1))
names(Colors)=labels

# Title
# title="Plasmodium vivax notified cases, Brazil 2003-2017"
fill_label="Cases"

#################
## Plot per  year

# Years
Years=as.character(seq(StartYear, EndYear,1))

VIVAX_CASES_MAPS=foreach(i=1:length(Years)) %do% {
  Plot_Name=paste0("VIVAX_CASES_MAP_",Years[i])
  assign(Plot_Name, getMUNI_MAPS("MU", "Vivax", "CASES", Years[i], title = paste("Plasmodium vivax notified cases, Brazil", Years[i]), breaks, labels, Colors, fill_label))
}

###############

# Plot CASES
Year="2003"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2003=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2003
rm(VIVAX_CASES_MAP_2003)

# Plot CASES
Year="2004"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2004=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2004  
rm(VIVAX_CASES_MAP_2004)

# Plot CASES
Year="2005"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2005=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2005  
rm(VIVAX_CASES_MAP_2005)

# Plot CASES
Year="2006"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2006=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2006  
rm(VIVAX_CASES_MAP_2006)

# Plot CASES
Year="2007"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2007=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2007  
rm(VIVAX_CASES_MAP_2007)

# Plot CASES
Year="2008"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2008=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2008  
rm(VIVAX_CASES_MAP_2008)

# Plot CASES
Year="2009"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2009=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2009  
rm(VIVAX_CASES_MAP_2009)

# Plot CASES
Year="2010"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2010=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2010  
rm(VIVAX_CASES_MAP_2010)

# Plot CASES
Year="2011"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2011=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2011  
rm(VIVAX_CASES_MAP_2011)

# Plot CASES
Year="2012"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2012=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2012  
rm(VIVAX_CASES_MAP_2012)

# Plot CASES
Year="2013"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2013=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2013  
rm(VIVAX_CASES_MAP_2013)

# Plot CASES
Year="2014"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2014=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2014  
rm(VIVAX_CASES_MAP_2014)

# Plot CASES
Year="2015"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2015=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2015  
rm(VIVAX_CASES_MAP_2015)

# Plot CASES
Year="2016"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2016=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2016  
rm(VIVAX_CASES_MAP_2016)

# Plot CASES
Year="2017"
title = paste("Plasmodium vivax notified cases, Brazil", Year)
VIVAX_CASES_MAP_2017=getMUNI_MAPS("MU", "Vivax", "CASES", Year, title, breaks, labels, Colors, fill_label)
VIVAX_CASES_MAP_2017  
rm(VIVAX_CASES_MAP_2017)


# Save
dev.copy(png, paste0(Plot_Folder,paste("Plasmodium vivax notified cases by municipality, Brazil", Year), ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

###############
## VIVAX API ##

# Set break points
breaks=c(-Inf, 0, 10, 50, 100, 500, 1000, Inf)
labels=c("0", paste("<", breaks[3:(length(breaks)-1)]), paste(">", breaks[(length(breaks)-1)]))

# Colors
getColors=colorRampPalette(brewer.pal(6,"Blues"))
Colors=c("gray85", getColors(length(labels)-1))
names(Colors)=labels

# Title
# title="Plasmodium vivax notified cases, Brazil 2003-2017"
fill_label="API"

#################
## Plot per  year

# Years
Years=as.character(seq(StartYear, EndYear,1))

VIVAX_API_MAPS=foreach(i=1:length(Years)) %do% {
  Plot_Name=paste0("VIVAX_API_MAP_",Years[i])
  assign(Plot_Name, getMUNI_MAPS("MU", "Vivax", "API", Years[i], title = paste("Plasmodium vivax API, Brazil", Years[i]), breaks, labels, Colors, fill_label))
}

###############

# Plot API
Year="2003"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2003=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2003
rm(VIVAX_API_MAP_2003)

# Plot API
Year="2004"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2004=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2004  
rm(VIVAX_API_MAP_2004)

# Plot API
Year="2005"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2005=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2005  
rm(VIVAX_API_MAP_2005)

# Plot API
Year="2006"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2006=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2006  
rm(VIVAX_API_MAP_2006)

# Plot API
Year="2007"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2007=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2007  
rm(VIVAX_API_MAP_2007)

# Plot API
Year="2008"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2008=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2008  
rm(VIVAX_API_MAP_2008)

# Plot API
Year="2009"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2009=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2009  
rm(VIVAX_API_MAP_2009)

# Plot API
Year="2010"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2010=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2010  
rm(VIVAX_API_MAP_2010)

# Plot API
Year="2011"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2011=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2011  
rm(VIVAX_API_MAP_2011)

# Plot API
Year="2012"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2012=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2012  
rm(VIVAX_API_MAP_2012)

# Plot API
Year="2013"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2013=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2013  
rm(VIVAX_API_MAP_2013)

# Plot API
Year="2014"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2014=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2014  
rm(VIVAX_API_MAP_2014)

# Plot API
Year="2015"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2015=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2015  
rm(VIVAX_API_MAP_2015)

# Plot API
Year="2016"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2016=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2016  
rm(VIVAX_API_MAP_2016)

# Plot API
Year="2017"
title = paste("Plasmodium vivax API, Brazil", Year)
VIVAX_API_MAP_2017=getMUNI_MAPS("MU", "Vivax", "API", Year, title, breaks, labels, Colors, fill_label)
VIVAX_API_MAP_2017  
rm(VIVAX_API_MAP_2017)


# Save
dev.copy(png, paste0(Plot_Folder,paste("Plasmodium vivax API by municipality, Brazil", Year), ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

######################
## FALCIPARUM CASES ##

# Set break points
breaks=c(-Inf, 0, 500, 1000, 10000, seq(25000,225000,25000))
labels=c("0", paste("<", breaks[3:length(breaks)]))

# Colors
getColors=colorRampPalette(brewer.pal(9,"Greens"))
Colors=c("gray85", getColors(length(labels)-1))

fill_label="Cases"

# Plot per  year
Years=as.character(seq(StartYear, EndYear,1))
FALCI_API_MAPS=foreach(i=1:length(Years)) %do% {
  # Save
  Year=Years[i]
  title = paste("Plasmodium falciparum notified cases by municipality, Brazil", Year)
  FALCI_CASES_MAP=getMUNI_MAPS("MU", "Falciparum", "CASES", Year, title, breaks, labels, Colors, fill_label)
  FALCI_CASES_MAP
  dev.copy(png, paste0(Plot_Folder,title, ".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  rm(FALCI_CASES_MAP)
}

# Save
Year="2017"
title = paste("Plasmodium falciparum notified cases by municipality, Brazil", Year)
FALCI_CASES_MAP=getMUNI_MAPS("MU", "Falciparum", "CASES", Year, title, breaks, labels, Colors, fill_label)
FALCI_CASES_MAP
dev.copy(png, paste0(Plot_Folder,title, ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()
rm(FALCI_CASES_MAP)


####################
## FALCIPARUM API ##

# Set break points
breaks=c(-Inf, 0, 10, 50, 100, Inf)
labels=c("0", paste("<", breaks[3:(length(breaks)-1)]), paste(">", breaks[(length(breaks)-1)]))

# Colors
getColors=colorRampPalette(brewer.pal(6,"Greens"))
Colors=c("gray85", getColors(length(labels)-1))

# Title
# title="Plasmodium falciparum Annual Parasite Index (API), Brazil 2003-2017"
fill_label="API"

# # Plot CASES
# FALCI_API_MAP=getSTATE_MAPS("MU", "Falciparum", "API", title, breaks, labels, Colors, fill_label)
# FALCI_API_MAP                                                                                     
# 
# # Save
# dev.copy(png, paste0(Plot_Folder,title, ".png"),
#          width = 1000, height = 1000, units = "px", pointsize = 12,
#          res = 100)
# dev.off()

# Save
Year="2017"
title = paste("Plasmodium falciparum API by municipality, Brazil", Year)
FALCI_API_MAP=getMUNI_MAPS("MU", "Falciparum", "API", Year, title, breaks, labels, Colors, fill_label)
FALCI_API_MAP
dev.copy(png, paste0(Plot_Folder,title, ".png"),
         width = 1000, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()
rm(FALCI_API_MAP)

#########################################################

# Mapping proportion of total cases
TS_MU_AGGREGATE_V=read.csv("C:/Users/nnekkab/Desktop/MCMC_Fitting/TOTAL_VIVAX_CASES_PROP.csv",sep = ",")
TS_MU_AGGREGATE_F=read.csv("C:/Users/nnekkab/Desktop/MCMC_Fitting/TOTAL_FALCI_CASES_PROP.csv",sep = ",")

#  VIVAX
TS_MU_AGGREGATE_V = TS_MU_AGGREGATE_V[,c("NAME","CASES","PROP")]
# Step 1: make names into character temporarily
BRA_SHP_MU$name_2=as.character(BRA_SHP_MU$name_2)

# Step 2: merge (left_join will help keep in order)
BRA_SHP_MU_SIVEP=left_join(BRA_SHP_MU, TS_MU_AGGREGATE_V, by = c("name_2" = "NAME"))
BRA_SHP_MU_SIVEP$name_2=factor(BRA_SHP_MU_SIVEP$name_2)

# Plot
ggplot(data=BRA_SHP_MU_SIVEP) +
  geom_polygon(color=NA, aes(long, lat, group=group, fill=CASES)) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  coord_equal() +
  theme_minimal() + 
  labs(title = "Total Plasmodium vivax cases in Brazil from 2003-2017", fill = "Cases") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot(data=BRA_SHP_MU_SIVEP) +
  geom_polygon(color=NA, aes(long, lat, group=group, fill=PROP)) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  coord_equal() +
  theme_minimal() + 
  labs(title = "Proportion of Plasmodium vivax cases in Brazil from 2003-2017", fill = "%") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



################################################################################################

###############################
## Mapping Indigeneous lands ##
###############################

library("rgdal")
library("sf")
library("maptools")
library("stringr")
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library("plyr")
library("raster")

# Load state abbreviations
ADMIN_NAMES=read.csv(file = "C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv", sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)

# Read shape file
Shape_File="C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Data/IBGE/Indigenous/ti_sirgas/ti_sirgas2000.shp"
# INDI_SHP=readOGR(dsn = Shape_File, use_iconv = T)
INDI_SHP=shapefile(Shape_File, use_iconv = T, encoding = "UTF-8")
# Plot
# plot(INDI_SHP)

# Codes
INDI_SHP$terrai_cod
# Padding codes to length 5
TI_CODES=data.frame(INDI_SHP[,c("gid","terrai_nom","terrai_cod","uf_sigla")])
# TI_CODES$terrai_nom=as.character(TI_CODES$terrai_nom)
# TI_CODES$terrai_nom=iconv(TI_CODES$terrai_nom, from = "UTF-8",to = "latin2")
# TI_CODES$terrai_cod=as.character(TI_CODES$terrai_cod)
# Add state codes
# TI_CODES$uf_sigla=as.character(TI_CODES$uf_sigla)

# Fix UF
ADMIN_NAMES_UF=ADMIN_NAMES[which(ADMIN_NAMES$Level == "UF"),]
TI_CODES$UF1=substr(TI_CODES$uf_sigla,1,2)
TI_CODES$UF2=substr(TI_CODES$uf_sigla,4,5)
TI_CODES$UF3=substr(TI_CODES$uf_sigla,7,8)
TI_CODES$UF1_CODE = ADMIN_NAMES_UF[match(TI_CODES$UF1, ADMIN_NAMES_UF$UF),"Code"] 
TI_CODES$UF2_CODE = ADMIN_NAMES_UF[match(TI_CODES$UF2, ADMIN_NAMES_UF$UF),"Code"] 
TI_CODES$UF3_CODE = ADMIN_NAMES_UF[match(TI_CODES$UF3, ADMIN_NAMES_UF$UF),"Code"] 
# TI_CODES$TI_CODE=paste0(TI_CODES$UF_CODE, TI_CODES$terrai_cod)

# TI code names
TI_CODES$NAMES=TI_CODES$terrai_nom
TI_CODES$NAMES=str_replace_all(TI_CODES$NAMES, "[[:punct:]]", "")
TI_CODES$NAMES=iconv(TI_CODES$NAMES,from="UTF-8",to="ASCII//TRANSLIT")
TI_CODES$NAMES=gsub(" ", "", TI_CODES$NAMES, fixed = TRUE)
TI_CODES$NAMES=gsub(" ", "", TI_CODES$NAMES, fixed = TRUE)
# TI_CODES$NAMES=TI_CODES$NAMES[order(TI_CODES$NAMES)]

# Match shape file to population by name
INDI_POP_File="C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Data/IBGE/Indigenous/INDI_POP_2010.csv"
INDI_POP=read.csv(file = INDI_POP_File, sep = ",")
# Seperate UF
INDI_POP$UF=gsub("[\\(\\)]", "", regmatches(INDI_POP$NAME, gregexpr("\\(.*?\\)", INDI_POP$NAME)))
INDI_POP[which(INDI_POP$UF == "character0"),"UF"] = NA
# Clean names
INDI_POP$NAME = as.character(INDI_POP$NAME)
TI_NAMES=INDI_POP[which(INDI_POP$LEVEL == "TI"),"NAME"]
TI_NAMES = substr(TI_NAMES,1,nchar(TI_NAMES)-5)
INDI_POP[which(INDI_POP$LEVEL == "TI"),"NAME"] = TI_NAMES
# Remove accents in names and space and make lowercase
TI_NAMES=str_replace_all(TI_NAMES, "[[:punct:]]", "")
TI_NAMES=iconv(TI_NAMES,from="UTF-8",to="ASCII//TRANSLIT")
TI_NAMES=gsub(" ", "", TI_NAMES, fixed = TRUE)
TI_NAMES=TI_NAMES[order(TI_NAMES)]

# Check matches
mTERRA_INDI_NAMES <- paste(TI_CODES$NAMES, collapse="|")
table(grepl(mTERRA_INDI_NAMES, TI_NAMES, ignore.case = TRUE)) # 14F, 487T
# Get non-matches and check
MATCHED_NAMES=grepl(mTERRA_INDI_NAMES, TI_NAMES, ignore.case = TRUE)
NON_MATCHED_NAMES=TI_NAMES[!MATCHED_NAMES]
NON_MATCHED_NAMES
# Add new corrections
TI_CODES$NAME_FIXED=TI_CODES$NAMES
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "BoaVistaAM")] = "BoaVista"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "BoaVistaPR")] = "BoaVista"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "Erikpatsa")] = "Erikbaktsa"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "Geripanco")] = "Jeripanco"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "KampadoRioAmonia")] = "KampadoRioAmonea"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "PanambiLagoaRica")] = "Panambi"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "SaoDomingosMT")] = "SaoDomingos"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "SaoFranciscodoCanimari")] = "SaoFranciscodoCanamari"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "SaoMarcosMT")] = "SaoMarcos"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "SaoMarcosRR")] = "SaoMarcos"
TI_CODES$NAME_FIXED[which(TI_CODES$NAME_FIXED == "TikunadeSantoAntonio")] = "TikunaSantoAntonio"
# Check matches again
mTERRA_INDI_NAMES_FIXED <- paste(TI_CODES$NAME_FIXED, collapse="|")
table(grepl(mTERRA_INDI_NAMES_FIXED, TI_NAMES, ignore.case = TRUE)) # 3F, 498F

# Add names for indi pop merge
INDI_POP$NAMES_MERGE=ifelse(INDI_POP$LEVEL == "TI",INDI_POP$NAME,NA)
INDI_POP$NAMES_MERGE=str_replace_all(INDI_POP$NAMES_MERGE, "[[:punct:]]", "")
INDI_POP$NAMES_MERGE=iconv(INDI_POP$NAMES_MERGE,from="UTF-8",to="ASCII//TRANSLIT")
INDI_POP$NAMES_MERGE=gsub(" ", "", INDI_POP$NAMES_MERGE, fixed = TRUE)

# Merge
TI_MERGE=merge(TI_CODES, INDI_POP, by.x = c("NAME_FIXED","UF1"), by.y=c("NAMES_MERGE","UF"), all.x=T)

# Set break points
breaks=c(-Inf, 0, 100, 500, 1000, 5000, 10000, 15000, Inf)
labels=c("0", paste("<", breaks[3:(length(breaks)-1)]), paste(">", breaks[(length(breaks)-1)]))

# Add categories# Categories
TI_MERGE[is.na(TI_MERGE$RES_TOTAL_INDI_LAND),"RES_TOTAL_INDI_LAND"] = 0
TI_MERGE$RES_CAT <- cut(as.numeric(TI_MERGE$RES_TOTAL_INDI_LAND), 
                        breaks = breaks, labels = labels)

# Colors
getColors=colorRampPalette(brewer.pal(6,"Blues"))
Colors=c("gray85", getColors(length(labels)-1))
names(Colors)=labels
TI_MERGE$RES_CAT_COL=TI_MERGE$RES_CAT
levels(TI_MERGE$RES_CAT_COL) = unname(Colors)
TI_MERGE$RES_CAT_COL=as.character(TI_MERGE$RES_CAT_COL)

#################
# Merge with plot

# Select variables to merge
TI_MERGE_SELECT=TI_MERGE[,c("gid","RES_TOTAL_INDI_LAND",
                            "RES_CAT","RES_CAT_COL")]
# TI_MERGE_SELECT$terrai_cod=as.integer(TI_MERGE_SELECT$terrai_cod)

# Order by name
# TI_MERGE_SELECT=TI_MERGE_SELECT[match(INDI_SHP$terrai_cod, TI_MERGE_SELECT$terrai_cod),]

# Merge by left join
# INDI_SHP=merge(INDI_SHP, TI_MERGE_SELECT, by = "gid", duplicateGeoms = T)
# INDI_SHP=inner_join(INDI_SHP, TI_MERGE_SELECT, by = "gid")
INDI_SHP$RES_CAT=INDI_SHP$gid
INDI_SHP$RES_CAT_COL=INDI_SHP$gid
INDI_SHP$RES_CAT_COL[INDI_SHP$RES_CAT %in% TI_MERGE_SELECT$gid] <- TI_MERGE_SELECT$RES_CAT_COL

# Labels
title="Resident population of indigeneous lands"
fill_label="Population"


######
# Plot

plot(INDI_SHP)

ggplot(data=INDI_SHP) +
  geom_polygon(color="black", fill=NA, aes(long, lat, group=group)) +
  coord_equal() +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# library(rgdal)
# library(raster)
# plot(subs2, col='blue')
# plot(subs1, add=TRUE, col='red')
# subs_union <- union(subs1, subs2)



