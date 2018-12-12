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


#################
## Analysis Level
#################

# Which administrative level ?
byNotification=TRUE
byResidence=FALSE
byInfection=FALSE

# Which variable(s) ?
byType=FALSE
byGender=FALSE
byAge=TRUE
byImportation=FALSE
bySetting=FALSE

# Choose time period
StartYear="2003"
EndYear="2017"

# Load state abbreviations
ADMIN_NAMES=read.csv(file = paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv"), sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)

# Melted data
Melted=TRUE


##################
## BY CASES OR API
##################

API=F
loadCleanData=F

if(!API){
  
  # Clean data or load already clean data
  if(loadCleanData){
    if(byNotification){
      if(byType){load(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byType_byNotification.RData"))}
      if(byGender){load()}
    }
    
    if(byResidence){
      if(byType){load()}
      if(byGender){load()}
    }
    
    if(byInfection){
      if(byType){load()}
      if(byGender){load()}
    }
    
  }else{
    ##############
    ## Data Upload
    ##############
    
    # Set file path
    if(envNN){
      FilePath=paste0(getwd(),"/SIVEP_clean.RData")
    }else{
      
    }
    
    # Get time series data by administrative level and by variable of interest
    if(byNotification){
      if(byType){
        TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)
        save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byType_byNotification.RData"))
        }
      if(byGender){
        TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)
        save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byGender_byNotification.RData"))
      }
      if(byAge){
        TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)
        save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byAge_byNotification.RData"))
      }
    }
    
    if(byResidence){
      if(byType){
        TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)
        save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byType_byResidence.RData"))
      }
      if(byGender){TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)}
    }
    
    if(byInfection){
      if(byType){
        TS=getDAILY_SIVEP_MALARIA_TYPE(FilePath, StartYear, EndYear, Melted)
        save(TS, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/TS_byType_byInfection.RData"))
        }
      if(byGender){TS=getDAILY_SIVEP_MALARIA_GENDER(FilePath, StartYear, EndYear, Melted)}
    }
  }
  
  
}else{
  
  ##############
  ## Data Upload
  ##############
  
  if(byNotification){
    TS_MU_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_MU_byNotification.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
    TS_UF_API=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/SIVEP_API_UF_byNotification.csv"), stringsAsFactors = F, row.names = NULL, check.names = F)
    TS=rbind(TS_UF_API, TS_MU_API)
    TS$DATE=as.Date(TS$DATE)
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
      if(byType){
        if(!API){
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byNotification/byType/")
        }else{
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byNotification/byType/")
          }
      }
      if(byGender){
        if(!API){
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byNotification/byGender/")
        }else{
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byNotification/byGender/")
        }
      }
      if(byAge){
        if(!API){
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byCases/byNotification/byAge/")
        }else{
          Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/byAPI/byNotification/byAge/")
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
options(scipen=999)

################
## General Plots
################

if(!Melted){
  # Daily number of cases, bar graph
  DailyPlot=ggplot(data = subset(TS, DATE_TYPE == "Daily" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    labs(title = paste0("Daily P. vivax and P. falciparum cases, Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species"))
  
  
  # Weekly number of cases, bar graph
  WeeklyPlot=ggplot(data = subset(TS, DATE_TYPE == "Weekly" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line", size = 1.05) +
    scale_x_date(date_breaks= "year", 
                 date_labels = "%Y") +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    labs(title = paste0("Weekly P. vivax and P. falciparum cases, Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") +
    guides(color=guide_legend(title="Plasmodium species")) + theme_light()
  
  # Monthly number of cases, line graph
  MonthlyPlot=ggplot(data = subset(TS, DATE_TYPE == "Monthly" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line", size = 1.05) +
    scale_x_date(date_breaks= "year", 
                 date_labels = "%Y") +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    labs(title = paste0("Weekly P. vivax and P. falciparum cases, Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") +
    guides(color=guide_legend(title="Plasmodium species")) + theme_light()
  
  # Yearly number of cases, line graph
  YearlyPlot=ggplot(data = subset(TS, DATE_TYPE == "Yearly" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line", size = 1.05) +
    scale_x_date(date_breaks= "year", 
                 date_labels = "%Y") +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    labs(title = paste0("Weekly P. vivax and P. falciparum cases, Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") +
    guides(color=guide_legend(title="Plasmodium species")) + theme_light()
  
  # Plot together
  grid.arrange(DailyPlot, WeeklyPlot, MonthlyPlot, YearlyPlot, nrow=2)
  
  
  #######
  ## Save
  #######
  
  if(SavePlots){
    # Plot
    grid.arrange(DailyPlot, WeeklyPlot, MonthlyPlot, YearlyPlot, nrow=2)
    
    # Save
    dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases, Brazil ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
  }
}else{
  
  if(byType){
    # Plot together (avoid double counting with level)
    TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU"), 
                           aes(DATE, CASES, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line") +
      scale_x_date(breaks = "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      facet_wrap(~DATE_TYPE, scales = "free_y") +
      labs(title = paste0("P. vivax and P. falciparum cases, Brazil ", 
                          StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
      guides(color=guide_legend(title="Plasmodium species"))
  }
  
  if(byGender){
    # Gender
    TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU" & !is.na(GENDER)), 
                           aes(DATE, CASES, color = GENDER)) +
      stat_summary(fun.y = sum, geom = "line") +
      scale_x_date(breaks = "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      facet_wrap(~DATE_TYPE, scales = "free_y") +
      labs(title = paste0("P. vivax and P. falciparum cases in Brazil by gender, ", 
                          StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
      guides(color=guide_legend(title="Gender"))
  }
  if(byAge){
    # Colors
    getColors=colorRampPalette(brewer.pal(6,"Reds"))
    Colors=getColors(length(unique(TS$AGE_CAT)))
    
    # # Age
    # TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU" & !is.na(AGE_CAT)), 
    #                        aes(DATE, CASES, color = AGE_CAT)) +
    #   stat_summary(fun.y = sum, geom = "line") +
    #   scale_x_date(breaks = "year", 
    #                date_labels = "%Y") +
    #   scale_color_manual(values = Colors) +
    #   facet_wrap(~DATE_TYPE, scales = "free_y") +
    #   labs(title = paste0("P. vivax and P. falciparum cases in Brazil by age category, ", 
    #                       StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    #   guides(color=guide_legend(title="Age category"))
    
    # Age weekly
    TS_Plot=ggplot(data = subset(TS, DATE_TYPE == "Weekly" & LEVEL == "MU" & !is.na(AGE_CAT)), 
                   aes(DATE, CASES, color = AGE_CAT)) +
      stat_summary(fun.y = sum, geom = "line", size = 1.05) +
      scale_x_date(date_breaks= "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = Colors) +
      facet_wrap(.~AGE_CAT, ncol=5) +
      labs(title = paste0("Weekly P. vivax and P. falciparum cases by age category, Brazil ", 
                          StartYear, "-", EndYear), x = "Year", y = "Number of Cases") +
      guides(color=guide_legend(title="Age category")) + theme_light() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  
  #######
  ## Save
  #######
  
  if(SavePlots){
    # Plot
    TS_CombinedPlot
    
    if(byType){
      # Save
      dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases, Brazil, ", StartYear, "-", EndYear, ".png"),
               width = 1800, height = 800, units = "px", pointsize = 12,
               res = 100)
      dev.off()
    }
    
    if(byGender){
      # Save
      dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases, Brazil by gender, ", StartYear, "-", EndYear, ".png"),
               width = 1800, height = 800, units = "px", pointsize = 12,
               res = 100)
      dev.off()
    }
    
    if(byAge){
      # Save
      dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases, Brazil by age category (facets), ", StartYear, "-", EndYear, ".png"),
               width = 1800, height = 800, units = "px", pointsize = 12,
               res = 100)
      dev.off()
    }
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
HighIncidenceStates=c("AC","AM","AP","MA","MT","PA","RO","RR","TO")

if(byType){
  # All
  TS_CombinedPlot_UF=ggplot(data = subset(TS_WEEKLY, LEVEL == "UF"), aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(.~NAME, ncol = 3) +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    labs(title = paste0("P. vivax and P. falciparum cases by state, Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Highest incidence
  TS_CombinedPlot_HighestIncidence_UF=ggplot(data = subset(TS_WEEKLY, LEVEL == "UF" & STATE %in% HighIncidenceStates), 
                                             aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    facet_wrap(.~NAME, ncol = 4) +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    labs(title = paste0("P. vivax and P. falciparum cases by state, Brazil ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
}


if(byGender){
  TS_CombinedPlot_UF=ggplot(data = subset(TS_WEEKLY, LEVEL == "UF" & !is.na(GENDER)), 
                            aes(DATE, CASES, color = GENDER)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    facet_wrap(~NAME, ncol = 5) +
    labs(title = paste0("P. vivax and P. falciparum cases in Brazil by state and gender, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Gender")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Highest incidence
  TS_CombinedPlot_HighestIncidence_UF=ggplot(data = subset(TS_WEEKLY, LEVEL == "UF" & STATE %in% HighIncidenceStates & !is.na(GENDER)), 
                                             aes(DATE, CASES, color = TYPE)) +
    stat_summary(fun.y = sum, geom = "line") +
    scale_x_date(breaks = "year", 
                 date_labels = "%Y") +
    scale_color_manual(values = c("#31a354","#3182bd")) +
    facet_wrap(~NAME, ncol = 5) +
    labs(title = paste0("P. vivax and P. falciparum cases in Brazil by state, ", 
                        StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
    guides(color=guide_legend(title="Plasmodium species")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
}



#######
## Save
#######

if(SavePlots){
  
  if(byType){
    # Plot
    TS_CombinedPlot_UF
    # Save
    dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases by state, Brazil ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Plot
    TS_CombinedPlot_HighestIncidence_UF
    # Save
    dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases by highest incidence state, Brazil ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
  }
  
  if(byGender){
    # Save
    dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum cases in Brazil by state, ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
  }
}


######################################
## Plots by Municipality (WEEKLY DATA)
######################################

# Count number of municipalities per state
aggregate(Name~UF, ADMIN_NAMES, FUN = length)

if(!byGender){
  # Get highest incidence municiaplity plots only
  HighestIncidence_MUN_Plots=foreach(i=1:length(HighIncidenceStates)) %do% {
    Plot_Name=paste0("TS_Plot_",HighIncidenceStates[i])
    assign(Plot_Name, ggplot(data = subset(TS_WEEKLY, LEVEL == "MU" & STATE == HighIncidenceStates[i]), aes(DATE, CASES, color = TYPE)) +
             stat_summary(fun.y = sum, geom = "line") +
             scale_x_date(breaks = "year", 
                          date_labels = "%Y") +
             scale_color_manual(values = c("#31a354","#3182bd")) +
             facet_wrap(~NAME) +
             labs(title = paste0("P. vivax and P. falciparum API in ",HighIncidenceStates[i]," state by municipality, ", 
                                 StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
             guides(color=guide_legend(title="Plasmodium species")) +
             theme(panel.grid.minor.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1)))
  }
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



############################
### Time series plots of API
############################

# By STATE
TS_Plot_State=ggplot(data = subset(TS, LEVEL == "UF"), aes(DATE, API, color = TYPE)) +
  stat_summary(fun.y = sum, geom = "line") +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~NAME, ncol = 3) +
  scale_color_manual(values = c("#31a354","#3182bd")) +
  labs(title = paste0("P. vivax and P. falciparum API in Brazil by state, ", 
                      StartYear, "-", EndYear), x = "Year", y = "API") + 
  guides(color=guide_legend(title="Plasmodium species")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
TS_Plot_State
# Save
dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API by state, Brazil ", StartYear, "-", EndYear, ".png"),
         width = 1800, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

HighIncidenceStates=unique(TS$STATE)

# Get highest incidence municiaplity plots only
HighestIncidence_MUN_Plots=foreach(i=1:length(HighIncidenceStates)) %do% {
  Plot_Name=paste0("TS_Plot_",HighIncidenceStates[i])
  assign(Plot_Name, ggplot(data = subset(TS, LEVEL == "MU" & STATE == HighIncidenceStates[i]), aes(DATE, API, color = TYPE)) +
           stat_summary(fun.y = sum, geom = "line") +
           scale_x_date(breaks = "year", 
                        date_labels = "%Y") +
           scale_color_manual(values = c("#31a354","#3182bd")) +
           facet_wrap(~NAME) +
           labs(title = paste0("P. vivax and P. falciparum API in ",HighIncidenceStates[i]," state by municipality, ", 
                               StartYear, "-", EndYear), x = "Year", y = "API") + 
           guides(color=guide_legend(title="Plasmodium species")) +
           theme(panel.grid.minor.x = element_blank(),
                 axis.text.x = element_text(angle = 90, hjust = 1)))
}

#######
## Save
#######


if(SavePlots){
  if(!byGender){
    # Highest incidence states
    HighIncidenceStates
    # Acre
    TS_Plot_AC
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Acre state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amazonas
    TS_Plot_AM
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Amazonas state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amapa
    TS_Plot_AP
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Amapa state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    
    # Maranhão
    TS_Plot_MA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Maranhão state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Mato Grosso
    TS_Plot_MT
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Mato Grosso state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Para
    TS_Plot_PA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Para state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Rondonia
    TS_Plot_RO
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Rondonia state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 72)
    dev.off()
    
    # Roraima
    TS_Plot_RR
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Roraima state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 72)
    dev.off()
    
    # Tocantins
    TS_Plot_TO
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API in Tocantins state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 72)
    dev.off()
    
  }else{
    # Highest incidence states
    HighIncidenceStates
    # Acre
    TS_Plot_AC
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API by gender in Acre state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amazonas
    TS_Plot_AM
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API by gender in Amazonas state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amapa
    TS_Plot_AP
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API by gender in Amapa state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    
    # Mato Grosso
    TS_Plot_MA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum API by gender in Maranhão state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Mato Grosso
    TS_Plot_MT
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Mato Grosso state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Para
    TS_Plot_PA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Para state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Rondonia
    TS_Plot_RO
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Rondonia state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 72)
    dev.off()
    
    # Roraima
    TS_Plot_RR
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Roraima state by municipality ", StartYear, "-", EndYear, "by MU.png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 72)
    dev.off()
  }
  
}
