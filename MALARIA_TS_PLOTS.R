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

if(Melted){
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


################
## General Summary Tables
################

if(!API){
  TS_UF = subset(TS, LEVEL == "UF")
  
  Table_Case_Counts = aggregate(CASES~YEAR, TS_UF, FUN = sum)
  Table_Case_Counts_Type = aggregate(CASES~YEAR+TYPE, TS_UF, FUN = sum)
  Table_Case_Counts_Type = reshape(Table_Case_Counts_Type, idvar = "YEAR", timevar = "TYPE", direction = "wide")
  Table_Case_Counts_State = aggregate(CASES~YEAR+STATE, TS_UF, FUN = sum)
  Table_Case_Counts_State = reshape(Table_Case_Counts_State, idvar = "YEAR", timevar = "STATE", direction = "wide")
  
  Table_Case_Counts = merge(Table_Case_Counts, Table_Case_Counts_Type, by = "YEAR")
  Table_Case_Counts = merge(Table_Case_Counts, Table_Case_Counts_State, by = "YEAR")
  
  # Save
  # write.csv(Table_Case_Counts, file = "Table_Case_Counts.csv", sep = ",")
}


################
## General Plots
################

if(!API){
  if(!Melted){
    # Daily number of cases, bar graph
    DailyPlot=ggplot(data = subset(TS, DATE_TYPE == "Daily" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line") +
      scale_x_date(breaks = "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Daily P. vivax and P. falciparum cases", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "Number of Cases") + 
      guides(color=guide_legend(title="Plasmodium species"))
    
    
    # Weekly number of cases, bar graph
    WeeklyPlot=ggplot(data = subset(TS, DATE_TYPE == "Weekly" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line", size = 1.05) +
      scale_x_date(date_breaks= "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Weekly P. vivax and P. falciparum cases", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "Number of Cases") + 
      guides(color=guide_legend(title="Plasmodium species")) + theme_light()
    
    # Monthly number of cases, line graph
    MonthlyPlot=ggplot(data = subset(TS, DATE_TYPE == "Monthly" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line", size = 1.05) +
      scale_x_date(date_breaks= "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Monthly P. vivax and P. falciparum cases", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "Number of Cases") + 
      guides(color=guide_legend(title="Plasmodium species")) + theme_light()
    
    # Yearly number of cases, line graph
    YearlyPlot=ggplot(data = subset(TS, DATE_TYPE == "Yearly" & LEVEL == "MU"), aes(DATE, CASES, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line", size = 1.05) +
      scale_x_date(date_breaks= "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Yearly P. vivax and P. falciparum cases", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "Number of Cases") + 
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
      TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU" & DATE_TYPE %in% c("Weekly","Monthly","Yearly")), 
                             aes(DATE, CASES, color = TYPE)) +
        stat_summary(fun.y = sum, geom = "line") +
        scale_x_date(breaks = "year", 
                     date_labels = "%Y") +
        scale_color_manual(values = c("#31a354","#3182bd")) +
        facet_wrap(~DATE_TYPE, scales = "free_y", ncol = 1) +
        labs(title = "P. vivax and P. falciparum cases", 
             subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
             x = "Year", y = "Number of Cases") + 
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
        labs(title = "P. vivax and P. falciparum cases in Brazil by gender",
             subtitle = paste0("Brazil, ", StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
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
        labs(title = "Weekly P. vivax and P. falciparum cases by age category", 
             subtitle = paste0("Brazil, ", StartYear, "-", EndYear)) +
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
}else{
  if(!Melted){
    # Daily number of cases, bar graph
    DailyPlot=ggplot(data = subset(TS, LEVEL == "MU"), aes(DATE, API, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line") +
      scale_x_date(breaks = "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Daily P. vivax and P. falciparum API", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "API") + 
      guides(color=guide_legend(title="Plasmodium species"))
    
    
    # Weekly number of cases, bar graph
    WeeklyPlot=ggplot(data = subset(TS, LEVEL == "MU"), aes(DATE, API, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line", size = 1.05) +
      scale_x_date(date_breaks= "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Weekly P. vivax and P. falciparum API", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "API") + 
      guides(color=guide_legend(title="Plasmodium species")) + theme_light()
    
    # Monthly number of cases, line graph
    MonthlyPlot=ggplot(data = subset(TS, LEVEL == "MU"), aes(DATE, API, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line", size = 1.05) +
      scale_x_date(date_breaks= "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Monthly P. vivax and P. falciparum API", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "API") + 
      guides(color=guide_legend(title="Plasmodium species")) + theme_light()
    
    # Yearly number of cases, line graph
    YearlyPlot=ggplot(data = subset(TS, LEVEL == "MU"), aes(DATE, API, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line", size = 1.05) +
      scale_x_date(date_breaks= "year", 
                   date_labels = "%Y") +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "Yearly P. vivax and P. falciparum API", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
           x = "Year", y = "API") + 
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
      dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum API, Brazil ", StartYear, "-", EndYear, ".png"),
               width = 1800, height = 800, units = "px", pointsize = 12,
               res = 100)
      dev.off()
    }
  }else{
    
    if(byType){
      # Plot together (avoid double counting with level)
      TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU" & DATE_TYPE %in% c("Weekly","Monthly","Yearly")), 
                             aes(DATE, API, color = TYPE)) +
        stat_summary(fun.y = sum, geom = "line") +
        scale_x_date(breaks = "year", 
                     date_labels = "%Y") +
        scale_color_manual(values = c("#31a354","#3182bd")) +
        facet_wrap(~DATE_TYPE, scales = "free_y", ncol = 1) +
        labs(title = "P. vivax and P. falciparum API", 
             subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
             x = "Year", y = "API") + 
        guides(color=guide_legend(title="Plasmodium species"))
    }
    
    if(byGender){
      # Gender
      TS_CombinedPlot=ggplot(data = subset(TS, LEVEL == "MU" & !is.na(GENDER)), 
                             aes(DATE, API, color = GENDER)) +
        stat_summary(fun.y = sum, geom = "line") +
        scale_x_date(breaks = "year", 
                     date_labels = "%Y") +
        scale_color_manual(values = c("#31a354","#3182bd")) +
        facet_wrap(~DATE_TYPE, scales = "free_y") +
        labs(title = "P. vivax and P. falciparum API in Brazil by gender",
             subtitle = paste0("Brazil, ", StartYear, "-", EndYear), 
             x = "Year", y = "API") + 
        guides(color=guide_legend(title="Gender"))
    }
    if(byAge){
      # Colors
      getColors=colorRampPalette(brewer.pal(6,"Reds"))
      Colors=getColors(length(unique(TS$AGE_CAT)))
      
      # Age weekly
      TS_Plot=ggplot(data = subset(TS, LEVEL == "MU" & !is.na(AGE_CAT)), 
                     aes(DATE, API, color = AGE_CAT)) +
        stat_summary(fun.y = sum, geom = "line", size = 1.05) +
        scale_x_date(date_breaks= "year", 
                     date_labels = "%Y") +
        scale_color_manual(values = Colors) +
        facet_wrap(.~AGE_CAT, ncol=5) +
        labs(title = "Weekly P. vivax and P. falciparum cases by age category", 
             subtitle = paste0("Brazil, ", StartYear, "-", EndYear)) +
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
        dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum API, Brazil, ", StartYear, "-", EndYear, ".png"),
                 width = 1800, height = 800, units = "px", pointsize = 12,
                 res = 100)
        dev.off()
      }
      
      if(byGender){
        # Save
        dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum API, Brazil by gender, ", StartYear, "-", EndYear, ".png"),
                 width = 1800, height = 800, units = "px", pointsize = 12,
                 res = 100)
        dev.off()
      }
      
      if(byAge){
        # Save
        dev.copy(png, paste0(Plot_Folder, "P. vivax and P. falciparum API, Brazil by age category (facets), ", StartYear, "-", EndYear, ".png"),
                 width = 1800, height = 800, units = "px", pointsize = 12,
                 res = 100)
        dev.off()
      }
    }
  }
}




###############################
## Plots by State 
###############################

if(!API){
  if(byType){
    # All
    TS_CombinedPlot_UF=ggplot(data = subset(TS, LEVEL == "UF"), aes(DATE, CASES, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line") +
      scale_x_date(breaks = "year", 
                   date_labels = "%Y") +
      facet_wrap(.~NAME, ncol = 3) +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = "P. vivax and P. falciparum cases by state", 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear), 
           x = "Year", y = "Number of Cases") + 
      guides(color=guide_legend(title="Plasmodium species")) +
      theme(panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    # Highest incidence
    TS_CombinedPlot_HighestIncidence_UF=ggplot(data = subset(TS, LEVEL == "UF" & STATE %in% HighIncidenceStates), 
                                               aes(DATE, CASES, color = TYPE)) +
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
  }
  if(byGender){
    TS_CombinedPlot_UF=ggplot(data = subset(TS, LEVEL == "UF" & !is.na(GENDER)), 
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
    TS_CombinedPlot_HighestIncidence_UF=ggplot(data = subset(TS, LEVEL == "UF" & STATE %in% HighIncidenceStates & !is.na(GENDER)), 
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
}else{
  if(byType){
    # All
    TS_CombinedPlot_UF=ggplot(data = subset(TS, LEVEL == "UF"), aes(DATE, API, color = TYPE)) +
      stat_summary(fun.y = sum, geom = "line") +
      scale_x_date(breaks = "year", 
                   date_labels = "%Y") +
      facet_wrap(.~NAME, ncol = 3) +
      scale_color_manual(values = c("#31a354","#3182bd")) +
      labs(title = paste("P. vivax and P. falciparum",tolower(Date_Type),"API by state"), 
           subtitle = paste0("Brazil, ", StartYear, "-", EndYear), 
           x = "Year", y = paste0(Date_Type," API")) + 
      guides(color=guide_legend(title="Plasmodium species")) +
      theme(panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    # Highest incidence
    TS_CombinedPlot_HighestIncidence_UF=ggplot(data = subset(TS, LEVEL == "UF" & STATE %in% HighIncidenceStates), 
                                               aes(DATE, CASES, color = TYPE)) +
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
  }
  if(byGender){
    TS_CombinedPlot_UF=ggplot(data = subset(TS, LEVEL == "UF" & !is.na(GENDER)), 
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
    TS_CombinedPlot_HighestIncidence_UF=ggplot(data = subset(TS, LEVEL == "UF" & STATE %in% HighIncidenceStates & !is.na(GENDER)), 
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
## Plots by Municipality 
######################################

# Get plots
if(!byGender){
  if(!API){
    # Get highest incidence municipality plots only
    HighestIncidence_MUN_Plots=foreach(i=1:length(HighIncidenceStates)) %do% {
      Plot_Name=paste0("TS_Plot_",HighIncidenceStates[i])
      assign(Plot_Name, ggplot(data = subset(TS, LEVEL == "MU" & 
                                               STATE == HighIncidenceStates[i] &
                                               NAME %in% HighestIncidenceMU), 
                               aes(DATE, CASES, color = TYPE)) +
               stat_summary(fun.y = sum, geom = "line") +
               scale_x_date(breaks = "year", 
                            date_labels = "%Y") +
               scale_color_manual(values = c("#31a354","#3182bd")) +
               facet_wrap(~NAME) +
               labs(title = paste0("P. vivax and P. falciparum cases in ",HighIncidenceStates[i]," state by municipality, ", 
                                   StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
               guides(color=guide_legend(title="Plasmodium species")) +
               theme(panel.grid.minor.x = element_blank(),
                     axis.text.x = element_text(angle = 90, hjust = 1)))
    }
  }else{
    # Get highest incidence municipality plots only
    HighestIncidence_MUN_Plots=foreach(i=1:length(HighIncidenceStates)) %do% {
      Plot_Name=paste0("TS_Plot_",HighIncidenceStates[i])
      assign(Plot_Name, ggplot(data = subset(TS, LEVEL == "MU" & 
                                               STATE == HighIncidenceStates[i] &
                                               NAME %in% HighestIncidenceMU), 
                               aes(DATE, log(API), color = TYPE)) +
               stat_summary(fun.y = sum, geom = "line") +
               scale_x_date(breaks = "year", 
                            date_labels = "%Y") +
               scale_color_manual(values = c("#31a354","#3182bd")) +
               facet_wrap(~NAME) +
               labs(title = paste0("P. vivax and P. falciparum ",tolower(Date_Type)," log API in ",HighIncidenceStates[i]," state by municipality, ", 
                                   StartYear, "-", EndYear), x = "Year", y = paste0("log(",Date_Type,"API)")) + 
               guides(color=guide_legend(title="Plasmodium species")) +
               theme(panel.grid.minor.x = element_blank(),
                     axis.text.x = element_text(angle = 90, hjust = 1)))
    }
  }
  
}else{
  ### AC ####
  TS_Plot_AC=ggplot(data = subset(TS, LEVEL == "MU" & STATE == "AC" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
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
  TS_Plot_AM=ggplot(data = subset(TS, LEVEL == "MU" & STATE == "AM" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
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
  TS_Plot_PA=ggplot(data = subset(TS, LEVEL == "MU" & STATE == "PA" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
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
  TS_Plot_RO=ggplot(data = subset(TS, LEVEL == "MU" & STATE == "RO" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
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
  TS_Plot_RR=ggplot(data = subset(TS, LEVEL == "MU" & STATE == "RR" & !is.na(GENDER)), aes(DATE, CASES, color = GENDER)) +
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
dev.copy(png, "C:/Users/nnekkab/Desktop/MCMC_Fitting/P. vivax monthly cases and API in candidate municipalities for MCMC fitting.png",
         width = 1800, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

# Export data for these MUs
MU_MCMC_VIVAX_DATA=subset(TS, LEVEL == "MU" & NAME %in% Candidate_MU & TYPE == "Vivax")
# Save csv
write.table(MU_MCMC_VIVAX_DATA,"C:/Users/nnekkab/Desktop/MCMC_Fitting/MU_MCMC_VIVAX_DATA.csv",sep = ",",
            row.names = F)
<<<<<<< HEAD

#######
## Save
#######


if(SavePlots){
  if(!byGender){

=======

#######
## Save
#######


if(SavePlots){
  if(!byGender){

>>>>>>> 11925e7350843a592658f8e5c5787d56c55d638a
    # Acre
    TS_Plot_AC
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Acre state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amazonas
    TS_Plot_AM
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Amazonas state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amapa
    TS_Plot_AP
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Amapa state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Maranhão
    TS_Plot_MA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Maranhão state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Mato Grosso
    TS_Plot_MT
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Mato Grosso state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Para
    TS_Plot_PA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Para state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Rondonia
    TS_Plot_RO
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Rondonia state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Roraima
    TS_Plot_RR
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Roraima state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Tocantins
    TS_Plot_TO
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases in Tocantins state by municipality ", StartYear, "-", EndYear, ".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 72)
    dev.off()
    
  }else{
    
    if(!API){
      # Highest incidence states
      HighIncidenceStates
      # Acre
      TS_Plot_AC
      # Save
      dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Acre state by municipality ", StartYear, "-", EndYear, "by MU.png"),
               width = 1800, height = 800, units = "px", pointsize = 12,
               res = 100)
      dev.off()
      
      # Amazonas
      TS_Plot_AM
      # Save
      dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Amazonas state by municipality ", StartYear, "-", EndYear, "by MU.png"),
               width = 1800, height = 800, units = "px", pointsize = 12,
               res = 100)
      dev.off()
      
      # Amapa
      TS_Plot_AP
      # Save
      dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Amapa state by municipality ", StartYear, "-", EndYear, "by MU.png"),
               width = 1800, height = 800, units = "px", pointsize = 12,
               res = 100)
      dev.off()
      
      
      # Mato Grosso
      TS_Plot_MA
      # Save
      dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum cases by gender in Maranhão state by municipality ", StartYear, "-", EndYear, "by MU.png"),
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
    
      }else{
    
    # Highest incidence states
    HighIncidenceStates
    # Acre
    TS_Plot_AC
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Acre state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amazonas
    TS_Plot_AM
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Amazonas state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Amapa
    TS_Plot_AP
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Amapa state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    
    # Maranhão
    TS_Plot_MA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Maranhão state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Mato Grosso
    TS_Plot_MT
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Mato Grosso state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Para
    TS_Plot_PA
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Para state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Rondonia
    TS_Plot_RO
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Rondonia state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
    # Roraima
    TS_Plot_RR
    # Save
    dev.copy(png, paste0(Plot_Folder,"P. vivax and P. falciparum ",tolower(Date_Type)," log API in Roraima state by municipality, ", 
                         StartYear, "-", EndYear,".png"),
             width = 1800, height = 800, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    }
  }
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
  labs(title = paste0("P. vivax and P. falciparum ", "API in Brazil by state, ", 
                      StartYear, "-", EndYear), x = "Year", y = paste(Date_Type,"API")) + 
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


#####################################################################################


######################
## CUMULATIVE CASES ##
######################
# 
# library(plyr)
# data(iris)
# 
# ## Ecdf over all species
# iris.all <- summarize(iris, Sepal.Length = unique(Sepal.Length), 
#                       ecdf = ecdf(Sepal.Length)(unique(Sepal.Length)) * length(Sepal.Length))
# 
# iris.species <- ddply(iris, .(Species), summarize,
#                       Sepal.Length = unique(Sepal.Length),
#                       ecdf = ecdf(Sepal.Length)(unique(Sepal.Length))*length(Sepal.Length))
# 
# ggplot(iris.all, aes(Sepal.Length, ecdf)) + geom_step()

###########
## CASES ##

# State
TS_UF=subset(TS, LEVEL ==  "UF")

# TS_AM=subset(TS_UF, STATE == "AM")
# TS_AM$csum <- ave(TS_AM$CASES, TS_AM$TYPE, FUN=cumsum)
# TS_AM$sum <- ave(TS_AM$CASES, TS_AM$TYPE, FUN=sum)
# TS_AM$csum_prop = (TS_AM$csum/TS_AM$sum)*100

# Cummulative sum (daily)
TS_UF$csum <- ave(TS_UF$CASES, TS_UF$NAME, TS_UF$TYPE, FUN=cumsum)
TS_UF$sum <- ave(TS_UF$CASES, TS_UF$TYPE, FUN=sum)
TS_UF$csum_prop = (TS_UF$csum/TS_UF$sum)*100
ggplot(data = TS_UF, aes(x = DATE, y = csum, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum cumulative cases by state, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
  guides(color=guide_legend(title="State")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Cummulative sum (monthly data) as % of total of all states per day
ggplot(data = TS_UF, aes(x = DATE, y = csum_prop, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum cumulative cases by state, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
  guides(color=guide_legend(title="State")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


####### MU

TS_MU=TS[which(TS$LEVEL == "MU"),]
TS_MU=TS_MU[,c("LEVEL","DATE","CODE","TYPE","CASES","STATE","NAME")]
TS_MU_AGGREGATE=aggregate(CASES~., TS_MU, FUN = sum)

# Cummulative sum (daily)
TS_MU$csum <- ave(TS_MU$CASES, TS_MU$NAME, TS_MU$TYPE, FUN=cumsum)
TS_MU$sum <- ave(TS_MU$CASES, TS_MU$TYPE, FUN=sum)
TS_MU$csum_prop = (TS_MU$csum/TS_MU$sum)*100
TS_MU = TS_MU[rev(order(TS_MU$csum)),]
Top5_MU = as.character(unique(TS_MU$NAME)[1:5])
  
ggplot(data = TS_MU, aes(x = DATE, y = csum, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum cumulative cases by municipality, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
  guides(color = F) +
  geom_text(data = subset(TS_MU, NAME %in% Top5_MU & DATE == "2017-12-01"), 
            aes(label = NAME, color = NAME, x = DATE, y = csum), 
            hjust = "top", vjust = -1, size = 3,
            check_overlap = T) +
  # guides(color=guide_legend(title="State")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Cummulative sum (monthly data) as % of total of all states per day
ggplot(data = TS_MU, aes(x = DATE, y = csum_prop, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum cumulative cases by state, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
  guides(color=F) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


# All years combined
TS_MU_All=TS_MU[,c("LEVEL","CODE","TYPE","CASES","STATE","NAME")]
TS_MU_AGGREGATE=aggregate(CASES~., TS_MU_All, FUN = sum)
TS_MU_AGGREGATE_V=subset(TS_MU_AGGREGATE, TYPE == "Vivax" & CASES !=0)
TS_MU_AGGREGATE_V$PROP = (TS_MU_AGGREGATE_V$CASES / sum(TS_MU_AGGREGATE_V$CASES))*100
TS_MU_AGGREGATE_V=TS_MU_AGGREGATE_V[rev(order(TS_MU_AGGREGATE_V$PROP)),]
sum(TS_MU_AGGREGATE_V[1:5,"PROP"]) # 5 MU account for 28% of total Pv cases
sum(TS_MU_AGGREGATE_V[1:10,"PROP"]) # 10 MU account for 36% of total Pv cases
sum(TS_MU_AGGREGATE_V[1:50,"PROP"]) # 50 MU account for 70% of total Pv cases
sum(TS_MU_AGGREGATE_V[1:75,"PROP"]) # 75 MU account for 81% of total Pv cases
sum(TS_MU_AGGREGATE_V[1:100,"PROP"]) # 100 MU account for 88% of total Pv cases

TS_MU_AGGREGATE_F=subset(TS_MU_AGGREGATE, TYPE == "Falciparum" & CASES !=0)
TS_MU_AGGREGATE_F$PROP = (TS_MU_AGGREGATE_F$CASES / sum(TS_MU_AGGREGATE_F$CASES))*100
TS_MU_AGGREGATE_F=TS_MU_AGGREGATE_F[rev(order(TS_MU_AGGREGATE_F$PROP)),]
sum(TS_MU_AGGREGATE_F[1:5,"PROP"]) # 5 MU account for 31% of total Pv cases
sum(TS_MU_AGGREGATE_F[1:10,"PROP"]) # 10 MU account for 41% of total Pv cases
sum(TS_MU_AGGREGATE_F[1:50,"PROP"]) # 50 MU account for 75% of total Pv cases
sum(TS_MU_AGGREGATE_F[1:75,"PROP"]) # 75 MU account for 84% of total Pv cases
sum(TS_MU_AGGREGATE_F[1:100,"PROP"]) # 100 MU account for 90% of total Pv cases

# Export data
write.table(TS_MU_AGGREGATE_V,"C:/Users/nnekkab/Desktop/MCMC_Fitting/TOTAL_VIVAX_CASES_PROP.csv",sep = ",",
            row.names = F)
write.table(TS_MU_AGGREGATE_F,"C:/Users/nnekkab/Desktop/MCMC_Fitting/TOTAL_FALCI_CASES_PROP.csv",sep = ",",
            row.names = F)

###########
## API ##

# State
TS_UF=subset(TS, LEVEL ==  "UF")

# Cummulative sum (daily)
TS_UF$csum <- ave(TS_UF$API, TS_UF$NAME, TS_UF$TYPE, FUN=cumsum)
TS_UF$sum <- ave(TS_UF$API, TS_UF$TYPE, FUN=sum)
TS_UF$csum_prop = (TS_UF$csum/TS_UF$sum)*100
ggplot(data = TS_UF, aes(x = DATE, y = csum, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum 15-year cumulative API by state, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Cummulative API") + 
  guides(color=guide_legend(title="State")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Cummulative sum (monthly data) as % of total of all states per day
ggplot(data = TS_UF, aes(x = DATE, y = csum_prop, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum cumulative API by state, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of API") + 
  guides(color=guide_legend(title="State")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


####### MU

TS_MU=TS[which(TS$LEVEL == "MU"),]
TS_MU=TS_MU[,c("LEVEL","DATE","CODE","TYPE","API","STATE","NAME")]
TS_MU_AGGREGATE=aggregate(API~., TS_MU, FUN = sum)

# Cummulative sum (daily)
TS_MU$csum <- ave(TS_MU$API, TS_MU$NAME, TS_MU$TYPE, FUN=cumsum)
TS_MU$sum <- ave(TS_MU$API, TS_MU$TYPE, FUN=sum)
TS_MU$csum_prop = (TS_MU$csum/TS_MU$sum)*100
TS_MU = TS_MU[rev(order(TS_MU$csum)),]
Top5_MU = as.character(unique(TS_MU$NAME)[1:5])

ggplot(data = TS_MU, aes(x = DATE, y = csum, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum cumulative API by municipality, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of Cases") + 
  guides(color = F) +
  geom_text(data = subset(TS_MU, NAME %in% Top5_MU & DATE == "2017-12-01"), 
            aes(label = NAME, color = NAME, x = DATE, y = csum), 
            hjust = "top", vjust = -1, size = 3,
            check_overlap = T) +
  # guides(color=guide_legend(title="State")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Cummulative sum (monthly data) as % of total of all states per day
ggplot(data = TS_MU, aes(x = DATE, y = csum_prop, color = NAME)) +
  geom_line(size = 1.1) +
  scale_x_date(breaks = "year",
               date_labels = "%Y") +
  facet_wrap(.~TYPE, ncol = 3) +
  labs(title = paste0("P. vivax and P. falciparum cumulative API by state, Brazil ", 
                      StartYear, "-", EndYear), x = "Year", y = "Number of API") + 
  guides(color=F) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


# All years combined
TS_MU_All=TS_MU[,c("LEVEL","CODE","TYPE","API","STATE","NAME")]
TS_MU_AGGREGATE=aggregate(API~., TS_MU_All, FUN = sum)
TS_MU_AGGREGATE_V=subset(TS_MU_AGGREGATE, TYPE == "Vivax" & API !=0)
TS_MU_AGGREGATE_V$PROP = (TS_MU_AGGREGATE_V$API / sum(TS_MU_AGGREGATE_V$API))*100
TS_MU_AGGREGATE_V=TS_MU_AGGREGATE_V[rev(order(TS_MU_AGGREGATE_V$PROP)),]
sum(TS_MU_AGGREGATE_V[1:5,"PROP"]) # 5 MU account for 28% of total Pv API
sum(TS_MU_AGGREGATE_V[1:10,"PROP"]) # 10 MU account for 36% of total Pv API
sum(TS_MU_AGGREGATE_V[1:50,"PROP"]) # 50 MU account for 70% of total Pv API
sum(TS_MU_AGGREGATE_V[1:75,"PROP"]) # 75 MU account for 81% of total Pv API
sum(TS_MU_AGGREGATE_V[1:100,"PROP"]) # 100 MU account for 88% of total Pv API

TS_MU_AGGREGATE_F=subset(TS_MU_AGGREGATE, TYPE == "Falciparum" & API !=0)
TS_MU_AGGREGATE_F$PROP = (TS_MU_AGGREGATE_F$API / sum(TS_MU_AGGREGATE_F$API))*100
TS_MU_AGGREGATE_F=TS_MU_AGGREGATE_F[rev(order(TS_MU_AGGREGATE_F$PROP)),]
sum(TS_MU_AGGREGATE_F[1:5,"PROP"]) # 5 MU account for 31% of total Pv API
sum(TS_MU_AGGREGATE_F[1:10,"PROP"]) # 10 MU account for 41% of total Pv API
sum(TS_MU_AGGREGATE_F[1:50,"PROP"]) # 50 MU account for 75% of total Pv API
sum(TS_MU_AGGREGATE_F[1:75,"PROP"]) # 75 MU account for 84% of total Pv API
sum(TS_MU_AGGREGATE_F[1:100,"PROP"]) # 100 MU account for 90% of total Pv API

# Export data
write.table(TS_MU_AGGREGATE_V,"C:/Users/nnekkab/Desktop/MCMC_Fitting/TOTAL_VIVAX_API_PROP.csv",sep = ",",
            row.names = F)
write.table(TS_MU_AGGREGATE_F,"C:/Users/nnekkab/Desktop/MCMC_Fitting/TOTAL_FALCI_API_PROP.csv",sep = ",",
            row.names = F)



###############################################


####################
## % Pv / Pv + Pf ##

# Get state-level data
TS_UF = subset(TS, TS$LEVEL == "UF")

# Get % Pv
sTS=subset(TS_UF, select = c("CODE","DATE","TYPE","CASES"))
sTS_Wide=reshape(sTS, idvar = c("CODE","DATE"), timevar = c("TYPE"), direction = "wide", v.names = "CASES")
sTS_Wide$VIVAX_PROP_FALCI = sTS_Wide$CASES.Vivax / (sTS_Wide$CASES.Vivax+sTS_Wide$CASES.Falciparum)
sTS_Long=reshape(sTS_Wide, idvar = c("CODE","DATE","VIVAX_PROP_FALCI"), v.names = "CASES",
                 timevar = c("TYPE"), direction = "long")
mTS=merge(TS, sTS_Long, by = c("CODE","DATE","TYPE","CASES"), all.x = TRUE)

# Clean data
TS_UF_PROP = mTS[which(mTS$TYPE == "Vivax"),]
TS_UF_PROP = TS_UF_PROP[complete.cases(TS_UF_PROP),]

# Plot
ggplot(data = subset(TS_UF_PROP, STATE != "TO"), aes(DATE, VIVAX_PROP_FALCI*100, color = NAME)) +
  geom_line(size=1) +
  facet_wrap(~NAME, scales = "free_x", ncol = 4) +
  scale_x_date(date_breaks = "year", date_labels =  "%Y") +
  labs(title = "Percent of Plasmodium vivax cases to Plasmodium falciparum cases by state", subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
       x = "Year", y = "% Pv / Pv+Pf") +
  theme_gray() +
  guides(color=guide_legend(title="State")) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Save
dev.copy(png, paste0("C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Plots/Proportion Pv to Pf/","Percent of Plasmodium vivax cases to Plasmodium falciparum cases by state ", StartYear, "-", EndYear, "by MU.png"),
         width = 1800, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()

##################
# Get MU-level data
TS_MU = subset(TS, TS$LEVEL == "MU")

# Get % Pv
sTS=subset(TS_MU, select = c("CODE","DATE","TYPE","CASES"))
sTS_Wide=reshape(sTS, idvar = c("CODE","DATE"), timevar = c("TYPE"), direction = "wide", v.names = "CASES")
sTS_Wide$VIVAX_PROP_FALCI = sTS_Wide$CASES.Vivax / (sTS_Wide$CASES.Vivax+sTS_Wide$CASES.Falciparum)
sTS_Long=reshape(sTS_Wide, idvar = c("CODE","DATE","VIVAX_PROP_FALCI"), v.names = "CASES",
                 timevar = c("TYPE"), direction = "long")
mTS=merge(TS, sTS_Long, by = c("CODE","DATE","TYPE","CASES"), all.x = TRUE)

# Clean data
TS_MU_PROP = mTS[which(mTS$TYPE == "Vivax"),]
TS_MU_PROP = TS_MU_PROP[complete.cases(TS_MU_PROP),]

# Plot
ggplot(data = subset(TS_MU_PROP, STATE != "TO"), aes(DATE, VIVAX_PROP_FALCI*100, color = NAME)) +
  geom_line(size=1) +
  facet_wrap(~STATE, scales = "free_x", ncol = 4) +
  scale_x_date(date_breaks = "year", date_labels =  "%Y") +
  labs(title = "Percent of Plasmodium vivax cases to Plasmodium falciparum cases by state", subtitle = paste0("Brazil, ", StartYear, "-", EndYear),
       x = "Year", y = "% Pv / Pv+Pf") +
  theme_gray() +
  guides(color=F) +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Save
dev.copy(png, paste0("C:/Users/nnekkab/Desktop/Malaria_Mapping_TimeSeries/Malaria_Mapping_TimeSeries_Plots/Proportion Pv to Pf/","Percent of Plasmodium vivax cases to Plasmodium falciparum cases by state ", StartYear, "-", EndYear, "by MU.png"),
         width = 1800, height = 800, units = "px", pointsize = 12,
         res = 100)
dev.off()
