###########################################################
###########################################################
##########                                       ##########
##########                                       ##########
##########      *** SIVEP DATA ANALYSIS ***      ##########
##########          *** IMPORTATION ***          ##########
##########                                       ##########
##########                                       ##########
###########################################################
###########################################################

library("data.table")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("igraph")
library("circlize")
library("devtools")
library("chorddiag")
library("RColorBrewer")
library("foreach")
library("colorspace")

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

# Plot folder
Plot_Folder=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Plots/Importations/")

# Load state abbreviations
ADMIN_NAMES=read.csv(file = paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/BRA_ADMIN_NAMES.csv"), sep = "")
ADMIN_NAMES$Code=as.character(ADMIN_NAMES$Code)


#########################################################################
############################# COUNTRY-LEVEL #############################
#########################################################################

# Load data or run
loadData = T

# For top-countries or all
byTOP_COUNTRIES = T
if(byTOP_COUNTRIES){
  # Top 10 countries including other
  Top_Countries_BR=c("BRASIL","GUIANA FRANCESA","VENEZUELA","GUIANA","PERU","BOLIVIA","SURINAME","COLOMBIA","PARAGUAI")
  Top_Countries=c("GUIANA FRANCESA","VENEZUELA","GUIANA","PERU","BOLIVIA","SURINAME","COLOMBIA","PARAGUAI")
}

# Get country-level aggregates
if(loadData){
  
  cat("Load country-level data by top countries")
  if(byTOP_COUNTRIES){
    # Load vivax tables by residence
    SIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    
    # Load vivax tables by infection
    SIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    
    # Load falciparum tables by residence
    SIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    
    # Load falciparum tables by infection
    SIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = NULL, check.names = F)
    
  }else{
    
    cat("Load country-level data by all countries")
    # Load vivax tables by residence
    SIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_RES_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
    
    # Load vivax tables by infection
    SIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_RES_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
    
    # Load falciparum tables by residence
    SIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_INF_VIVAX=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
    
    # Load falciparum tables by infection
    SIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_COUNTS.csv"), row.names = NULL, check.names = F)
    pSIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
    pwbSIVEP_PAIS_INF_FALCI=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = NULL, check.names = F)
  }

}else{
  
  ############################
  ## Run data upload script ##
  ############################
  
  cat("Run data upload script\n")
  
  # Set file path
  if(envNN){
    FilePath=paste0(getwd(),"/SIVEP_clean.RData")
  }else{
    
  }
  
  # Get SIVEP raw notification data
  load(FilePath)
  
  # Read country codes files
  PAIS_CODE=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_CODES.csv"), header = TRUE, stringsAsFactors = FALSE)
  PAIS_CODE$PAIS_CODE=as.character(PAIS_CODE$PAIS_CODE)
  
  ###############
  ## RESIDENCE ##
  
  ###########
  ## VIVAX ##
  
  # Paramters
  RES_OR_INF="PAIS_RES"
  TYPE="Vivax"
  
  # Get country level data for Pv
  list_SIVEP_PAIS_RES_VIVAX=getSIVEP_MALARIA_TYPE_COUNTRY(RES_OR_INF, TYPE)
  SIVEP_PAIS_RES_VIVAX=list_SIVEP_PAIS_RES_VIVAX[[1]]
  pSIVEP_PAIS_RES_VIVAX=list_SIVEP_PAIS_RES_VIVAX[[2]]
  pwbSIVEP_PAIS_RES_VIVAX=list_SIVEP_PAIS_RES_VIVAX[[3]]
  
  # Save tables
  if(byTOP_COUNTRIES){
    write.csv(SIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
  }else{
    write.csv(SIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_COUNTS.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_RES_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
    
  }
   
  ################
  ## FALCIPARUM ##
  
  # Paramters
  RES_OR_INF="PAIS_RES"
  TYPE="Falciparum"
  
  # Get country level data for Pv
  list_SIVEP_PAIS_RES_FALCI=getSIVEP_MALARIA_TYPE_COUNTRY(RES_OR_INF, TYPE)
  SIVEP_PAIS_RES_FALCI=list_SIVEP_PAIS_RES_FALCI[[1]]
  pSIVEP_PAIS_RES_FALCI=list_SIVEP_PAIS_RES_FALCI[[2]]
  pwbSIVEP_PAIS_RES_FALCI=list_SIVEP_PAIS_RES_FALCI[[3]]
  
  # Save tables
  if(byTOP_COUNTRIES){
    write.csv(SIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
  }else{
    write.csv(SIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_COUNTS.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_RES_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_RES_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
    
  }  
  ###############
  ## INFECTION ##
  
  ###########
  ## VIVAX ##
  
  # Paramters
  RES_OR_INF="PAIS_INF"
  TYPE="Vivax"
  
  # Get country level data for Pv
  list_SIVEP_PAIS_INF_VIVAX=getSIVEP_MALARIA_TYPE_COUNTRY(RES_OR_INF, TYPE)
  SIVEP_PAIS_INF_VIVAX=list_SIVEP_PAIS_INF_VIVAX[[1]]
  pSIVEP_PAIS_INF_VIVAX=list_SIVEP_PAIS_INF_VIVAX[[2]]
  pwbSIVEP_PAIS_INF_VIVAX=list_SIVEP_PAIS_INF_VIVAX[[3]]
  
  # Save tables
  if(byTOP_COUNTRIES){
    write.csv(SIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_VIVAX_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
  }else{
    write.csv(SIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_COUNTS.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_INF_VIVAX, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_VIVAX_CASE_PROPORTIONS.csv"), row.names = F)
  }  
  
  ################
  ## FALCIPARUM ##
  
  # Paramters
  RES_OR_INF="PAIS_INF"
  TYPE="Falciparum"
  
  # Get country level data for Pv
  list_SIVEP_PAIS_INF_FALCI=getSIVEP_MALARIA_TYPE_COUNTRY(RES_OR_INF, TYPE)
  SIVEP_PAIS_INF_FALCI=list_SIVEP_PAIS_INF_FALCI[[1]]
  pSIVEP_PAIS_INF_FALCI=list_SIVEP_PAIS_INF_FALCI[[2]]
  pwbSIVEP_PAIS_INF_FALCI=list_SIVEP_PAIS_INF_FALCI[[3]]
  
  # Save tables
  if(byTOP_COUNTRIES){
    write.csv(SIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_COUNTS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_FALCI_CASE_PROPORTIONS_byTOP_COUNTRIES.csv"), row.names = F)
  }else{
    write.csv(SIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_COUNTS.csv"), row.names = F)
    write.csv(pSIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_ALL_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
    write.csv(pwbSIVEP_PAIS_INF_FALCI, file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_INF_IMPORTED_FALCI_CASE_PROPORTIONS.csv"), row.names = F)
  }   
}



#######################
## STACKED-BOX PLOTS ##
#######################

if(RES_OR_INF == "PAIS_RES"){
  title = "Proportion of imported P. vivax cases by case residence country"
  legend_title = "Country of residence"
}else{
  title = "Proportion of imported P. vivax cases by probable infection country"
  legend_title = "Country of probable infection"
}

############
## COLORS ##

if(byTOP_COUNTRIES){
  getColors=colorRampPalette(brewer.pal(10,"Spectral"))
  Colors=rev(getColors(10))
  names(Colors)=Top_Countries
}else{
  # getColors=colorRampPalette(brewer.pal(10,"Spectral"))
  # Colors=rev(getColors(10))
  # names(Colors)=names(Colors)=c("BOLIVIA","COLOMBIA","FRANCA","GUIANA","GUIANA FRANCESA","OTHER","PARAGUAI",
  #                               "PERU","SURINAME","VENEZUELA")
}

#################
## PLOT & SAVE ##

Plot=getSTACKED_BOX_PLOT(pwbSIVEP_PAIS, "PAIS_RES")
Plot

# Save
dev.copy(png, paste0(Plot_Folder,title,".png"),
           width = 1600, height = 1000, units = "px", pointsize = 12,
           res = 100)
dev.off()


################
## CORD GRAPH ##

if(RES_OR_INF == "PAIS_RES"){
  title = "Country of residence of imported P. vivax cases,"
}else{
  title = "Country of probable infection of imported P. vivax cases,"
}

CHORD_DIAGRAMS=getCHORD_DIAGRAMS(SIVEP_PAIS, RES_OR_INF)


#######################################################################
############################# STATE-LEVEL #############################
#######################################################################

# Load data or run
loadData = T

# For top-countries or all
byTOP_STATES = T
if(byTOP_STATES){
  # Top 10 countries including other
  Top_States=c("RO","AC","AM","RR","PA","AP","TO","MA","MT")
}

# Aggregate data by state of residence and malaria type
SIVEP_UF_RES_VIVAX = df %>%
  select(DT_NOTIF, UF_NOTIF, UF_RESID, RES_EXAM) %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF) %>%
  group_by(YEAR, UF_NOTIF, UF_RESID) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, UF_NOTIF, UF_RESID, Vivax) %>%
  spread(key = YEAR, value = Vivax)


# Assign names
SIVEP_UF_RES_VIVAX$UF_NOTIF = as.character(ADMIN_NAMES[match(SIVEP_UF_RES_VIVAX$UF_NOTIF, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_RES_VIVAX$UF_RESID = as.character(ADMIN_NAMES[match(SIVEP_UF_RES_VIVAX$UF_RESID, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_RES_VIVAX[is.na(SIVEP_UF_RES_VIVAX)] = 0

unique(SIVEP_UF_RES_VIVAX$UF_NOTIF)
unique(SIVEP_UF_RES_VIVAX$UF_RESID)

# Remove if RES = 0
SIVEP_UF_RES_VIVAX=SIVEP_UF_RES_VIVAX[-which(SIVEP_UF_RES_VIVAX$UF_RESID == "0"),]

# Remove self-links
SIVEP_UF_RES_VIVAX=SIVEP_UF_RES_VIVAX[-which(SIVEP_UF_RES_VIVAX$UF_RESID == SIVEP_UF_RES_VIVAX$UF_NOTIF),]

# Get edgelist by residence
Years=2003:2018
UF_EDGELIST_RES_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("UF_EDGELIST_RES_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_UF_RES_VIVAX$UF_RESID,
                                      to = SIVEP_UF_RES_VIVAX$UF_NOTIF,
                                      values = SIVEP_UF_RES_VIVAX[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_UF_RES_VIVAX$UF_NOTIF)))),rep("gray",length(unique(SIVEP_UF_RES_VIVAX$UF_RESID))-length(unique(SIVEP_UF_RES_VIVAX$UF_NOTIF))))
names(Colors)=c(unique(SIVEP_UF_RES_VIVAX$UF_NOTIF),unique(SIVEP_UF_RES_VIVAX$UF_RESID[!SIVEP_UF_RES_VIVAX$UF_RESID %in% SIVEP_UF_RES_VIVAX$UF_NOTIF]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(UF_EDGELIST_RES_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 0,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("State of residence of reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"State of residence of reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}


##################
## UF-INF-LEVEL ##
##################

# Aggregate data by state of infection and malaria type
SIVEP_UF_INF_VIVAX = df %>%
  select(DT_NOTIF, UF_NOTIF, UF_INFEC, RES_EXAM) %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF) %>%
  group_by(YEAR, UF_NOTIF, UF_INFEC) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, UF_NOTIF, UF_INFEC, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Remove df
# rm(df)

# Assign names
SIVEP_UF_INF_VIVAX$UF_NOTIF = as.character(ADMIN_NAMES[match(SIVEP_UF_INF_VIVAX$UF_NOTIF, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_INF_VIVAX$UF_INFEC = as.character(ADMIN_NAMES[match(SIVEP_UF_INF_VIVAX$UF_INFEC, ADMIN_NAMES$Code),"UF"])
SIVEP_UF_INF_VIVAX[is.na(SIVEP_UF_INF_VIVAX)] = 0

unique(SIVEP_UF_INF_VIVAX$UF_NOTIF)
unique(SIVEP_UF_INF_VIVAX$UF_INFEC)

# Remove if INF = 0
SIVEP_UF_INF_VIVAX=SIVEP_UF_INF_VIVAX[-which(SIVEP_UF_INF_VIVAX$UF_INFEC == "0"),]

# Remove self-links
SIVEP_UF_INF_VIVAX=SIVEP_UF_INF_VIVAX[-which(SIVEP_UF_INF_VIVAX$UF_INFEC == SIVEP_UF_INF_VIVAX$UF_NOTIF),]

# Get edgelist by infection
Years=2003:2018
UF_EDGELIST_INF_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("UF_EDGELIST_INF_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_UF_INF_VIVAX$UF_INFEC,
                                      to = SIVEP_UF_INF_VIVAX$UF_NOTIF,
                                      values = SIVEP_UF_INF_VIVAX[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_UF_INF_VIVAX$UF_NOTIF)))),rep("gray",length(unique(SIVEP_UF_INF_VIVAX$UF_INFEC))-length(unique(SIVEP_UF_INF_VIVAX$UF_NOTIF))))
names(Colors)=c(unique(SIVEP_UF_INF_VIVAX$UF_NOTIF),unique(SIVEP_UF_INF_VIVAX$UF_INFEC[!SIVEP_UF_INF_VIVAX$UF_INFEC %in% SIVEP_UF_INF_VIVAX$UF_NOTIF]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(UF_EDGELIST_INF_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 0,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("State of probable infection of reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"State of probable infection of reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}



###############################################################################


############################
## Run data upload script ##
############################

cat("Run data upload script\n")

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Get SIVEP raw notification data
load(FilePath)


##################
## AC-RES-LEVEL ##
##################

# Aggregate data by country of residence and malaria type
SIVEP_MU_RES_VIVAX_AC = df %>%
  select(DT_NOTIF, UF_NOTIF, MUN_NOTI, MUN_RESI, RES_EXAM) %>% 
  filter(UF_NOTIF == "12") %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, UF_NOTIF) %>%
  group_by(YEAR, MUN_NOTI, MUN_RESI) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, MUN_NOTI, MUN_RESI, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Remove df
# rm(df)

# Assign names
SIVEP_MU_RES_VIVAX_AC$MUN_NOTI = as.character(ADMIN_NAMES[match(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_RES_VIVAX_AC$MUN_RESI = as.character(ADMIN_NAMES[match(SIVEP_MU_RES_VIVAX_AC$MUN_RESI, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_RES_VIVAX_AC[is.na(SIVEP_MU_RES_VIVAX_AC)] = 0

unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI)
unique(SIVEP_MU_RES_VIVAX_AC$MUN_RESI)

# Remove if INF = 0
SIVEP_MU_RES_VIVAX_AC=SIVEP_MU_RES_VIVAX_AC[-which(SIVEP_MU_RES_VIVAX_AC$MUN_RESI == "0"),]

# Remove self-links
SIVEP_MU_RES_VIVAX_AC=SIVEP_MU_RES_VIVAX_AC[-which(SIVEP_MU_RES_VIVAX_AC$MUN_RESI == SIVEP_MU_RES_VIVAX_AC$MUN_NOTI),]

# Get edgelist by infection
Years=2003:2018
MU_EDGELIST_RES_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("MU_EDGELIST_RES_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_MU_RES_VIVAX_AC$MUN_RESI,
                                      to = SIVEP_MU_RES_VIVAX_AC$MUN_NOTI,
                                      values = SIVEP_MU_RES_VIVAX_AC[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI)))),rep("gray",length(unique(SIVEP_MU_RES_VIVAX_AC$MUN_RESI))-length(unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI))))
names(Colors)=c(unique(SIVEP_MU_RES_VIVAX_AC$MUN_NOTI),unique(SIVEP_MU_RES_VIVAX_AC$MUN_RESI[!SIVEP_MU_RES_VIVAX_AC$MUN_RESI %in% SIVEP_MU_RES_VIVAX_AC$MUN_NOTI]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(MU_EDGELIST_RES_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Municipality of residence of Acre reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Municipality of residence of Acre reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}

##################
## AC-INF-LEVEL ##
##################

# Aggregate data by country of infection and malaria type
SIVEP_MU_INF_VIVAX_AC = df %>%
  select(DT_NOTIF, UF_NOTIF, MUN_NOTI, MUN_INFE, RES_EXAM) %>% 
  filter(UF_NOTIF == "12") %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, UF_NOTIF) %>%
  group_by(YEAR, MUN_NOTI, MUN_INFE) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, MUN_NOTI, MUN_INFE, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Remove df
# rm(df)

# Assign names
SIVEP_MU_INF_VIVAX_AC$MUN_NOTI = as.character(ADMIN_NAMES[match(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_INF_VIVAX_AC$MUN_INFE = as.character(ADMIN_NAMES[match(SIVEP_MU_INF_VIVAX_AC$MUN_INFE, ADMIN_NAMES$Code),"Name"])
SIVEP_MU_INF_VIVAX_AC[is.na(SIVEP_MU_INF_VIVAX_AC)] = 0

unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI)
unique(SIVEP_MU_INF_VIVAX_AC$MUN_INFE)

# Remove if INF = 0
SIVEP_MU_INF_VIVAX_AC=SIVEP_MU_INF_VIVAX_AC[-which(SIVEP_MU_INF_VIVAX_AC$MUN_INFE == "0"),]

# Remove self-links
SIVEP_MU_INF_VIVAX_AC=SIVEP_MU_INF_VIVAX_AC[-which(SIVEP_MU_INF_VIVAX_AC$MUN_INFE == SIVEP_MU_INF_VIVAX_AC$MUN_NOTI),]

# Get edgelist by infection
Years=2003:2018
MU_EDGELIST_INF_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist_Name=paste0("MU_EDGELIST_INF_VIVAX_",Years[i])
  Edgelist=assign(Edgelist_Name, 
                  setNames(data.frame(from = SIVEP_MU_INF_VIVAX_AC$MUN_INFE,
                                      to = SIVEP_MU_INF_VIVAX_AC$MUN_NOTI,
                                      values = SIVEP_MU_INF_VIVAX_AC[,as.character(Years[i])]),
                           c("from","to","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=c(rev(getColors(length(unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI)))),rep("gray",length(unique(SIVEP_MU_INF_VIVAX_AC$MUN_INFE))-length(unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI))))
names(Colors)=c(unique(SIVEP_MU_INF_VIVAX_AC$MUN_NOTI),unique(SIVEP_MU_INF_VIVAX_AC$MUN_INFE[!SIVEP_MU_INF_VIVAX_AC$MUN_INFE %in% SIVEP_MU_INF_VIVAX_AC$MUN_NOTI]))

# Plot 
plot.new()

par(mfrow=c(1,1))
for(i in 1:length(Years)){
  circos.clear()
  circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
  chordDiagram(MU_EDGELIST_INF_VIVAX[[i]], 
               big.gap = 10,
               grid.col = Colors, 
               directional = 1,
               self.link = F,
               annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.1),
               transparency = 0.5)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y){
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0, sector.name, facing = "clockwise", niceFacing = T, cex = 0.7, adj = c(0, 0.5))
  }, bg.border = NA)
  title(paste("Municipality of probable infection of Acre reported P. vivax cases, Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
  # Save
  dev.copy(png, paste0(Plot_Folder,"Municipality of probable infection of Acre reported P. vivax cases, Brazil ",as.character(2002+i),".png"),
           width = 1000, height = 1000, units = "px", pointsize = 12,
           res = 100)
  dev.off()
}

#################################################################


# Where are venezuelans going?


############################
## Run data upload script ##
############################

cat("Run data upload script\n")

# Set file path
if(envNN){
  FilePath=paste0(getwd(),"/SIVEP_clean.RData")
}else{
  
}

# Get SIVEP raw notification data
load(FilePath)

# Read country codes files
PAIS_CODE=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_CODES.csv"), header = TRUE, stringsAsFactors = FALSE)
PAIS_CODE$PAIS_CODE=as.character(PAIS_CODE$PAIS_CODE)


###############
## INFECTION ##

###########
## VIVAX ##

#UF
SIVEP_VENEZ_UF_INF_VIVAX = df %>%
  select(DT_NOTIF, PAIS_INF, UF_NOTIF, RES_EXAM) %>% 
  filter(PAIS_INF == "216") %>%
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, -PAIS_INF) %>%
  group_by(YEAR, UF_NOTIF) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, UF_NOTIF, Vivax) %>%
  spread(key = YEAR, value = Vivax)
#MU
SIVEP_VENEZ_MU_INF_VIVAX = df %>%
  select(DT_NOTIF, PAIS_INF, MUN_NOTI, RES_EXAM) %>% 
  filter(PAIS_INF == "216") %>%
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, -PAIS_INF) %>%
  group_by(YEAR, MUN_NOTI) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, MUN_NOTI, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Assign names
SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF = as.character(ADMIN_NAMES[match(SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF, ADMIN_NAMES$Code),"UF"])
SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF=as.factor(SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF)
SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI = as.character(ADMIN_NAMES[match(SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI, ADMIN_NAMES$Code),"Name"])
SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI=as.factor(SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI)
SIVEP_VENEZ_UF_INF_VIVAX[is.na(SIVEP_VENEZ_UF_INF_VIVAX)] = 0
SIVEP_VENEZ_MU_INF_VIVAX[is.na(SIVEP_VENEZ_MU_INF_VIVAX)] = 0

# UF: melt the data frame for plotting
mVENEZ_VIVAX_UF <- melt(SIVEP_VENEZ_UF_INF_VIVAX, id.vars='UF_NOTIF')
levels(mVENEZ_VIVAX_UF$UF_NOTIF) = levels(SIVEP_VENEZ_UF_INF_VIVAX$UF_NOTIF) 

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=rev(getColors(length(levels(mVENEZ_VIVAX_UF$UF_NOTIF))))
names(Colors)=levels(mVENEZ_VIVAX_UF$UF_NOTIF)

# Stacked UF
mVENEZ_VIVAX_UF_Plot=ggplot(mVENEZ_VIVAX_UF, aes(y=value, x=variable, fill = UF_NOTIF)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="States of notification of P. vivax cases infected in Venezuela", y="Number of cases", x="") +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Brazilian State"))

mVENEZ_VIVAX_UF_Plot

# Save
dev.copy(png, paste0(Plot_Folder,"Venezuela/States of notification of P. vivax cases infected in Venezuela, Brazil 2003-2018.png"),
         width = 1200, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

##################
## Roraima MU

# RR MUs
RR_MU=as.character(ADMIN_NAMES[which(ADMIN_NAMES$UF == "RR"),"Name"])
SIVEP_VENEZ_RR_INF_VIVAX=SIVEP_VENEZ_MU_INF_VIVAX[which(SIVEP_VENEZ_MU_INF_VIVAX$MUN_NOTI %in% RR_MU),]
SIVEP_VENEZ_RR_INF_VIVAX$MUN_NOTI=factor(as.character(SIVEP_VENEZ_RR_INF_VIVAX$MUN_NOTI))

# RR: melt the data frame for plotting
mVENEZ_VIVAX_RR <- melt(SIVEP_VENEZ_RR_INF_VIVAX, id.vars='MUN_NOTI')
levels(mVENEZ_VIVAX_RR$MUN_NOTI) = levels(SIVEP_VENEZ_RR_INF_VIVAX$MUN_NOTI) 

# Colors RR
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=rev(getColors(length(levels(mVENEZ_VIVAX_RR$MUN_NOTI))))
names(Colors)=levels(mVENEZ_VIVAX_RR$MUN_NOTI)

# Stacked RR
mVENEZ_VIVAX_RR_Plot=ggplot(mVENEZ_VIVAX_RR, aes(y=value, x=variable, fill = MUN_NOTI)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="Roraima state municipalities of notification P. vivax cases infected in Venezuela", y="Number of cases", x="") +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Roraima municipalities"))

mVENEZ_VIVAX_RR_Plot

# Save
dev.copy(png, paste0(Plot_Folder,"Venezuela/Roraima state municipalities of notification P. vivax cases infected in Venezuela, Brazil 2003-2018.png"),
         width = 1200, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

###############
## RESIDENCE ##

###########
## VIVAX ##

#UF
SIVEP_VENEZ_UF_RES_VIVAX = df %>%
  select(DT_NOTIF, PAIS_RES, UF_NOTIF, RES_EXAM) %>% 
  filter(PAIS_RES == "216") %>%
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, -PAIS_RES) %>%
  group_by(YEAR, UF_NOTIF) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, UF_NOTIF, Vivax) %>%
  spread(key = YEAR, value = Vivax)
#MU
SIVEP_VENEZ_MU_RES_VIVAX = df %>%
  select(DT_NOTIF, PAIS_RES, MUN_NOTI, RES_EXAM) %>% 
  filter(PAIS_RES == "216") %>%
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF, -PAIS_RES) %>%
  group_by(YEAR, MUN_NOTI) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, MUN_NOTI, Vivax) %>%
  spread(key = YEAR, value = Vivax)

# Assign names
SIVEP_VENEZ_UF_RES_VIVAX$UF_NOTIF = as.character(ADMIN_NAMES[match(SIVEP_VENEZ_UF_RES_VIVAX$UF_NOTIF, ADMIN_NAMES$Code),"UF"])
SIVEP_VENEZ_UF_RES_VIVAX$UF_NOTIF=as.factor(SIVEP_VENEZ_UF_RES_VIVAX$UF_NOTIF)
SIVEP_VENEZ_MU_RES_VIVAX$MUN_NOTI = as.character(ADMIN_NAMES[match(SIVEP_VENEZ_MU_RES_VIVAX$MUN_NOTI, ADMIN_NAMES$Code),"Name"])
SIVEP_VENEZ_MU_RES_VIVAX$MUN_NOTI=as.factor(SIVEP_VENEZ_MU_RES_VIVAX$MUN_NOTI)
SIVEP_VENEZ_UF_RES_VIVAX[is.na(SIVEP_VENEZ_UF_RES_VIVAX)] = 0
SIVEP_VENEZ_MU_RES_VIVAX[is.na(SIVEP_VENEZ_MU_RES_VIVAX)] = 0

# UF: melt the data frame for plotting
mVENEZ_VIVAX_UF <- melt(SIVEP_VENEZ_UF_RES_VIVAX, id.vars='UF_NOTIF')
levels(mVENEZ_VIVAX_UF$UF_NOTIF) = levels(SIVEP_VENEZ_UF_RES_VIVAX$UF_NOTIF) 

# Colors
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=rev(getColors(length(levels(mVENEZ_VIVAX_UF$UF_NOTIF))))
names(Colors)=levels(mVENEZ_VIVAX_UF$UF_NOTIF)

# Stacked UF
mVENEZ_VIVAX_UF_Plot=ggplot(mVENEZ_VIVAX_UF, aes(y=value, x=variable, fill = UF_NOTIF)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="States of notification of P. vivax cases resident of Venezuela", y="Number of cases", x="") +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Brazilian State"))

mVENEZ_VIVAX_UF_Plot

# Save
dev.copy(png, paste0(Plot_Folder,"Venezuela/States of notification of P. vivax cases resident of Venezuela, Brazil 2003-2018.png"),
         width = 1200, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()

##################
## Roraima MU

# RR MUs
RR_MU=as.character(ADMIN_NAMES[which(ADMIN_NAMES$UF == "RR"),"Name"])
SIVEP_VENEZ_RR_RES_VIVAX=SIVEP_VENEZ_MU_RES_VIVAX[which(SIVEP_VENEZ_MU_RES_VIVAX$MUN_NOTI %in% RR_MU),]
SIVEP_VENEZ_RR_RES_VIVAX$MUN_NOTI=factor(as.character(SIVEP_VENEZ_RR_RES_VIVAX$MUN_NOTI))

# RR: melt the data frame for plotting
mVENEZ_VIVAX_RR <- melt(SIVEP_VENEZ_RR_RES_VIVAX, id.vars='MUN_NOTI')
levels(mVENEZ_VIVAX_RR$MUN_NOTI) = levels(SIVEP_VENEZ_RR_RES_VIVAX$MUN_NOTI) 

# Colors RR
getColors=colorRampPalette(brewer.pal(10,"Spectral"))
Colors=rev(getColors(length(levels(mVENEZ_VIVAX_RR$MUN_NOTI))))
names(Colors)=levels(mVENEZ_VIVAX_RR$MUN_NOTI)

# Stacked RR
mVENEZ_VIVAX_RR_Plot=ggplot(mVENEZ_VIVAX_RR, aes(y=value, x=variable, fill = MUN_NOTI)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Colors,
                    labels=names(Colors)) +
  theme_minimal() +
  labs(title="Roraima state municipalities of notification P. vivax cases resident of Venezuela", y="Number of cases", x="") +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.title.y=element_text(size=12),
        legend.position="right",
        legend.title=element_text(size=12, face = "bold"))  +
  guides(fill=guide_legend(title="Roraima municipalities"))

mVENEZ_VIVAX_RR_Plot

# Save
dev.copy(png, paste0(Plot_Folder,"Venezuela/Roraima state municipalities of notification P. vivax cases resident of Venezuela, Brazil 2003-2018.png"),
         width = 1200, height = 1000, units = "px", pointsize = 12,
         res = 100)
dev.off()
