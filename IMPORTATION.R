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

library("igraph")
library("circlize")
# library("devtools")
# devtools::install_github("mattflor/chorddiag")
library("chorddiag")
library("RColorBrewer")
library("foreach")

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

# Select variables
# SIVEP = subset(df, select = c("DT_NOTIF",
#                               "PAIS_RESID","PAIS_INFE",
#                               "UF_NOTI","UF_RESID","UF_INFE",
#                               "MUNI_NOTI","MUNI_RESID","MUNI_INFE",
#                               "RES_EXAM"))
# rm(df)
# 
# SIVEP_PAIS = SIVEP %>% 
#   group_by(DT_NOTIF, PAIS_RESID, PAIS_INFE) %>%
#   spread(RES_EXAM, n, fill = 0) %>%
#   rename(FALCI = "F", FV = "F+V", VIVAX = "V") %>%
#   mutate(Falciparum = FALCI + FV) %>%
#   mutate(Vivax = VIVAX + FV)


###################
## COUNTRY-LEVEL ##
###################

# Read country codes files
PAIS_CODE=read.csv(file=paste0(getwd(),"/Malaria_Mapping_TimeSeries_Data/COUNTRY_CODES.csv"), header = TRUE, stringsAsFactors = FALSE)
PAIS_CODE$PAIS_CODE=as.character(PAIS_CODE$PAIS_CODE)

# Aggregate data by country of residence and malaria type
SIVEP_PAIS_RES = df %>%
  select(DT_NOTIF, PAIS_RES, RES_EXAM) %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF) %>%
  group_by(YEAR, PAIS_RES) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, PAIS_RES, Falciparum, Vivax) 

# Aggregate data by country of residence and malaria type
SIVEP_PAIS_INF = df %>%
  select(DT_NOTIF, PAIS_INF, RES_EXAM) %>% 
  mutate(YEAR = year(DT_NOTIF)) %>% 
  select(-DT_NOTIF) %>%
  group_by(YEAR, PAIS_INF) %>%
  count(RES_EXAM) %>%
  spread(RES_EXAM, n, fill = 0) %>%
  rename(FALCI = "F") %>%
  rename(FV = "F+V") %>%
  rename(VIVAX = "V") %>%
  mutate(Falciparum = FALCI + FV) %>%
  mutate(Vivax = VIVAX + FV) %>%
  select(YEAR, PAIS_INF, Falciparum, Vivax) 

# Remove df
rm(df)

# Add country names
SIVEP_PAIS_RES$NAME = PAIS_CODE[match(SIVEP_PAIS_RES$PAIS_RES, PAIS_CODE$PAIS_CODE),"PAIS"]
SIVEP_PAIS_INF$NAME = PAIS_CODE[match(SIVEP_PAIS_INF$PAIS_INF, PAIS_CODE$PAIS_CODE),"PAIS"]

# Get edgelist by residence
Years=2003:2018
COUNTRY_EDGELIST_RES_VIVAX=foreach(i=1:length(Years)) %do% {
  Edgelist=paste0("COUNTRY_EDGELIST_RES_VIVAX_",Years[i])
  assign(Edgelist, 
         setNames(data.frame(from = SIVEP_PAIS_RES[which(SIVEP_PAIS_RES$YEAR == Years[i]),"NAME"],
                                        to = "BRASIL",
                                        values = SIVEP_PAIS_RES[which(SIVEP_PAIS_RES$YEAR == Years[i]),"Vivax"]),
                             c("to","from","weight")))
}

################
## Cord graph ##

# Colors
getColors=colorRampPalette(brewer.pal(11,"Spectral"))
Colors=getColors(nrow(COUNTRY_EDGELIST_RES_VIVAX_2003))

COUNTRY_EDGELIST_RES_VIVAX_PLOTS=lapply(COUNTRY_EDGELIST_RES_VIVAX,
                                        function(x){
                                          # Remove brasil-brasil edge
                                          Data=x[-1,]
                                          Data=Data[rev(order(Data$weight)),]
                                          G = graph.data.frame(Data, directed=TRUE)
                                          M = as_adjacency_matrix(G, type="both", names=TRUE, sparse=FALSE, attr="weight")
                                          Plot=chorddiag(M, type = "directional", showTicks = F, groupnameFontsize = 10, groupnamePadding = 10, margin = 90,
                                                    groupColors = Colors)
                                        })

circos.par(start.degree = 123)
chordDiagram(M)
# plot.new()
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

chordDiagram(M, 
             annotationTrack = "grid", 
             preAllocateTracks = list(track.height = 0.1),
             transparency = 0.5,
             grid.col = Colors)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE)
  # circos.axis(h = "top", labels.cex = 0.8, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)


# Plots
par(mfrow=c(1,1))
COUNTRY_EDGELIST_RES_VIVAX_PLOTS[[1]]
COUNTRY_EDGELIST_RES_VIVAX_PLOTS[[2]]
COUNTRY_EDGELIST_RES_VIVAX_PLOTS[[3]]
COUNTRY_EDGELIST_RES_VIVAX_PLOTS[[4]]

