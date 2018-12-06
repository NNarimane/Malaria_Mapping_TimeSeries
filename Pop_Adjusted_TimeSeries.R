###################################################
###                                             ###
### Population Adjusted Time Series of P. vivax ###
###                                             ###
###################################################


#################################
## Packages and working directory
#################################

library("dplyr")
library("sqldf")
library("foreach")
library("taRifx")
library("imputeTS")
library("stringr")
library("lubridate")
library("reshape2")
library("malariaAtlas")
library("data.table")

setwd("C:/Users/nnekkab/Desktop/Preliminary Time Series Analyses/Population Adjusted Time Series/")


##############
## Data Upload
##############

# Upload UF and GR table
UF_GR=read.csv("Data/UF_2003.csv", stringsAsFactors = F)
UF_GR$Unit_Name=iconv(UF_GR$Unit_Name, to='ASCII//TRANSLIT')

# Get population data function
getPOP_Data=function(Year){
  
  if(Year < 2010){
    
    # Read data and format
    BRA_POP=read.csv(paste0("Data/BRA_POP_",Year,".csv"), stringsAsFactors = F)
    BRA_POP$Name=iconv(BRA_POP$Name, to='ASCII//TRANSLIT')
    BRA_POP$Name=gsub("\\*", "", BRA_POP$Name)
    BRA_POP$Name=gsub("\\s+"," ", BRA_POP$Name)
    
    # Make population estimates integer values
    BRA_POP[,paste0("X",Year)] = as.integer(BRA_POP[,paste0("X",Year)])
    
    # Get sum of population for states and regions
    BRA_POP$Number=substr(BRA_POP$Code,1,2)
    BRA_POP=merge(BRA_POP, UF_GR[,2:length(UF_GR)], by.x = "Number", by.y = "Code")
    BRA_POP[,5]=as.numeric(BRA_POP[,5])
    States_POP=aggregate(BRA_POP[,5], by=list(BRA_POP$Unit_Name), FUN=sum, na.action = NULL)
    Regions_POP=aggregate(BRA_POP[,5], by=list(BRA_POP$GR), FUN=sum, na.action = NULL)
    Brazil_POP=sum(States_POP$x, na.rm = TRUE)
    State_Regions_Brazil=rbind(c("Brasil",Brazil_POP), Regions_POP, States_POP)
    
    # Merge
    POP_Merge=merge(UF_GR, State_Regions_Brazil, by.x="Unit_Name", by.y="Group.1")
    
    # Order
    Order=c("Level","Code","Name","UF", paste0("X",Year))
    
    # Select columns for MU data
    BRA_POP=BRA_POP[BRA_POP$Level == "MU",]
    BRA_POP=BRA_POP[,Order]
    BRA_POP$Name=gsub("\\s*\\([^\\)]+\\)", "", BRA_POP$Name)
    
    # Rename and select columns for GR and UF data
    names(POP_Merge) = c("Name","Level","Code","GR","UF",paste0("X",Year))
    POP_Merge=POP_Merge[,Order]
    
    # Rbind the two data 
    BRA_POP=rbind(POP_Merge, BRA_POP)
    
    # Order and rename rownames
    BRA_POP=BRA_POP[order(BRA_POP$Code),]
    rownames(BRA_POP)=1:nrow(BRA_POP)
    
    # Shorten code number
    BRA_POP$Code=substr(BRA_POP$Code, 1, 6)
    
    return(BRA_POP)
    
  }else{
    
    if(Year == 2010){
      
      # 2010 Census Data
      BRA_POP=read.csv("Data/BRA_POP_2010.csv", stringsAsFactors = F)
      BRA_POP$Name=iconv(BRA_POP$Name, to='ASCII//TRANSLIT')
      BRA_POP$Name=gsub("\\*", "", BRA_POP$Name)
      BRA_POP$Name=gsub("\\s+"," ", BRA_POP$Name)
      
      # Make population estimates integer values
      BRA_POP[,paste0("X",Year)] = as.integer(BRA_POP[,paste0("X",Year)])
      
      # Keep only BR, GR, UF, MU
      BRA_POP=BRA_POP[which(BRA_POP$Level == "BR" | 
                                        BRA_POP$Level == "GR" |
                                        BRA_POP$Level == "MU" |
                                        BRA_POP$Level == "UF"),]
      
      # # Merge
      BRA_POP$Number=substr(BRA_POP$Code,1,2)
      BRA_POP=merge(BRA_POP, UF_GR[,2:length(UF_GR)], by.x = "Number", by.y = "Code")
      BRA_POP[,5]=as.numeric(BRA_POP[,5])
      
      # Order
      Order=c("Level","Code","Name","UF", "X2010")
      
      # Select columns for MU data
      BRA_POP=BRA_POP[,Order]
      BRA_POP$Name=gsub("\\s*\\([^\\)]+\\)", "", BRA_POP$Name)

      # Remove doubles
      BRA_POP = unique(BRA_POP)
      
      # Order and rename rownames
      BRA_POP=BRA_POP[order(BRA_POP$Code),]
      rownames(BRA_POP)=1:nrow(BRA_POP)
      
      # Shorten code number
      BRA_POP$Code=substr(BRA_POP$Code, 1, 6)
      
      # Make BR code 0
      BRA_POP[which(BRA_POP$Level == "BR"),"Code"] = "0"
      
      return(BRA_POP)
      
    }else{
      
      # 2011-2018
      BRA_POP=read.csv("Data/BRA_POP_2011_2018.csv", stringsAsFactors = F)
      BRA_POP$Name=iconv(BRA_POP$Name, to='ASCII//TRANSLIT')
      BRA_POP$Name=gsub("\\*", "", BRA_POP$Name)
      BRA_POP$Name=gsub("\\s+"," ", BRA_POP$Name)
      
      # Make population estimates integer values
      BRA_POP[,4:ncol(BRA_POP)] = lapply(BRA_POP[,5:ncol(BRA_POP)], as.integer)
      
      # Keep only BR, GR, UF, MU
      BRA_POP=BRA_POP[which(BRA_POP$Level == "BR" | 
                                        BRA_POP$Level == "GR" |
                                        BRA_POP$Level == "MU" |
                                        BRA_POP$Level == "UF"),]
      
      # # Merge
      BRA_POP$Number=substr(BRA_POP$Code,1,2)
      BRA_POP=merge(BRA_POP, UF_GR[,2:length(UF_GR)], by.x = "Number", by.y = "Code")
      BRA_POP[,5]=as.numeric(BRA_POP[,5])
      
      # Order
      Order=c("Level","Code","Name","UF", paste0("X", seq(2011,2018,1)))
      
      # Select columns for MU data
      BRA_POP=BRA_POP[,Order]
      BRA_POP$Name=gsub("\\s*\\([^\\)]+\\)", "", BRA_POP$Name)
      
      # Remove doubles
      BRA_POP = unique(BRA_POP)
      
      # Order and rename rownames
      BRA_POP=BRA_POP[order(BRA_POP$Code),]
      rownames(BRA_POP)=1:nrow(BRA_POP)
      
      # Shorten code number
      BRA_POP$Code=substr(BRA_POP$Code, 1, 6)
      
      # Make BR code 0
      BRA_POP[which(BRA_POP$Level == "BR"),"Code"] = "0"
      
      return(BRA_POP)
    }
  }

}

# Population data
BRA_POP_2003=getPOP_Data(2003)
BRA_POP_2004=getPOP_Data(2004)
BRA_POP_2005=getPOP_Data(2005)
BRA_POP_2006=getPOP_Data(2006)
BRA_POP_2007=getPOP_Data(2007)
BRA_POP_2008=getPOP_Data(2008)
BRA_POP_2009=getPOP_Data(2009)
BRA_POP_2010=getPOP_Data(2010)
BRA_POP_2011_2018=getPOP_Data(2011)

########
## Merge
########

# Merge function (including interpolation of missing data)
getMerge=function(){
  
  # Merge (without names since there can be spelling differences)
  BRA_POP_2003_2018=Reduce(function(x, y) merge(x, y, by=c("Level","Code","UF"), all=TRUE), 
                           list(BRA_POP_2003, BRA_POP_2004, BRA_POP_2005,
                                BRA_POP_2006, BRA_POP_2007, BRA_POP_2008,
                                BRA_POP_2009, BRA_POP_2010, BRA_POP_2011_2018))
  
  
  # Test=BRA_POP_2003_2018[duplicated(BRA_POP_2003_2018$Code, BRA_POP_2003_2018$Level) == TRUE,]
  
  # Remove older names 
  ColumnsToKeep=c("Level","Code","UF","Name", paste0("X", seq(2003,2018,1)))
  BRA_POP_2003_2018=BRA_POP_2003_2018[,ColumnsToKeep]
  # Rename
  colnames(BRA_POP_2003_2018) = c("Level","Code","UF","Name", seq(2003,2018,1))
  
  # Make population estimates integer values
  BRA_POP_2003_2018[,5:ncol(BRA_POP_2003_2018)] = lapply(BRA_POP_2003_2018[,5:ncol(BRA_POP_2003_2018)], as.integer)
  
  # Subset data with NAs
  MissingData=BRA_POP_2003_2018[rowSums(is.na(BRA_POP_2003_2018)) > 0,]
  
  # Replace data
  Interpolation=foreach(i=1:nrow(MissingData), .combine = "rbind") %do% {
    
    # Select data
    Data=t(MissingData[i,5:ncol(MissingData)])
    
    # Perform spline interpolation
    SplineData=as.integer(na.interpolation(Data, option = "spline"))
    
  }
  
  # Re-insert missing data with interpolation
  Interpolation=cbind(MissingData[,1:4], Interpolation)
  colnames(Interpolation)=colnames(MissingData)
  
  # Merge
  BRA_POP_2003_2018_FINAL=rbind(BRA_POP_2003_2018[complete.cases(BRA_POP_2003_2018),], Interpolation)
  
  return(BRA_POP_2003_2018_FINAL)
}

# Combined population estimates, census, and counts
BRA_POP_2003_2018=getMerge()

# Clean up
rm(BRA_POP_2003, BRA_POP_2004, BRA_POP_2005,
   BRA_POP_2006, BRA_POP_2007, BRA_POP_2008,
   BRA_POP_2009, BRA_POP_2010, BRA_POP_2011_2018)
gc()

################
## P. vivax data
################

# Get P. vivax time series data function
getPV_TS=function(){
  # Upload time series raw data
  TS_Raw = read.csv(file = "C:/Users/nnekkab/Desktop/DATA/TimeSeriesMalaria.csv", header=TRUE, stringsAsFactors=FALSE)
  
  # Fix all municipality names of time series data (remove accents)
  TS_Raw$MUN_NAME=gsub("Ã", "a", TS_Raw$MUN_NAME)
  TS_Raw$MUN_NAME=gsub("¢", "", TS_Raw$MUN_NAME)
  TS_Raw$MUN_NAME=iconv(TS_Raw$MUN_NAME, from = "ISO-8859-1", to = "ASCII//TRANSLIT")
  TS_Raw$MUN_NAME=str_replace_all(TS_Raw$MUN_NAME, "[[:punct:]]", "")
  
  # Date formatting
  TS_Raw$DT_NOTIF=as.Date(TS_Raw$DT_NOTIF)
  TS_Raw=data.frame(cbind(TS_Raw, year=year(ymd(TS_Raw$DT_NOTIF))))
  
  # Order
  TS_Raw=TS_Raw[order(TS_Raw[,"year"]),]
  
  # Column names
  colnames(TS_Raw) = c("Code","Name","UF","Date","VIVAX","Year")
  
  return(TS_Raw)
}

# Get P. vivax cases
PV_TS=getPV_TS()

# Fix wrong UF
PV_TS[which(PV_TS$Name == "Pedra Branca do Amapari"),"UF"] = "AP"

#################
## Annual Summary
#################

PV_TS_Aggregated=PV_TS[,c("Code","Name","UF","VIVAX","Year")]
PV_TS_Aggregated=aggregate(VIVAX~Code+Name+UF+Year, data=PV_TS_Aggregated, FUN = sum)

#########################################
## Add Population Column to P. vivax data
#########################################

# Melt population data
mBRA_POP_2003_2018=melt(BRA_POP_2003_2018, id = c("Level","Code","UF","Name"),
                        variable.name = "Year", value.name = "Population")

# Check names
table(PV_TS_Aggregated$Name %in% BRA_POP_2003_2018$Name)

# Factor year
PV_TS_Aggregated$Year=as.factor(PV_TS_Aggregated$Year)

# Merge Pvivax case data with yearly population estimates
PV_POP=merge(PV_TS_Aggregated, mBRA_POP_2003_2018, by = c("Code","Name","UF","Year"), all.x = TRUE)

#########################################
## Adjust Pv cases per population of 1000
#########################################

PV_POP$VIVAX_API = (PV_POP$VIVAX / PV_POP$Population) * 1000


###########
## Map
##########

Year=2003

# # Get municipality shape files
BRA_shp_lvl2=getShp(country = "Brazil", admin_level = "admin2", format = "df")

# Upload State acronyms
StateAbbrev=read.csv(file="Data/StateAbbrev.csv")
# Remove accents
StateAbbrev$name=iconv(StateAbbrev$name, to='ASCII//TRANSLIT')
# Keep only state name and acronym
StateAbbrev=StateAbbrev[,c("subdivision","name")]

# Merge names
PV_POP=merge(PV_POP, StateAbbrev, by.x="UF", by.y="subdivision", all.x=T)
# Change Name to name2
colnames(PV_POP) = c("UF","Code","name2","Year","VIVAX","Level","Population","VIVAX_API","name1")

# # Select only one year
# PV_POP_MU_2003=PV_POP[which(PV_POP$Year == Year),]
# 
# # Step 1b: change abbrevation of state names to full name
# PV_POP_MU_2003=merge(PV_POP_MU_2003, StateAbbrev, by.x="UF", by.y="subdivision", all.x=T)
# # Step 1c: rename to name1
# colnames(PV_POP_MU_2003)[ncol(PV_POP_MU_2003)]="name1"
# 
# # Step 3: check if name match
# table(PV_POP_MU_2003$name2 %in% BRA_shp_lvl2$name_2) # all True


########
## Merge

# Need to merge shapefiles and population data with both MU name and State name
# Step 1: make names into character temporarily
BRA_shp_lvl2$name_1=as.character(BRA_shp_lvl2$name_1)
BRA_shp_lvl2$name_2=as.character(BRA_shp_lvl2$name_2)
# Step 2: merge (left_join will help keep in order)
BRA_shp_lvl1_df_POP=left_join(BRA_shp_lvl2, PV_POP, by = c("name_1" = "name1","name_2" = "name2"))
# Step 3: reorder and change back to factors
BRA_shp_lvl1_df_POP$name_1=factor(BRA_shp_lvl1_df_POP$name_1)
BRA_shp_lvl1_df_POP$name_2=factor(BRA_shp_lvl1_df_POP$name_2)
# Step 4: save/load
save(BRA_shp_lvl1_df_POP, file="BRA_shp_lvl2_PV.RData")
# load(file=paste0("BRA_shp_lvl1_df_POP_", Year, ".RData"))


# Melt data
setDT(BRA_shp_lvl1_df_POP)
# BRA_shp_lvl1_df_POP_melted=melt(BRA_shp_lvl1_df_POP,
#                                 measure.vars = list(c("X2010","X2011",
#                                                       "X2012","X2013",
#                                                       "X2014","X2015",
#                                                       "X2016","X2017",
#                                                       "X2018")),
#                                 variable.name = "Year", 
#                                 value.name = "Population",
#                                 id.vars = 1:23)

# Create population categorical variable
BRA_shp_lvl1_df_POP$Vivax_Case_Counts <- cut(as.numeric(BRA_shp_lvl1_df_POP[,"VIVAX"]),
                                             breaks=c(NA, -Inf, 100, 1000, 5000, 10000, 20000, 30000,  Inf),
                                             labels=c("< 100","< 1000","< 5000",
                                                      "< 10000", "< 20000", "< 30000", 
                                                      "> 30000"))

# Fix Year labels
levels(BRA_shp_lvl1_df_POP_melted$Year)=as.character(seq(2010,2018))


#######
## Plot



# Plot
BRA_POP_Plot_LVL1=ggplot(BRA_shp_lvl1_df_POP_melted) +
  aes(long, lat, group=group, fill = Population) +
  scale_fill_brewer(palette="Blues") +
  coord_equal() +
  geom_polygon(colour = "black", size = 0.5) +
  facet_wrap(facets= Year ~.) +
  labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica")

#################################
# # Categorize population variable
# Population <- cut(as.numeric(BRA_shp_lvl2[,paste0("X",Year)]),
#                   breaks=c(-Inf, 1000, 10000, 100000, 1000000, 10000000,  Inf),
#                   labels=c("< 1 000","< 10 000","< 100 000",
#                            "< 1 000 000", "< 10 000 000", "> 10 000 000"))

# Categorize VIVAX variable
Vivax_Case_Counts <- cut(as.numeric(BRA_shp_lvl2[,"VIVAX"]),
                  breaks=c(-Inf, 100, 1000, 5000, 10000, 20000, 30000,  Inf),
                  labels=c("< 100","< 1000","< 5000",
                           "< 10000", "< 20000", "< 30000", 
                           "> 30000"))


# With colors
ggplot(data=BRA_shp_lvl2) +
  geom_polygon(color=NA, aes(long, lat, group=group, fill=Vivax_Case_Counts)) +
  scale_fill_brewer(palette="Blues", na.value = "grey") +
  ggtitle(paste("Brazilian", Year, "Vivax_Case_Counts")) +
  coord_equal() +
  theme_minimal() 


# Categorize VIVAX_API variable
Vivax_Case_Counts <- cut(as.numeric(BRA_shp_lvl2[,"VIVAX_API"]),
                         breaks=c(-Inf, 100, 1000, 5000, 10000, 20000, 30000,  Inf),
                         labels=c("< 100","< 1000","< 5000",
                                  "< 10000", "< 20000", "< 30000", 
                                  "> 30000"))


# With colors
ggplot(data=BRA_shp_lvl2) +
  geom_polygon(color=NA, aes(long, lat, group=group, fill=Vivax_Case_Counts)) +
  scale_fill_brewer(palette="Blues", na.value = "grey") +
  ggtitle(paste("Brazilian", Year, "Vivax_Case_Counts")) +
  coord_equal() +
  theme_minimal() 





