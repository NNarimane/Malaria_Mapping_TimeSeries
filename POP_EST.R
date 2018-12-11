######################################
###                                ###
### Brazilian Population Estimates ###
###                                ###
######################################

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


##############
## Data Upload
##############

# Upload UF and GR table
UF_GR=read.csv("Malaria_Mapping_TimeSeries_Data/UF_2003.csv", stringsAsFactors = F)
UF_GR$Unit_Name=iconv(UF_GR$Unit_Name, to='ASCII//TRANSLIT')
UF_GR=subset(UF_GR, select = c(Level, Code, Unit_Name, GR))
colnames(UF_GR)=c("LEVEL","CODE","Unit_Name","GR")

# Get population data function
getPOP_Data=function(Year){
  
  if(Year !=2007 & Year !=2010){
    
    # Read data and format
    BRA_POP=read.csv(paste0("Malaria_Mapping_TimeSeries_Data/IBGE/Clean/POP_",Year,".csv"), stringsAsFactors = F)
    BRA_POP=BRA_POP[,1:5]
    BRA_POP$POP=as.integer(BRA_POP$POP)
    BRA_POP$NAME=iconv(BRA_POP$NAME, to='ASCII//TRANSLIT')
    BRA_POP$NAME=gsub("\\*", "", BRA_POP$NAME)
    BRA_POP$NAME=gsub("\\s+"," ", BRA_POP$NAME)
    
    # Get CODE
    if(Year > 2006){
      BRA_POP$CODE = as.character(BRA_POP$UF_CODE * 100000 + BRA_POP$MUN_CODE)
      BRA_POP$CODE = substr(BRA_POP$CODE, 1, 6)
    }else{
      BRA_POP$CODE = as.character(BRA_POP$UF_CODE * 10000 + BRA_POP$MUN_CODE)
    }
    
    
    # Fix names and drop columns
    BRA_POP = subset(BRA_POP, select = -c(MUN_CODE))
    colnames(BRA_POP) = c("UF","UF_CODE","NAME",Year,"CODE")
    
    # Get sum of population for states and regions
    BRA_POP=merge(BRA_POP, UF_GR[,2:length(UF_GR)], by.x = "UF_CODE", by.y = "CODE")
    
    States_POP=aggregate(BRA_POP[,as.character(Year)], by=list(BRA_POP$Unit_Name), FUN=sum, na.rm=TRUE, na.action=NULL)
    Regions_POP=aggregate(BRA_POP[,as.character(Year)], by=list(BRA_POP$GR), FUN=sum, na.rm=TRUE, na.action=NULL)
    Brazil_POP=sum(States_POP$x, na.rm = TRUE)
    State_Regions_Brazil=rbind(c("Brasil",Brazil_POP), Regions_POP, States_POP)
    
    # Merge
    POP_Merge=merge(UF_GR, State_Regions_Brazil, by.x="Unit_Name", by.y="Group.1")
    names(POP_Merge) = c("NAME","LEVEL","CODE","GR",Year)
    
    # Order
    Order=c("CODE","LEVEL","NAME","GR", Year)
    
    # Select columns for MU data
    BRA_POP = subset(BRA_POP, select = -c(UF_CODE,Unit_Name,UF))
    BRA_POP$LEVEL = "MU"
    BRA_POP=BRA_POP[,Order]
    BRA_POP$NAME=gsub("\\s*\\([^\\)]+\\)", "", BRA_POP$NAME)
    
    # Rename and select columns for GR and UF data
    POP_Merge=POP_Merge[,Order]
    
    # Rbind the two data 
    BRA_POP=rbind(POP_Merge, BRA_POP)
    
    # Order and rename rownames
    BRA_POP$CODE=as.integer(BRA_POP$CODE)
    BRA_POP=BRA_POP[order(BRA_POP$CODE),]
    rownames(BRA_POP)=1:nrow(BRA_POP)
    
    return(BRA_POP)
    
  }
  
  if(Year == 2007){
    
    # Read data and format
    BRA_POP=read.csv(paste0("Malaria_Mapping_TimeSeries_Data/IBGE/Clean/POP_",Year,".csv"), stringsAsFactors = F)
    BRA_POP$NAME=iconv(BRA_POP$NAME, to='ASCII//TRANSLIT')
    BRA_POP$NAME=gsub("\\*", "", BRA_POP$NAME)
    BRA_POP$NAME=gsub("\\s+"," ", BRA_POP$NAME)
    
    # Shorten code number
    BRA_POP$CODE=substr(BRA_POP$CODE, 1, 6)
    
    # Get UF code
    BRA_POP$UF_CODE = substr(BRA_POP$CODE, 1, 2)
    colnames(BRA_POP) = c("LEVEL","CODE","NAME","2007","UF_CODE")
    
    # Get sum of population for states and regions
    BRA_POP=merge(BRA_POP, UF_GR[,2:length(UF_GR)], by.x = "UF_CODE", by.y = "CODE")
    
    States_POP=aggregate(BRA_POP[,as.character(Year)], by=list(BRA_POP$Unit_Name), FUN=sum, na.rm=TRUE, na.action=NULL)
    Regions_POP=aggregate(BRA_POP[,as.character(Year)], by=list(BRA_POP$GR), FUN=sum, na.rm=TRUE, na.action=NULL)
    Brazil_POP=sum(States_POP$x, na.rm = TRUE)
    State_Regions_Brazil=rbind(c("Brasil",Brazil_POP), Regions_POP, States_POP)
    
    # Merge
    POP_Merge=merge(UF_GR, State_Regions_Brazil, by.x="Unit_Name", by.y="Group.1")
    names(POP_Merge) = c("NAME","LEVEL","CODE","GR",Year)
    
    # Order
    Order=c("CODE","LEVEL","NAME","GR", "2007")
    
    # Select columns for MU data
    BRA_POP = subset(BRA_POP, select = -c(UF_CODE,Unit_Name))
    BRA_POP=BRA_POP[,Order]
    BRA_POP$NAME=gsub("\\s*\\([^\\)]+\\)", "", BRA_POP$NAME)
    
    # Rename and select columns for GR and UF data
    POP_Merge=POP_Merge[,Order]
    
    # Rbind the two data 
    BRA_POP=rbind(POP_Merge, BRA_POP)
    
    # Order and rename rownames
    BRA_POP$CODE=as.integer(BRA_POP$CODE)
    BRA_POP=BRA_POP[order(BRA_POP$CODE),]
    rownames(BRA_POP)=1:nrow(BRA_POP)
    
    return(BRA_POP)
  }
  
  if(Year == 2010){
      
      # 2010 Census Data
      BRA_POP=read.csv("Malaria_Mapping_TimeSeries_Data/IBGE/Clean/POP_2010.csv", stringsAsFactors = F)
      BRA_POP$Name=iconv(BRA_POP$Name, to='ASCII//TRANSLIT')
      BRA_POP$Name=gsub("\\*", "", BRA_POP$Name)
      BRA_POP$Name=gsub("\\s+"," ", BRA_POP$Name)
      
      # Make population estimates integer values
      # BRA_POP[,paste0("X",Year)] = as.integer(BRA_POP[,paste0("X",Year)])
      
      # Keep only BR, GR, UF, MU
      BRA_POP=BRA_POP[which(BRA_POP$Level == "BR" | 
                              BRA_POP$Level == "GR" |
                              BRA_POP$Level == "MU" |
                              BRA_POP$Level == "UF"),]
      
      # Fix code
      BRA_POP[which(BRA_POP$Level == "BR"),"Code"] = 0
      # Merge by code
      BRA_POP$UF_CODE=substr(BRA_POP$Code,1,2)
      BRA_POP=merge(BRA_POP, UF_GR[,2:length(UF_GR)], by.x = "UF_CODE", by.y = "CODE")
      # BRA_POP[,5]=as.numeric(BRA_POP[,5])
      
      # Select columns for MU data
      BRA_POP = subset(BRA_POP, select = -c(UF_CODE,Unit_Name))
      colnames(BRA_POP) = c("LEVEL","CODE","NAME","2010","GR")
      # Order
      Order=c("CODE","LEVEL","NAME","GR", "2010")
      BRA_POP=BRA_POP[,Order]
      BRA_POP$NAME=gsub("\\s*\\([^\\)]+\\)", "", BRA_POP$NAME)
      
      # Remove doubles
      BRA_POP = unique(BRA_POP)
      
      # Shorten code number
      BRA_POP$CODE=substr(BRA_POP$CODE, 1, 6)
      
      # Order and rename rownames
      BRA_POP$CODE=as.integer(BRA_POP$CODE)
      BRA_POP=BRA_POP[order(BRA_POP$CODE),]
      rownames(BRA_POP)=1:nrow(BRA_POP)
      
      return(BRA_POP)
  }

}

# Population data
foreach(i=2003:2018) %do% {
  BRA_POP_YEAR=paste0("BRA_POP_",i)
  assign(BRA_POP_YEAR, getPOP_Data(i))}

########
## Merge
########

# Merge function (including interpolation of missing data)
getMerge=function(){
  
  # Get list of populations by year
  POP_LIST=list(BRA_POP_2003, BRA_POP_2004, BRA_POP_2005,
            BRA_POP_2006, BRA_POP_2007, BRA_POP_2008,
            BRA_POP_2009, BRA_POP_2010, BRA_POP_2011,
            BRA_POP_2012, BRA_POP_2013, BRA_POP_2014,
            BRA_POP_2015, BRA_POP_2016, BRA_POP_2017,
            BRA_POP_2018)
  POP_LIST = lapply(POP_LIST, function(x) x[!(names(x) %in% "NAME")])
  NAME=BRA_POP_2018[,c("CODE","NAME")]
  
  # Merge (without names since there can be spelling differences
  BRA_POP_2003_2018=Reduce(function(x, y) merge(x, y, by=c("CODE","LEVEL","GR"), all=TRUE),POP_LIST)
  
  # Merge back names
  BRA_POP_2003_2018=merge(BRA_POP_2003_2018, NAME, by = "CODE")
  BRA_POP_2003_2018=BRA_POP_2003_2018[,c("CODE","LEVEL","NAME","GR",as.character(seq(2003,2018,1)))]
  
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
BRA_POP_EST=getMerge()

# Save
write.csv(BRA_POP_EST, file = "Malaria_Mapping_TimeSeries_Data/BRA_POP_EST.csv")


########################
### RL Pop Estimates ###

RL_Pop_Estimates=fread("Malaria_Mapping_TimeSeries_Data/pop_estimated.csv", encoding = "UTF-8")
RL_Pop_Estimates$YEAR=as.factor(RL_Pop_Estimates$YEAR)
RL_Pop_Estimates=RL_Pop_Estimates[,-c("COD.MUNIC")]
cRL_Pop_Estimates=recast(setDT(RL_Pop_Estimates), COD.MUNIC1+UF+COD.UF+NOME.MUNIC ~ YEAR, measure.var = "POP.EST")


#### Compare
BRA_POP_2003_2018_MU=BRA_POP_2003_2018[which(BRA_POP_2003_2018$Level =="MU"),]

### Conclusion: RL values and NN values same


