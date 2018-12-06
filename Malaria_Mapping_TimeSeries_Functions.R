######################################
######################################
###                                ###
###  Brazilian P.vivax Time Series ###
###           Functions            ###
###                                ###
######################################
######################################

##############################################
## Load and Clean P. vivax Time Series Data ##
##############################################

# Get P. vivax raw time series data function
getPV_Raw_Data=function(FilePath, StartYear, EndYear){
  # Upload time series raw data
  TS_Raw = read.csv(file = FilePath, header=TRUE, stringsAsFactors=FALSE)
  
  # Fix all municipality names of time series data (remove accents)
  TS_Raw$MUN_NAME=gsub("Ã", "a", TS_Raw$MUN_NAME)
  TS_Raw$MUN_NAME=gsub("¡", "", TS_Raw$MUN_NAME)
  TS_Raw$MUN_NAME=iconv(TS_Raw$MUN_NAME, from = "ISO-8859-1", to = "ASCII//TRANSLIT")
  # TS_Raw$MUN_NAME=str_replace_all(TS_Raw$MUN_NAME, "[[:punct:]]", "")
  
  # Date formatting
  TS_Raw$DT_NOTIF=as.Date(TS_Raw$DT_NOTIF)
  TS_Raw=data.frame(cbind(TS_Raw,
                          year=floor_date(TS_Raw$DT_NOTIF, unit = "year"),
                          month=floor_date(TS_Raw$DT_NOTIF, unit = "month"),
                          week=floor_date(TS_Raw$DT_NOTIF, unit = "week"),
                          year_num=year(ymd(TS_Raw$DT_NOTIF)),
                          month_num=month(ymd(TS_Raw$DT_NOTIF)),
                          week_num=week(ymd(TS_Raw$DT_NOTIF))))
  
  # Column names
  colnames(TS_Raw) = c("MU_CODE","MU_NAME","STATE","DT","VIVAX","DT_YEAR", "DT_MONTH", "DT_WEEK",
                       "YEAR", "MONTH", "WEEK")
  
  # Choose time period
  TS_Raw=TS_Raw[which(TS_Raw[,"YEAR"] >= StartYear & TS_Raw[,"YEAR"] <= EndYear),]
  
  # Order
  TS_Raw=TS_Raw[order(TS_Raw[,"YEAR"]),]
  
  return(TS_Raw)
}


###################################
## Formatting and Stratification ##
###################################

# Get P. vivax time series in TS format
getPV_TS_Format=function(TS_Raw, StartYear, EndYear, Period, Frequency){
  
  # Weekly data
  TS = aggregate(TS_Raw[,"VIVAX"], by = list(TS_Raw[,Period]), sum)
  colnames(TS) = c(as.character(Period), "VIVAX")
  
  # Get TS format
  TS_Formatted = ts(na.omit(TS$VIVAX), start = c(as.integer(StartYear),1), frequency=Frequency)
  
  return(TS_Formatted)
}

# Get P. vivax time series in TS format and stratified by State
getPV_TS_Format_Stratified=function(TS_Raw, StartYear, EndYear, Stratification, Period, Frequency){
  # Get state by state time series
  TS = aggregate(TS_Raw$VIVAX ~ TS_Raw[,Stratification]+TS_Raw[,Period], TS_Raw, sum)

  # Get TS format
  if(Stratification == "STATE"){
    colnames(TS) = c("STATE", as.character(Period), "VIVAX")
    TS_Formatted=by(TS, TS$STATE, FUN=function(TS) ts(TS[,3], start = c(as.integer(StartYear),1), frequency=Frequency))  
  }
  if(Stratification == "MU_NAME"){
    colnames(TS) = c("MU_NAME", as.character(Period), "VIVAX")
    TS_Formatted=by(TS, TS$MU_NAME, FUN=function(TS) ts(TS[,3], start = c(as.integer(StartYear),1), frequency=Frequency))  
    
  }
  return(TS_Formatted)
}

###################
## Decomposition ##
###################

# Get trends
getPV_TS_Trend=function(TS, Trend, Subset, Group){
  # Single or stratified TS
  if(!is.list(TS)){
    #################
    ## Decompose Data
    #################
    
    # Use stl method to decompose data
    TS_decomposed=stl(TS, s.window="periodic")
    
    # Exract trends
    TS_Trend=TS_decomposed$time.series[,Trend]
    
    # Subset by number of years
    TS_Subset=subset(TS_Trend, start = ((as.numeric(EndYear)-as.numeric(StartYear)+1)*52)-52*Subset, end=(as.numeric(EndYear)-as.numeric(StartYear)+1)*52)
    
    # Format data and dates
    TS_Subset_DF=as.numeric(TS_Subset)
    TS_Subset_DF=data.frame(Date=as.numeric(time(TS_Subset)),
                            TS_Subset_DF)
    TS_Subset_DF$Date=as.Date(format(date_decimal(TS_Subset_DF$Date), "%Y-%m-%d"))
    colnames(TS_Subset_DF)=c("Date","All Cases")
    # Melt
    TS_Subset_DF_Melt=melt(TS_Subset_DF, id.vars="Date")
    TS_Subset_DF_Melt$Group=rep(Group, nrow(TS_Subset_DF_Melt))
    colnames(TS_Subset_DF_Melt)=c("Year", "Name", "Cases", "Group")
    
  }else{
    TS_Subset=foreach(i = 1:length(TS)) %do% {
      #################
      ## Decompose Data
      #################
      
      # Use stl method to decompose data
      TS_decomposed=stl(TS[[i]], s.window="periodic")
      
      # Exract trends
      TS_Trend=TS_decomposed$time.series[,Trend]
      
      # Subset by number of years
      TS_Subset=subset(TS_Trend, start = ((as.numeric(EndYear)-as.numeric(StartYear)+1)*52)-52*Subset, end=(as.numeric(EndYear)-as.numeric(StartYear)+1)*52)
      
    }
    names(TS_Subset)=names(TS)
    
    # Format data and dates
    TS_Subset_DF=lapply(TS_Subset, as.numeric)
    TS_Subset_DF=data.frame(Date=as.numeric(time(TS_Subset[[1]])),
                            TS_Subset_DF)
    TS_Subset_DF$Date=as.Date(format(date_decimal(TS_Subset_DF$Date), "%Y-%m-%d"))
    # Melt
    TS_Subset_DF_Melt=melt(TS_Subset_DF, id.vars="Date")
    TS_Subset_DF_Melt$Group=rep(Group, nrow(TS_Subset_DF_Melt))
    colnames(TS_Subset_DF_Melt)=c("Year", "Name", "Cases", "Group")
  }
  
  return(TS_Subset_DF_Melt)
}




