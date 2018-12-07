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
  TS_Raw = fread(file = FilePath, encoding = "UTF-8")
  
  # Fix all municipality names of time series data (remove accents)
  # TS_Raw$MUN_NAME=gsub("Ã", "a", TS_Raw$MUN_NAME)
  # TS_Raw$MUN_NAME=gsub("¡", "", TS_Raw$MUN_NAME)
  # TS_Raw$MUN_NAME=iconv(TS_Raw$MUN_NAME, from = "ISO-8859-1", to = "ASCII//TRANSLIT")
  # # TS_Raw$MUN_NAME=str_replace_all(TS_Raw$MUN_NAME, "[[:punct:]]", "")
  
  # Date formatting
  TS_Raw$DT_NOTIF=as.Date(TS_Raw$DT_NOTIF)
  TS_Raw=data.frame(cbind(TS_Raw,
                          year=floor_date(TS_Raw$DT_NOTIF, unit = "year"),
                          month=floor_date(TS_Raw$DT_NOTIF, unit = "month"),
                          week=floor_date(TS_Raw$DT_NOTIF, unit = "week"),
                          year_num=year(ymd(TS_Raw$DT_NOTIF)),
                          month_num=month(ymd(TS_Raw$DT_NOTIF)),
                          week_num=epiweek(ymd(TS_Raw$DT_NOTIF))))
  
  # Column names
  colnames(TS_Raw) = c("MU_CODE","MU_NAME","STATE","DT","VIVAX","DT_YEAR", "DT_MONTH", "DT_WEEK",
                       "YEAR", "MONTH", "WEEK")
  
  # Choose time period
  TS_Raw=TS_Raw[which(TS_Raw[,"YEAR"] >= StartYear & TS_Raw[,"YEAR"] <= EndYear),]
  
  # Order
  TS_Raw=TS_Raw[order(TS_Raw[,"YEAR"]),]
  
  return(TS_Raw)
}

# Get SIVEP data cleaned up and by species
getDAILY_SIVEP_MALARIA_TYPE=function(FilePath, StartYear, EndYear, Melted){
  
  # Get SIVEP raw notification data
  load(FilePath)
  
  # Choose time period
  df=df[which(df[,"DT_NOTIF"] >= paste0(StartYear,"-01-01") & df[,"DT_NOTIF"] <= paste0(EndYear, "-12-31")),]
  
  # Split data by malaria type and aggregate to day (MU - level)
  # day_malaria_type <- df %>%
  #   group_by(DT_NOTIF, MUN_NOTI) %>%
  #   count(RES_EXAM) %>%
  #   mutate(LEVEL = "MU") %>%
  #   select(DT_NOTIF, LEVEL, MUN_NOTI, RES_EXAM, n) %>%
  #   spread(RES_EXAM, n, fill = 0) %>%
  #   rename(CODE = MUN_NOTI) %>%
  #   rename(FALCI = "F") %>%
  #   rename(FV = "F+V") %>%
  #   rename(VIVAX = "V") %>%
  #   mutate(Falciparum = FALCI + FV) %>%
  #   mutate(Vivax = VIVAX + FV) %>%
  #   select(DT_NOTIF, LEVEL, CODE, Falciparum, Vivax) %>%
  #   gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE))
  
  day_malaria_type_places_muni <- df %>%
    group_by(DT_NOTIF, MUN_NOTI, MUN_RESI, MUN_INFE) %>%
    count(RES_EXAM) %>%
    mutate(LEVEL = "MU") %>%
    select(DT_NOTIF, LEVEL, MUN_NOTI, MUN_RESI, MUN_INFE, RES_EXAM, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(CODE_NOTIF = MUN_NOTI, CODE_RESID = MUN_RESI, CODE_INFEC = MUN_INFE, FALCI = "F", FV = "F+V", VIVAX = "V") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE_NOTIF, CODE_RESID, CODE_INFEC, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE_NOTIF, CODE_RESID, CODE_INFEC))
  
  # UT - level
  # day_malaria_type_state <- df %>%
  #   group_by(DT_NOTIF, UF_NOTIF) %>%
  #   count(RES_EXAM) %>%
  #   mutate(LEVEL = "UF") %>%
  #   select(DT_NOTIF, LEVEL, UF_NOTIF, RES_EXAM, n) %>%
  #   spread(RES_EXAM, n, fill = 0) %>%
  #   rename(CODE = UF_NOTIF) %>%
  #   rename(FALCI = "F") %>%
  #   rename(FV = "F+V") %>%
  #   rename(VIVAX = "V") %>%
  #   mutate(Falciparum = FALCI + FV) %>%
  #   mutate(Vivax = VIVAX + FV) %>%
  #   select(DT_NOTIF, LEVEL, CODE, Falciparum, Vivax) %>%
  #   gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE))
  
  day_malaria_type_places_state <- df %>%
    group_by(DT_NOTIF, UF_NOTIF, UF_RESID, UF_INFEC) %>%
    count(RES_EXAM) %>%
    mutate(LEVEL = "UF") %>%
    select(DT_NOTIF, LEVEL, UF_NOTIF, UF_RESID, UF_INFEC, RES_EXAM, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(CODE_NOTIF = UF_NOTIF, CODE_RESID = UF_RESID, CODE_INFEC = UF_INFEC, FALCI = "F", FV = "F+V", VIVAX = "V") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE_NOTIF, CODE_RESID, CODE_INFEC, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE_NOTIF, CODE_RESID, CODE_INFEC))
  
  # BR - level
  # day_malaria_type_brazil <- df %>%
  #   group_by(DT_NOTIF) %>%
  #   count(RES_EXAM)%>%
  #   mutate(LEVEL = "BR",
  #          CODE = "0") %>%
  #   select(DT_NOTIF, LEVEL, CODE, RES_EXAM, n) %>%
  #   spread(RES_EXAM, n, fill = 0) %>%
  #   rename(FALCI = "F") %>%
  #   rename(FV = "F+V") %>%
  #   rename(VIVAX = "V") %>%
  #   mutate(Falciparum = FALCI + FV) %>%
  #   mutate(Vivax = VIVAX + FV) %>%
  #   select(DT_NOTIF, LEVEL, CODE, Falciparum, Vivax) %>%
  #   gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE))
  
  day_malaria_type_brazil <- df %>%
    group_by(DT_NOTIF) %>%
    count(RES_EXAM)%>%
    mutate(LEVEL = "BR",
           CODE = "1") %>%
    select(DT_NOTIF, LEVEL, CODE, RES_EXAM, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "F") %>%
    rename(FV = "F+V") %>%
    rename(VIVAX = "V") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE))
  
  # Merge together
  # SIVEP_MALARIA_TYPE=rbind(day_malaria_type_brazil, day_malaria_type_state, day_malaria_type)
  SIVEP_MALARIA_TYPE=rbind(day_malaria_type_brazil, day_malaria_type_places_state, day_malaria_type_places_muni)
  
  # Select administrative level via code
  if(byNotification){
    SIVEP_MALARIA_TYPE = subset(SIVEP_MALARIA_TYPE, select=-c(CODE_RESID, CODE_INFEC))
    names(SIVEP_MALARIA_TYPE)[names(SIVEP_MALARIA_TYPE) == 'CODE_NOTIF'] <- 'CODE'
  }
  if(byResidence){
    SIVEP_MALARIA_TYPE = subset(SIVEP_MALARIA_TYPE, select=-c(CODE_NOTIF, CODE_INFEC))
    names(SIVEP_MALARIA_TYPE)[names(SIVEP_MALARIA_TYPE) == 'CODE_RESID'] <- 'CODE'
  }
  if(byInfection){
    SIVEP_MALARIA_TYPE = subset(SIVEP_MALARIA_TYPE, select=-c(CODE_NOTIF, CODE_RESID))
    names(SIVEP_MALARIA_TYPE)[names(SIVEP_MALARIA_TYPE) == 'CODE_INFEC'] <- 'CODE'
  }
  
  # Get week, month, and year
  SIVEP_MALARIA_TYPE=data.frame(cbind(SIVEP_MALARIA_TYPE,
                          DT_YEAR=floor_date(SIVEP_MALARIA_TYPE$DT_NOTIF, unit = "year"),
                          DT_MONTH=floor_date(SIVEP_MALARIA_TYPE$DT_NOTIF, unit = "month"),
                          DT_WEEK=floor_date(SIVEP_MALARIA_TYPE$DT_NOTIF, unit = "week")))
  
  if(Melted){
    #########################
    ## General Plots (Melted)
    #########################
    
    # Melt by date
    mSIVEP_MALARIA_TYPE=melt(setDT(SIVEP_MALARIA_TYPE),
                   measure.vars = list(c("DT_NOTIF",
                                         "DT_YEAR",
                                         "DT_MONTH",
                                         "DT_WEEK")),
                   variable.name = "DATE_TYPE", 
                   value.name = "DATE",
                   id.vars = c("LEVEL","CODE","TYPE","CASES"))
    
    # Reorder and relabel date type
    levels(mSIVEP_MALARIA_TYPE$DATE_TYPE) = c("Daily","Yearly","Monthly","Weekly")
    mSIVEP_MALARIA_TYPE$DATE_TYPE = factor(mSIVEP_MALARIA_TYPE$DATE_TYPE,
                                 levels = c("Daily","Weekly","Monthly","Yearly"))
    
    return(mSIVEP_MALARIA_TYPE)
    
  }else{
    
    return(SIVEP_MALARIA_TYPE)
  }
}

# Get SIVEP data cleaned up and by species
getDAILY_SIVEP_MALARIA_GENDER=function(FilePath, StartYear, EndYear, Melted){
  
  # Get SIVEP raw notification data
  load(FilePath)
  
  # Choose time period
  df=df[which(df[,"DT_NOTIF"] >= paste0(StartYear,"-01-01") & df[,"DT_NOTIF"] <= paste0(EndYear, "-12-31")),]
  
  # Split data by malaria type and aggregate to day (MU - level)
  day_malaria_type_gender <- df %>%
    group_by(DT_NOTIF, MUN_NOTI, SEXO) %>%
    count(RES_EXAM) %>%
    mutate(LEVEL = "MU") %>%
    select(DT_NOTIF, LEVEL, MUN_NOTI, RES_EXAM, SEXO, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(CODE = MUN_NOTI, FALCI = "F", FV = "F+V", VIVAX = "V", GENDER = SEXO) %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE, GENDER, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE, GENDER))
  
  # UT - level
  day_malaria_type_state_gender <- df %>%
    group_by(DT_NOTIF, UF_NOTIF, SEXO) %>%
    count(RES_EXAM) %>%
    mutate(LEVEL = "UF") %>%
    select(DT_NOTIF, LEVEL, UF_NOTIF, RES_EXAM, SEXO, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(CODE = UF_NOTIF, FALCI = "F", FV = "F+V", VIVAX = "V", GENDER = SEXO) %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE, GENDER, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE, GENDER))
  
  # BR - level
  day_malaria_type_brazil_gender <- df %>%
    group_by(DT_NOTIF, SEXO) %>%
    count(RES_EXAM)%>%
    mutate(LEVEL = "BR",
           CODE = "0") %>%
    select(DT_NOTIF, LEVEL, CODE, RES_EXAM, SEXO, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "F", FV = "F+V", VIVAX = "V", GENDER = SEXO) %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE, GENDER, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE, GENDER))
  
  # Merge together
  SIVEP_MALARIA_GENDER=rbind(day_malaria_type_brazil_gender, day_malaria_type_state_gender, day_malaria_type_gender)
  
  # Remove unknown gender
  levels(SIVEP_MALARIA_GENDER$GENDER)[levels(SIVEP_MALARIA_GENDER$GENDER)=="I"] = NA
  
  # Get week, month, and year
  SIVEP_MALARIA_GENDER=data.frame(cbind(SIVEP_MALARIA_GENDER,
                                      DT_YEAR=floor_date(SIVEP_MALARIA_GENDER$DT_NOTIF, unit = "year"),
                                      DT_MONTH=floor_date(SIVEP_MALARIA_GENDER$DT_NOTIF, unit = "month"),
                                      DT_WEEK=floor_date(SIVEP_MALARIA_GENDER$DT_NOTIF, unit = "week")))
  
  if(Melted){
    #########################
    ## General Plots (Melted)
    #########################
    
    # Melt by date
    mSIVEP_GENDER=melt(setDT(SIVEP_MALARIA_GENDER),
                             measure.vars = list(c("DT_NOTIF",
                                                   "DT_YEAR",
                                                   "DT_MONTH",
                                                   "DT_WEEK")),
                             variable.name = "DATE_TYPE", 
                             value.name = "DATE",
                             id.vars = c("LEVEL","CODE","TYPE","GENDER","CASES"))
    
    # Reorder and relabel date type
    levels(mSIVEP_GENDER$DATE_TYPE) = c("Daily","Yearly","Monthly","Weekly")
    mSIVEP_GENDER$DATE_TYPE = factor(mSIVEP_GENDER$DATE_TYPE,
                                           levels = c("Daily","Weekly","Monthly","Yearly"))
    
    return(mSIVEP_GENDER)
    
  }else{
    
    return(SIVEP_MALARIA_GENDER)
  }
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




