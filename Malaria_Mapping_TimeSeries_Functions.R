#############################################################
#############################################################
##########                                         ##########
##########                                         ##########
##########      *** SIVEP DATA ANALYSIS ***        ##########
##########            *** FUNCTIONS ***            ##########
##########                                         ##########
##########                                         ##########
#############################################################
#############################################################

#############################################
## LOAD AND AGGREGATE SIVEP DATA FUNCTIONS ##
#############################################

# Get SIVEP data cleaned up and by species
getDAILY_SIVEP_MALARIA_TYPE=function(FilePath, StartYear, EndYear, Melted){
  
  # Get SIVEP raw notification data
  load(FilePath)
  
  # Choose time period
  df=df[which(df[,"DT_NOTIF"] >= paste0(StartYear,"-01-01") & df[,"DT_NOTIF"] <= paste0(EndYear, "-12-31")),]
  
  # Replace missing state code with municipality state code
  df$UF_NOTIF=substr(df$MUN_NOTI, 1, 2) 
  df$UF_RESID=substr(df$MUN_RESI, 1, 2)
  df$UF_INFEC=substr(df$MUN_INFE, 1, 2)
  
  # Split data by malaria type and aggregate to day (MU - level)
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
  SIVEP_MALARIA_TYPE=rbind(day_malaria_type_brazil, day_malaria_type_places_state, day_malaria_type_places_muni)
  
  # Select administrative level via code
  if(byNotification){
    SIVEP_MALARIA_TYPE = subset(SIVEP_MALARIA_TYPE, select=-c(CODE, CODE_RESID, CODE_INFEC))
    names(SIVEP_MALARIA_TYPE)[names(SIVEP_MALARIA_TYPE) == 'CODE_NOTIF'] <- 'CODE'
  }
  if(byResidence){
    SIVEP_MALARIA_TYPE = subset(SIVEP_MALARIA_TYPE, select=-c(CODE, CODE_NOTIF, CODE_INFEC))
    names(SIVEP_MALARIA_TYPE)[names(SIVEP_MALARIA_TYPE) == 'CODE_RESID'] <- 'CODE'
  }
  if(byInfection){
    SIVEP_MALARIA_TYPE = subset(SIVEP_MALARIA_TYPE, select=-c(CODE, CODE_NOTIF, CODE_RESID))
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

# Get SIVEP data cleaned up and by gender
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
    mSIVEP_MALARIA_GENDER=melt(setDT(SIVEP_MALARIA_GENDER),
                             measure.vars = list(c("DT_NOTIF",
                                                   "DT_YEAR",
                                                   "DT_MONTH",
                                                   "DT_WEEK")),
                             variable.name = "DATE_TYPE", 
                             value.name = "DATE",
                             id.vars = c("LEVEL","CODE","TYPE","GENDER","CASES"))
    
    # Reorder and relabel date type
    levels(mSIVEP_MALARIA_GENDER$DATE_TYPE) = c("Daily","Yearly","Monthly","Weekly")
    mSIVEP_MALARIA_GENDER$DATE_TYPE = factor(mSIVEP_MALARIA_GENDER$DATE_TYPE,
                                           levels = c("Daily","Weekly","Monthly","Yearly"))
    
    return(mSIVEP_MALARIA_GENDER)
    
  }else{
    
    return(SIVEP_MALARIA_GENDER)
  }
}

# Get SIVEP data cleaned up and by age
getDAILY_SIVEP_MALARIA_AGE=function(FilePath, StartYear, EndYear, Melted){
  
  # Get SIVEP raw notification data
  load(FilePath)
  
  # Choose time period
  df=df[which(df[,"DT_NOTIF"] >= paste0(StartYear,"-01-01") & df[,"DT_NOTIF"] <= paste0(EndYear, "-12-31")),]
  
  ### Creating the continuos age by year and after by categories (age range) from IBGE
  df <- df %>% 
    filter(ID_PACIE < 30 & ID_DIMEA == "D" | ID_PACIE < 12 & ID_DIMEA == "M" | ID_PACIE <= 100 & ID_DIMEA == "A")
  
  df$ID_PACIE <- as.double(df$ID_PACIE) 
  
  df <- df %>% 
    mutate(AGE_CONT = if_else(ID_DIMEA == "A", ID_PACIE, 0))

  df$AGE_RANGE <- NA 
  df[which(df$AGE_CONT <= 4),]$AGE_RANGE <- "0-4"
  df[which(df$AGE_CONT >= 5 & df$AGE_CONT <= 9),]$AGE_RANGE <- "5-9"
  df[which(df$AGE_CONT >= 10 & df$AGE_CONT <= 14),]$AGE_RANGE <- "10-14"
  df[which(df$AGE_CONT >= 15 & df$AGE_CONT <= 19),]$AGE_RANGE <- "15-19"
  df[which(df$AGE_CONT >= 20 & df$AGE_CONT <= 24),]$AGE_RANGE <- "20-24"
  df[which(df$AGE_CONT >= 25 & df$AGE_CONT <= 29),]$AGE_RANGE <- "25-29"
  df[which(df$AGE_CONT >= 30 & df$AGE_CONT <= 34),]$AGE_RANGE <- "30-34"
  df[which(df$AGE_CONT >= 35 & df$AGE_CONT <= 39),]$AGE_RANGE <- "35-39"
  df[which(df$AGE_CONT >= 40 & df$AGE_CONT <= 44),]$AGE_RANGE <- "40-44"
  df[which(df$AGE_CONT >= 45 & df$AGE_CONT <= 49),]$AGE_RANGE <- "45-49"
  df[which(df$AGE_CONT >= 50 & df$AGE_CONT <= 54),]$AGE_RANGE <- "50-54"
  df[which(df$AGE_CONT >= 55 & df$AGE_CONT <= 59),]$AGE_RANGE <- "55-59"
  df[which(df$AGE_CONT >= 60 & df$AGE_CONT <= 64),]$AGE_RANGE <- "60-64"
  df[which(df$AGE_CONT >= 65 & df$AGE_CONT <= 69),]$AGE_RANGE <- "65-69"
  df[which(df$AGE_CONT >= 70 & df$AGE_CONT <= 74),]$AGE_RANGE <- "70-74"
  df[which(df$AGE_CONT >= 75 & df$AGE_CONT <= 79),]$AGE_RANGE <- "75-79"
  df[which(df$AGE_CONT >= 80),]$AGE_RANGE <- "80+"
  
  df$AGE_RANGE = factor(df$AGE_RANGE, label=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"), levels= c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))
  
  ### Counting malaria cases by day, UF, municipality, AGE RANGE, malaria type.
  day_malaria_type_age <- df %>%
    group_by(DT_NOTIF, MUN_NOTI, AGE_RANGE) %>%
    count(RES_EXAM) %>%
    mutate(LEVEL = "MU") %>%
    select(DT_NOTIF, LEVEL, MUN_NOTI, RES_EXAM, AGE_RANGE, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(CODE = MUN_NOTI, FALCI = "F", FV = "F+V", VIVAX = "V", AGE_CAT = AGE_RANGE) %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE, AGE_CAT, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE, AGE_CAT))
  
  ### Counting malaria cases by day, UF, AGE RANGE and malaria type
  day_malaria_type_state_age <- df %>%
    group_by(DT_NOTIF, UF_NOTIF, AGE_RANGE) %>%
    count(RES_EXAM) %>%
    mutate(LEVEL = "UF") %>%
    select(DT_NOTIF, LEVEL, UF_NOTIF, RES_EXAM, AGE_RANGE, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(CODE = UF_NOTIF, FALCI = "F", FV = "F+V", VIVAX = "V", AGE_CAT = AGE_RANGE) %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE, AGE_CAT, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE, AGE_CAT))
  
  ### Counting malaria cases by day, AGE RANGE and malaria type
  day_malaria_type_brazil_age <- df %>%
    group_by(DT_NOTIF, AGE_RANGE) %>%
    count(RES_EXAM)%>%
    mutate(LEVEL = "BR",
           CODE = "1") %>%
    select(DT_NOTIF, LEVEL, CODE, RES_EXAM, AGE_RANGE, n) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "F", FV = "F+V", VIVAX = "V", AGE_CAT = AGE_RANGE) %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(DT_NOTIF, LEVEL, CODE, AGE_CAT, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(DT_NOTIF, LEVEL, CODE, AGE_CAT))
  
  # Merge together
  SIVEP_MALARIA_AGE=rbind(day_malaria_type_brazil_age, day_malaria_type_state_age, day_malaria_type_age)
  
  # Get week, month, and year
  SIVEP_MALARIA_AGE=data.frame(cbind(SIVEP_MALARIA_AGE,
                                        DT_YEAR=floor_date(SIVEP_MALARIA_AGE$DT_NOTIF, unit = "year"),
                                        DT_MONTH=floor_date(SIVEP_MALARIA_AGE$DT_NOTIF, unit = "month"),
                                        DT_WEEK=floor_date(SIVEP_MALARIA_AGE$DT_NOTIF, unit = "week")))
  
  if(Melted){
    #########################
    ## General Plots (Melted)
    #########################
    
    # Melt by date
    mSIVEP_MALARIA_AGE=melt(setDT(SIVEP_MALARIA_AGE),
                       measure.vars = list(c("DT_NOTIF",
                                             "DT_YEAR",
                                             "DT_MONTH",
                                             "DT_WEEK")),
                       variable.name = "DATE_TYPE", 
                       value.name = "DATE",
                       id.vars = c("LEVEL","CODE","TYPE","AGE_CAT","CASES"))
    
    # Reorder and relabel date type
    levels(mSIVEP_MALARIA_AGE$DATE_TYPE) = c("Daily","Yearly","Monthly","Weekly")
    mSIVEP_MALARIA_AGE$DATE_TYPE = factor(mSIVEP_MALARIA_AGE$DATE_TYPE,
                                     levels = c("Daily","Weekly","Monthly","Yearly"))
    
    return(mSIVEP_MALARIA_AGE)
    
  }else{
    
    return(SIVEP_MALARIA_AGE)
  }
}


##############################
## API CALCULATION FUNCTION ##
##############################

# Get function that calculates API by year
getAPI=function(TS, Level, Date_Type, Denominator){
  
  # Get population estimates by level (state or municipality)
  POP_EST_LEVEL=POP_EST[which(POP_EST$LEVEL == Level),]
  
  # Give BR code "0"
  TS[which(TS$LEVEL == "BR"),"CODE"] = "0"
  
  # Level of time series
  TS=TS[which(TS$LEVEL == Level),]
  
  # Select by date type
  TS=subset(TS, TS$DATE_TYPE == Date_Type)
  
  # Get year (by epi-week)
  TS$YEAR = NA
  TS$YEAR = ifelse((TS$DATE < as.Date("2003-01-01") & TS$DATE - as.Date("2003-01-01") >= -6), 2003, year(TS$DATE)) 
  
  # Keep variables and remove variables before aggregation
  TS=subset(TS, select = -c(DATE_TYPE))
  
  # Aggregate 
  TS_API=aggregate(CASES~., data = TS, FUN = sum, na.action = na.omit)
  
  # Get population of each level by year
  TS_API$POP=foreach(i=1:nrow(TS_API), .combine = "rbind") %do% {
    POP=ifelse(is.null(POP_EST_LEVEL[which(TS_API[i,"CODE"] == POP_EST_LEVEL[,"CODE"]),as.character(TS_API[i,"YEAR"])]),NA, POP_EST_LEVEL[which(TS_API[i,"CODE"] == POP_EST_LEVEL[,"CODE"]),as.character(TS_API[i,"YEAR"])])
  }
  
  # Get API
  TS_API$API=(TS_API$CASES/TS_API$POP)*Denominator
  
  # Assign names
  TS_API$STATE = ADMIN_NAMES[match(TS_API$CODE, ADMIN_NAMES$Code),"UF"] 
  TS_API$NAME = ADMIN_NAMES[match(TS_API$CODE, ADMIN_NAMES$Code),"Name"] 
  
  
  return(TS_API)
}


###################################
## Formatting and Stratification ##
###################################

# Get P. vivax time series in TS format
# getPV_TS_Format=function(TS, Type, StartYear, EndYear, Period, Frequency){
#   
#   # Get type
#   TS = TS[which(TS$TYPE == Type),]
#   
#   # Get by Period
#   TS = TS[which(TS$DATE_TYPE == Period),]
#   
#   # Weekly data
#   TS_Sum = aggregate(TS[,"CASES"], by = list(TS[,"DATE"]), sum)
#   # colnames(TS_Sum) = c(as.character(Period), "VIVAX")
#   
#   # Get TS format
#   TS_Formatted = ts(na.omit(TS_Sum$VIVAX), start = c(as.integer(StartYear),1), frequency=Frequency)
#   
#   return(TS_Formatted)
# }

# Get P. vivax time series in TS format and stratified by State
getPV_TS_Format_Stratified=function(TS, Type, StartYear, EndYear, Stratification, Period, Frequency){
  # Get type
  TS = TS[which(TS$TYPE == Type),]
  
  # Get by Period
  TS = TS[which(TS$DATE_TYPE == Period),]
  
  # By level
  TS = TS[which(TS$LEVEL == Stratification),]
  
  # Get state by state time series
  TS_Sum = aggregate(CASES ~ CODE+DATE, TS, sum)
  
  # Format
  TS_Formatted=by(TS, TS$CODE, FUN=function(TS) ts(TS$CASES, start = as.integer(StartYear), end = as.integer(EndYear), frequency=Frequency))  

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




