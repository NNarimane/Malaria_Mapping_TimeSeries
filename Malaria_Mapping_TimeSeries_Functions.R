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
    rename(CODE_NOTIF = MUN_NOTI, CODE_RESID = MUN_RESI, CODE_INFEC = MUN_INFE, FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax") %>%
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
    rename(CODE_NOTIF = UF_NOTIF, CODE_RESID = UF_RESID, CODE_INFEC = UF_INFEC, FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax") %>%
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
    rename(FALCI = "Falciparum") %>%
    rename(FV = "V+F") %>%
    rename(VIVAX = "Vivax") %>%
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
    rename(CODE = MUN_NOTI, FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax", GENDER = SEXO) %>%
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
    rename(CODE = UF_NOTIF, FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax", GENDER = SEXO) %>%
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
    rename(FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax", GENDER = SEXO) %>%
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
    rename(CODE = MUN_NOTI, FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax", AGE_CAT = AGE_RANGE) %>%
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
    rename(CODE = UF_NOTIF, FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax", AGE_CAT = AGE_RANGE) %>%
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
    rename(FALCI = "Falciparum", FV = "V+F", VIVAX = "Vivax", AGE_CAT = AGE_RANGE) %>%
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

# Get SIVEP data cleaned up and by species and treatment
getSIVEP_MALARIA_TYPE_TREATMENT=function(FilePath){
  
  # Get SIVEP raw notification data
  load(FilePath)
  
  # Choose time period
  # df=df[which(df[,"DT_NOTIF"] >= paste0(StartYear,"-01-01") & df[,"DT_NOTIF"] <= paste0(EndYear, "-12-31")),]
  
  cat("Aggregate by type, gender, age, treatment\n")
  SIVEP_TREAT = df %>%
    select(DT_NOTIF, RES_EXAM, SEXO, GESTANTE_,AGE_CAT, ESQUEMA) %>% 
    mutate(YEAR = year(DT_NOTIF)) %>% 
    select(-DT_NOTIF) %>%
    mutate(ESQUEMA = replace(ESQUEMA, ESQUEMA == "N/A", NA)) %>%
    mutate(GESTANTE = as.character(GESTANTE_)) %>%
    mutate(GESTANTE = replace(GESTANTE, is.na(GESTANTE), "NA")) %>%
    mutate(GESTANTE = replace(GESTANTE, which(GESTANTE=="NO"), "NA")) %>%
    mutate(GESTANTE = replace(GESTANTE, which(GESTANTE=="Ignored"), "NA")) %>%
    mutate(GESTANTE = as.factor(GESTANTE)) %>%
    mutate(PREGNANT = as.character(GESTANTE_)) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="1qtr"), "Yes")) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="2qtr"), "Yes")) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="3qtr"), "Yes")) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="Ignored" & SEXO=="Falciparum"), "Yes")) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="Ignored" & SEXO=="I"), "No")) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="Ignored" & SEXO=="M"), "No")) %>%
    mutate(PREGNANT = replace(PREGNANT, is.na(GESTANTE_), "No")) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="NO"), "No")) %>%
    mutate(PREGNANT = replace(PREGNANT, which(GESTANTE_=="NA"), "No")) %>%
    select(YEAR, RES_EXAM, SEXO, GESTANTE, PREGNANT, AGE_CAT, ESQUEMA) %>%
    group_by(YEAR, RES_EXAM, SEXO, GESTANTE, PREGNANT, AGE_CAT, ESQUEMA) %>%
    count(RES_EXAM) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "Falciparum") %>%
    rename(FV = "V+F") %>%
    rename(VIVAX = "Vivax") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select(YEAR, SEXO, GESTANTE, PREGNANT, AGE_CAT, ESQUEMA, Falciparum, Vivax) %>%
    gather(key = 'TYPE', value = 'CASES', -c(YEAR, SEXO, GESTANTE, PREGNANT, AGE_CAT, ESQUEMA))
  
  # Recode ESQUEMA
  SIVEP_TREAT$ESQUEMA=as.character(SIVEP_TREAT$ESQUEMA)
  SIVEP_TREAT$ESQUEMA=recode(SIVEP_TREAT$ESQUEMA,
                             "1" = "Pv/Po:CQ-3d + PQ-7d",
                             "2" = "Pv/Po:CQ-3d + PQ-14d",
                             "3" = "Pv/Po preg & <6mo & Pm all ages: CQ-3d",
                             "4" = "Pv/Po: CQ-12weeks",
                             "5" = "Pf:AM+LF-3d",
                             "6" = "Pf:AS+MQ-3d",
                             "7" = "Pf:QQ-1dose + DC-1dose + PQ-1dose",
                             "8" = "Pf/Po/Pv: AM+LF or AS+MF-3d + PQ-7d",
                             "9" = "Pf 1st trimester & age<6mo:QQ-3d & CM-5d",
                             "10" = "Severe Pf",
                             "11" = "Pf:AM+LF-3d + PQ-1dose",
                             "12" = "Pf:AS+MQ-3d + PQ-1dose",
                             "83" = "*Pv/Pf:MQ-1dose + PQ-7d",
                             "85" = "*Pv children:AS-capsule-4d + PQ-7d",
                             "86" = "*Pf:MQ-1dose + PQ-1dose",
                             "87" = "*Pf:QQ-1dose",
                             "88" = "*Pf children:AS-capsule-4d + MQ-3d + PQ-1dose",
                             "89" = "*Pv/Pf:QQ-1dose + DC-1dose + PQ-1dose",
                             "99" = "Other scheme")
  SIVEP_TREAT$ESQUEMA[which(SIVEP_TREAT$ESQUEMA == "123")] = NA
  SIVEP_TREAT$ESQUEMA=as.factor(SIVEP_TREAT$ESQUEMA)
  SIVEP_TREAT$TYPE=as.factor(SIVEP_TREAT$TYPE)
  
  # Reorder
  SIVEP_TREAT$ESQUEMA=factor(SIVEP_TREAT$ESQUEMA, 
                             levels=c("Pv/Po:CQ-3d + PQ-7d",
                                      "Pv/Po:CQ-3d + PQ-14d",
                                      "Pv/Po: CQ-12weeks",
                                      "*Pv children:AS-capsule-4d + PQ-7d",
                                      "Pv/Po preg & <6mo & Pm all ages: CQ-3d",
                                      "Pv/Po/Pf: AM+LF or AS+MF-3d + PQ-7d",
                                      "*Pv/Pf:MQ-1dose + PQ-7d",
                                      "*Pv/Pf:QQ-1dose + DC-1dose + PQ-1dose",
                                      "Pf:QQ-1dose + DC-1dose + PQ-1dose",
                                      "Pf:AM+LF-3d + PQ-1dose",
                                      "Pf:AS+MQ-3d + PQ-1dose", 
                                      "*Pf:MQ-1dose + PQ-1dose",
                                      "Pf:AM+LF-3d",
                                      "Pf:AS+MQ-3d",
                                      "*Pf:QQ-1dose",
                                      "Pf 1st trimester & age<6mo:QQ-3d & CM-5d",
                                      "*Pf children:AS-capsule-4d + MQ-3d + PQ-1dose",
                                      "Severe Pf",
                                      "Other scheme"))
  
  # Primaquine
  SIVEP_TREAT$PQ=SIVEP_TREAT$ESQUEMA
  SIVEP_TREAT$PQ=recode_factor(SIVEP_TREAT$PQ, 
                               `Pv/Po:CQ-3d + PQ-7d` = "Yes",
                               `Pv/Po:CQ-3d + PQ-14d` = "Yes",
                               `Pv/Po: CQ-12weeks` = "No",
                               `*Pv children:AS-capsule-4d + PQ-7d` = "Yes",
                               `Pv/Po preg & <6mo & Pm all ages: CQ-3d` = "No",
                               `Pv/Po/Pf: AM+LF or AS+MF-3d + PQ-7d` = "Yes",
                               `*Pv/Pf:MQ-1dose + PQ-7d` = "Yes",
                               `*Pv/Pf:QQ-1dose + DC-1dose + PQ-1dose` = "Yes",
                               `Pf:QQ-1dose + DC-1dose + PQ-1dose` = "Yes",
                               `Pf:AM+LF-3d + PQ-1dose` = "Yes",
                               `Pf:AS+MQ-3d + PQ-1dose` = "Yes", 
                               `*Pf:MQ-1dose + PQ-1dose` = "Yes",
                               `Pf:AM+LF-3d` = "No",
                               `Pf:AS+MQ-3d` = "No",
                               `*Pf:QQ-1dose` = "No",
                               `Pf 1st trimester & age<6mo:QQ-3d & CM-5d` = "No",
                               `*Pf children:AS-capsule-4d + MQ-3d + PQ-1dose` = "Yes",
                               `Severe Pf` = "Unknown",
                               `Other scheme` = "Unknown")
  
  # Dose of PQ
  SIVEP_TREAT$PQ_Amount=SIVEP_TREAT$ESQUEMA
  SIVEP_TREAT$PQ_Amount=recode_factor(SIVEP_TREAT$PQ_Amount, 
                                      `Pv/Po:CQ-3d + PQ-7d` = "7-day",
                                      `Pv/Po:CQ-3d + PQ-14d` = "14-day",
                                      `Pv/Po: CQ-12weeks` = "None",
                                      `*Pv children:AS-capsule-4d + PQ-7d` = "7-day",
                                      `Pv/Po preg & <6mo & Pm all ages: CQ-3d` = "None",
                                      `Pv/Po/Pf: AM+LF or AS+MF-3d + PQ-7d` = "7-day",
                                      `*Pv/Pf:MQ-1dose + PQ-7d` = "7-day",
                                      `*Pv/Pf:QQ-1dose + DC-1dose + PQ-1dose` = "1-dose",
                                      `Pf:QQ-1dose + DC-1dose + PQ-1dose` = "1-dose",
                                      `Pf:AM+LF-3d + PQ-1dose` = "1-dose",
                                      `Pf:AS+MQ-3d + PQ-1dose` = "1-dose", 
                                      `*Pf:MQ-1dose + PQ-1dose` = "1-dose",
                                      `Pf:AM+LF-3d` = "None",
                                      `Pf:AS+MQ-3d` = "None",
                                      `*Pf:QQ-1dose` = "None",
                                      `Pf 1st trimester & age<6mo:QQ-3d & CM-5d` = "None",
                                      `*Pf children:AS-capsule-4d + MQ-3d + PQ-1dose` = "1-dose",
                                      `Severe Pf` = "Unknown",
                                      `Other scheme` = "Unknown")
  
  return(SIVEP_TREAT)
  
}

# Get importation tables at country level
getSIVEP_MALARIA_TYPE_COUNTRY=function(RES_OR_INF, TYPE){
  
  cat("Aggregate by type and country\n")
  SIVEP_PAIS = df %>%
    select_("DT_NOTIF", RES_OR_INF, "RES_EXAM") %>% 
    mutate(YEAR = year(DT_NOTIF)) %>% 
    select(-DT_NOTIF) %>%
    group_by_("YEAR", RES_OR_INF) %>%
    count(RES_EXAM) %>%
    spread(RES_EXAM, n, fill = 0) %>%
    rename(FALCI = "Falciparum") %>%
    rename(FV = "V+F") %>%
    rename(VIVAX = "Vivax") %>%
    mutate(Falciparum = FALCI + FV) %>%
    mutate(Vivax = VIVAX + FV) %>%
    select_("YEAR", RES_OR_INF, TYPE) %>%
    spread_(key = "YEAR", value = TYPE)
  
  if(RES_OR_INF == "PAIS_RES"){
    
    cat("By resident country\n")
    
    if(byTOP_COUNTRIES){
      
      cat("By top countries\n")
      
      # Get names
      SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      SIVEP_PAIS$PAIS_RES = PAIS_CODE[match(SIVEP_PAIS$PAIS_RES, PAIS_CODE$PAIS_CODE),"PAIS"]
      SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_RES),]
      
      # # Combine France and French Guyana
      # SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES == "GUIANA FRANCESA"),]=c("GUIANA FRANCESA",(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES == "FRANCA"),2:ncol(SIVEP_PAIS)] +
      #                                                                                     SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES == "GUIANA FRANCESA"),2:ncol(SIVEP_PAIS)]))
      # SIVEP_PAIS=SIVEP_PAIS[-which(SIVEP_PAIS$PAIS_RES == "FRANCA"),]
      
      # Get other category
      OTHER=t(as.data.frame(c(OTHER="OTHER", colSums(SIVEP_PAIS[!(SIVEP_PAIS$PAIS_RES %in% Top_Countries_BR),2:ncol(SIVEP_PAIS)]))))
      OTHER=data.frame(OTHER, check.names = F, stringsAsFactors = F)
      colnames(OTHER)=colnames(SIVEP_PAIS)
      rownames(OTHER)=1
      OTHER[,2:ncol(OTHER)]=as.numeric(OTHER[,2:ncol(OTHER)])
      
      # Get prop table with Brazil
      SIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES %in% Top_Countries_BR),],OTHER)
      pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      pSIVEP_PAIS=cbind(PAIS_RES = SIVEP_PAIS$PAIS_RES,pSIVEP_PAIS)
      
      # Get prop table without Brazil
      sSIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES %in% Top_Countries),],OTHER)
      pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS[,-1]), 2))
      pwbSIVEP_PAIS=cbind(PAIS_RES = sSIVEP_PAIS$PAIS_RES,pwbSIVEP_PAIS)
      
    }else{
      
      cat("By all countries\n")
      
      # Get names
      SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      SIVEP_PAIS$PAIS_RES = PAIS_CODE[match(SIVEP_PAIS$PAIS_RES, PAIS_CODE$PAIS_CODE),"PAIS"]
      SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_RES),]
      
      # # Combine France and French Guyana
      # SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES == "GUIANA FRANCESA"),]=c("GUIANA FRANCESA",(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES == "FRANCA"),2:ncol(SIVEP_PAIS)] +
      #                                                                                     SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES == "GUIANA FRANCESA"),2:ncol(SIVEP_PAIS)]))
      # SIVEP_PAIS=SIVEP_PAIS[-which(SIVEP_PAIS$PAIS_RES == "FRANCA"),]
      
      # Get prop table with Brazil
      pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      pSIVEP_PAIS=cbind(PAIS_RES = SIVEP_PAIS$PAIS_RES,pSIVEP_PAIS)
      
      # Get prop table without Brazil
      pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[-1,-1]), 2))
      pwbSIVEP_PAIS=cbind(PAIS_RES = SIVEP_PAIS$PAIS_RES[-1],pwbSIVEP_PAIS)
    }
   
  }else{
    
    cat("By infection country\n")
    
    if(byTOP_COUNTRIES){
      
      cat("By top countries\n")
      
      # Get names
      SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      SIVEP_PAIS$PAIS_INF = PAIS_CODE[match(SIVEP_PAIS$PAIS_INF, PAIS_CODE$PAIS_CODE),"PAIS"]
      SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_INF),]
      
      # # Combine France and French Guyana
      # SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF == "GUIANA FRANCESA"),]=c("GUIANA FRANCESA",(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF == "FRANCA"),2:ncol(SIVEP_PAIS)] +
      #                                                                  SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF == "GUIANA FRANCESA"),2:ncol(SIVEP_PAIS)]))
      # SIVEP_PAIS=SIVEP_PAIS[-which(SIVEP_PAIS$PAIS_INF == "FRANCA"),]
      
      # Get other category
      OTHER=t(as.data.frame(c(OTHER="OTHER", colSums(SIVEP_PAIS[!(SIVEP_PAIS$PAIS_INF %in% Top_Countries_BR),2:ncol(SIVEP_PAIS)]))))
      OTHER=data.frame(OTHER, check.names = F, stringsAsFactors = F)
      colnames(OTHER)=colnames(SIVEP_PAIS)
      rownames(OTHER)=1
      OTHER[,2:ncol(OTHER)]=as.numeric(OTHER[,2:ncol(OTHER)])
      
      # Get prop table with Brazil
      SIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF %in% Top_Countries_BR),],OTHER)
      pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      pSIVEP_PAIS=cbind(PAIS_INF = SIVEP_PAIS$PAIS_INF,pSIVEP_PAIS)
      
      # Get prop table without Brazil
      sSIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF %in% Top_Countries),],OTHER)
      pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS[,-1]), 2))
      pwbSIVEP_PAIS=cbind(PAIS_INF = sSIVEP_PAIS$PAIS_INF,pwbSIVEP_PAIS)
      
    }else{
      
      cat("By all countries\n")
      
      # Get names
      SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      SIVEP_PAIS$PAIS_INF = PAIS_CODE[match(SIVEP_PAIS$PAIS_INF, PAIS_CODE$PAIS_CODE),"PAIS"]
      SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_INF),]
      
      # # Combine France and French Guyana
      # SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF == "GUIANA FRANCESA"),]=c("GUIANA FRANCESA",(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF == "FRANCA"),2:ncol(SIVEP_PAIS)] +
      #                                                                                     SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF == "GUIANA FRANCESA"),2:ncol(SIVEP_PAIS)]))
      # SIVEP_PAIS=SIVEP_PAIS[-which(SIVEP_PAIS$PAIS_INF == "FRANCA"),]
      
      # Get prop table with Brazil
      pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      pSIVEP_PAIS=cbind(PAIS_INF = SIVEP_PAIS$PAIS_INF,pSIVEP_PAIS)
      
      # Get prop table without Brazil
      pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[-1,-1]), 2))
      pwbSIVEP_PAIS=cbind(PAIS_INF = SIVEP_PAIS$PAIS_INF[-1],pwbSIVEP_PAIS)
    }
  }
  return(list(SIVEP_PAIS, pSIVEP_PAIS, pwbSIVEP_PAIS))
}

########################## Fix
getSIVEP_MALARIA_TYPE_STATE=function(RES_OR_INF, TYPE){
  
  if(RES_OR_INF == "PAIS_RES"){
    
    cat("By resident state\n")
    
    if(byTOP_COUNTRIES){
      
      cat("By top countries\n")
      
      # # Get names
      # SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      # SIVEP_PAIS$PAIS_RES = PAIS_CODE[match(SIVEP_PAIS$PAIS_RES, PAIS_CODE$PAIS_CODE),"PAIS"]
      # SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_RES),]
      # 
      # # Get other category
      # OTHER=t(as.data.frame(c(OTHER="OTHER", colSums(SIVEP_PAIS[!(SIVEP_PAIS$PAIS_RES %in% Top_Countries_BR),2:ncol(SIVEP_PAIS)]))))
      # OTHER=data.frame(OTHER, check.names = F, stringsAsFactors = F)
      # colnames(OTHER)=colnames(SIVEP_PAIS)
      # rownames(OTHER)=1
      # OTHER[,2:ncol(OTHER)]=as.numeric(OTHER[,2:ncol(OTHER)])
      # 
      # # Get prop table with Brazil
      # SIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES %in% Top_Countries_BR),],OTHER)
      # pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      # pSIVEP_PAIS=cbind(PAIS_RES = SIVEP_PAIS$PAIS_RES,pSIVEP_PAIS)
      # 
      # # Get prop table without Brazil
      # sSIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_RES %in% Top_Countries),],OTHER)
      # pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS[,-1]), 2))
      # pwbSIVEP_PAIS=cbind(PAIS_RES = sSIVEP_PAIS$PAIS_RES,pwbSIVEP_PAIS)
      
    }else{
      
      cat("By all countries\n")
      
      cat("Aggregate by type and state\n")
      SIVEP_UF = df %>%
        select(DT_NOTIF, UF_NOTIF, UF_RESID, RES_EXAM) %>% 
        mutate(YEAR = year(DT_NOTIF)) %>% 
        select(-DT_NOTIF) %>%
        group_by(YEAR, UF_NOTIF, UF_RESID) %>%
        count(RES_EXAM) %>%
        spread(RES_EXAM, n, fill = 0) %>%
        rename(FALCI = "Falciparum") %>%
        rename(FV = "V+F") %>%
        rename(VIVAX = "Vivax") %>%
        mutate(Falciparum = FALCI + FV) %>%
        mutate(Vivax = VIVAX + FV) %>%
        select(YEAR, UF_NOTIF, UF_RESID, Vivax) %>%
        spread(key = YEAR, value = Vivax)
      
      # # Get names
      # SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      # SIVEP_PAIS$PAIS_RES = PAIS_CODE[match(SIVEP_PAIS$PAIS_RES, PAIS_CODE$PAIS_CODE),"PAIS"]
      # SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_RES),]
      # 
      # # Get prop table with Brazil
      # pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      # pSIVEP_PAIS=cbind(PAIS_RES = SIVEP_PAIS$PAIS_RES,pSIVEP_PAIS)
      # 
      # # Get prop table without Brazil
      # pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[-1,-1]), 2))
      # pwbSIVEP_PAIS=cbind(PAIS_RES = SIVEP_PAIS$PAIS_RES[-1],pwbSIVEP_PAIS)
    }
    
  }else{
    
    cat("By infection country\n")
    
    if(byTOP_COUNTRIES){
      
      cat("By top countries\n")
      
      # Get names
      SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      SIVEP_PAIS$PAIS_INF = PAIS_CODE[match(SIVEP_PAIS$PAIS_INF, PAIS_CODE$PAIS_CODE),"PAIS"]
      SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_INF),]
      
      # Get other category
      OTHER=t(as.data.frame(c(OTHER="OTHER", colSums(SIVEP_PAIS[!(SIVEP_PAIS$PAIS_INF %in% Top_Countries_BR),2:ncol(SIVEP_PAIS)]))))
      OTHER=data.frame(OTHER, check.names = F, stringsAsFactors = F)
      colnames(OTHER)=colnames(SIVEP_PAIS)
      rownames(OTHER)=1
      OTHER[,2:ncol(OTHER)]=as.numeric(OTHER[,2:ncol(OTHER)])
      
      # Get prop table with Brazil
      SIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF %in% Top_Countries_BR),],OTHER)
      pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      pSIVEP_PAIS=cbind(PAIS_INF = SIVEP_PAIS$PAIS_INF,pSIVEP_PAIS)
      
      # Get prop table without Brazil
      sSIVEP_PAIS=bind_rows(SIVEP_PAIS[which(SIVEP_PAIS$PAIS_INF %in% Top_Countries),],OTHER)
      pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(sSIVEP_PAIS[,-1]), 2))
      pwbSIVEP_PAIS=cbind(PAIS_INF = sSIVEP_PAIS$PAIS_INF,pwbSIVEP_PAIS)
      
    }else{
      
      cat("By all countries\n")
      
      # Get names
      SIVEP_PAIS[is.na(SIVEP_PAIS)] = 0
      SIVEP_PAIS$PAIS_INF = PAIS_CODE[match(SIVEP_PAIS$PAIS_INF, PAIS_CODE$PAIS_CODE),"PAIS"]
      SIVEP_PAIS=SIVEP_PAIS[complete.cases(SIVEP_PAIS$PAIS_INF),]
      
      # Get prop table with Brazil
      pSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[,-1]), 2))
      pSIVEP_PAIS=cbind(PAIS_INF = SIVEP_PAIS$PAIS_INF,pSIVEP_PAIS)
      
      # Get prop table without Brazil
      pwbSIVEP_PAIS=as.data.frame(prop.table(as.matrix(SIVEP_PAIS[-1,-1]), 2))
      pwbSIVEP_PAIS=cbind(PAIS_INF = SIVEP_PAIS$PAIS_INF[-1],pwbSIVEP_PAIS)
    }
  }
  return(list(SIVEP_PAIS, pSIVEP_PAIS, pwbSIVEP_PAIS))
}


# Get stacked box plots - country level
getSTACKED_BOX_PLOT=function(DATA, RES_OR_INF, y, title, legend_title){
  
  # melt the data frame for plotting
  mDATA <- melt(DATA, id.vars=RES_OR_INF)
  # levels(mDATA[,RES_OR_INF]) = levels(DATA[,RES_OR_INF]) 
  # mDATA$value=mDATA$value*100
  
  if(RES_OR_INF == "PAIS_RES"){
    if(byTOP_COUNTRIES){
      mDATA$PAIS_RES=as.character(mDATA$PAIS_RES)
      mDATA=mDATA[which(mDATA$PAIS_RES %in% Top_Countries),]
      mDATA$PAIS_RES=factor(mDATA$PAIS_RES)
      mDATA=setDT(mDATA)[ , PAIS_RES := factor(PAIS_RES, levels = Top_Countries)]
    }
    # Stacked
    mDATA_Plot=ggplot(mDATA, aes(y=value, x=variable, fill = PAIS_RES)) +   
      geom_bar(stat = "identity") +
      scale_fill_manual(values=Colors,
                        labels=names(Colors)) +
      theme_minimal() +
      labs(title=title, y=y, x="") +
      theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
            axis.title.y=element_text(size=12),
            legend.position="right",
            legend.title=element_text(size=12))  +
      guides(fill=guide_legend(title=legend_title))
  }else{
    if(byTOP_COUNTRIES){
      mDATA$PAIS_INF=as.character(mDATA$PAIS_INF)
      mDATA=mDATA[which(mDATA$PAIS_INF %in% Top_Countries),]
      mDATA$PAIS_INF=factor(mDATA$PAIS_INF)
      mDATA=setDT(mDATA)[ , PAIS_INF := factor(PAIS_INF, levels = Top_Countries)]
    }
    # Stacked
    mDATA_Plot=ggplot(mDATA, aes(y=value, x=variable, fill = PAIS_INF)) +   
      geom_bar(stat = "identity") +
      scale_fill_manual(values=Colors,
                        labels=names(Colors)) +
      theme_minimal() +
      labs(title=title, y=y, x="") +
      theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
            axis.title.y=element_text(size=12),
            legend.position="right",
            legend.title=element_text(size=12))  +
      guides(fill=guide_legend(title=legend_title))
  }
  
  return(mDATA_Plot)
}

# Get importation chord diagram - country and state
getCHORD_DIAGRAMS=function(LEVEL, DATA, RES_OR_INF){
  
  # Get edgelist by country
  Years=2003:2018
  COUNTRY_EDGELIST=foreach(i=1:length(Years)) %do% {
    Edgelist_Name=paste0("COUNTRY_EDGELIST",Years[i])
    Edgelist=assign(Edgelist_Name, 
                    setNames(data.frame(from = DATA[RES_OR_INF],
                                        to = "BRASIL",
                                        values = DATA[,as.character(Years[i])]),
                             c("to","from","weight")))
  }
  COUNTRY_EDGELIST=lapply(COUNTRY_EDGELIST, function(x) x[-1,])
  
  # Colors
  if(LEVEL == "COUNTRY"){
    Colors=c("slategray4", colorRampPalette(c("deeppink4","deeppink3",
                                              "darkorchid4","darkorchid3",
                                              "royalblue","cyan3",
                                              "aquamarine3","springgreen3",
                                              "mediumseagreen","darkolivegreen2",
                                              "gold","darkgoldenrod2",
                                              "darkorange","coral1","indianred2",
                                              "firebrick1","firebrick"))
             (nrow(COUNTRY_EDGELIST[[1]])))
    names(Colors)=c("BRASIL", as.character(COUNTRY_EDGELIST[[1]]$to))
  }
  if(LEVEL == "UF"){
    # Colors
    getColors=colorRampPalette(brewer.pal(10,"Spectral"))
    Colors=c(rev(getColors(length(unique(DATA$UF_NOTIF)))),rep("gray",length(unique(DATA$UF_RESID))-length(unique(DATA$UF_NOTIF))))
    names(Colors)=c(unique(DATA$UF_NOTIF),unique(DATA$UF_RESID[!DATA$UF_RESID %in% DATA$UF_NOTIF]))
  }
  
  
  # Plot 
  plot.new()
  par(mfrow=c(1,1))
  for(i in 1:length(Years)){
    circos.clear()
    circos.par(start.degree = 240, clock.wise = F, track.margin=c(-0.03,0.05))
    chordDiagram(COUNTRY_EDGELIST[[i]], 
                 grid.col = Colors, 
                 directional = -1,
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
    title(paste(title, "Brazil",as.character(2002+i)), line = -1, cex = 2, outer = F)
    
    # Save
    dev.copy(png, paste0(Plot_Folder, title, "Brazil ",as.character(2002+i),".png"),
             width = 1000, height = 1000, units = "px", pointsize = 12,
             res = 100)
    dev.off()
    
  }
}

########################################
## API AND RATIO CALCULATION FUNCTION ##
########################################

# Get PvPf ration function
getRATIO=function(TS){
  
  # UF-level
  sTS=subset(TS, select = c("CODE","DATE","TYPE","CASES"))
  sTS_Wide=reshape(sTS, idvar = c("CODE","DATE"), timevar = c("TYPE"), direction = "wide", v.names = "CASES")
  sTS_Wide$RATIO = sTS_Wide$CASES.Vivax / sTS_Wide$CASES.Falciparum
  
  # Remerge
  sTS_Long=reshape(sTS_Wide, idvar = c("CODE","DATE","RATIO"), v.names = "CASES",
                          timevar = c("TYPE"), direction = "long")
  
  mTS=merge(TS, sTS_Long, by = c("CODE","DATE","TYPE","CASES"), all.x = TRUE)
  
  return(mTS)
  
}

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
  
  # Get Ration
  TS_API_RATIO=getRATIO(TS_API)
  
  # Assign names
  TS_API_RATIO$STATE = ADMIN_NAMES[match(TS_API_RATIO$CODE, ADMIN_NAMES$Code),"UF"] 
  TS_API_RATIO$NAME = ADMIN_NAMES[match(TS_API_RATIO$CODE, ADMIN_NAMES$Code),"Name"] 
  
  
  return(TS_API_RATIO)
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




