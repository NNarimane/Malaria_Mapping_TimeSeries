#####################
#### BRAZIL MAPS ####
#####################

library("malariaAtlas")
library("data.table")
library("plyr")
library("RColorBrewer")
library("dplyr")


setwd("C:/Users/nnekkab/Desktop/MAPS")

############################################
## Upload Census and Population Estimates ##

# 2010 Census Data
BRA_POP_2010=read.csv("BRA_POP_2010.csv", stringsAsFactors = F)

# 2011-2018 Population Estimates
BRA_POP_2018_2011=read.csv("BRA_POP_2011_2018.csv", stringsAsFactors = F)

# Merge two datasets
BRA_POP=merge(BRA_POP_2010, BRA_POP_2018_2011, by=c("Level","Code","Name"), all.x=T)
rm(BRA_POP_2010, BRA_POP_2018_2011)

# Fix all names (remove accents)
BRA_POP$Name=iconv(BRA_POP$Name, to='ASCII//TRANSLIT')


#################
## State-Level ##

# State-level shape files
BRA_shp_lvl1_df=getShp(country = "Brazil", admin_level = "admin1", format = "df")

# Plot
autoplot(BRA_shp_lvl1_df)

# State-level population (UF)
BRA_POP_UF=BRA_POP[which(BRA_POP$Level=="UF"),]

# Alphabetical order
BRA_POP_UF=BRA_POP_UF[order(BRA_POP_UF$Name),]
# Factor
BRA_POP_UF$Name=as.factor(BRA_POP_UF$Name)

# Add population data to shape data
BRA_shp_lvl1_df_POP=merge(BRA_shp_lvl1_df, 
                          BRA_POP_UF[,3:ncol(BRA_POP_UF)], 
                          by.x = "name_1",
                          by.y = "Name",
                          all.x = T
)

# Melt data
setDT(BRA_shp_lvl1_df_POP)
BRA_shp_lvl1_df_POP_melted=melt(BRA_shp_lvl1_df_POP,
                                measure.vars = list(c("X2010","X2011",
                                                      "X2012","X2013",
                                                      "X2014","X2015",
                                                      "X2016","X2017",
                                                      "X2018")),
                                variable.name = "Year", 
                                value.name = "Population",
                                id.vars = 1:23)

# Create population categorical variable
BRA_shp_lvl1_df_POP_melted$Population <- cut(as.numeric(BRA_shp_lvl1_df_POP_melted$Population), breaks=c(-Inf, 500000, 1000000, 5000000, 10000000, 20000000,  Inf), 
                                          labels=c("< 500 000","< 1 000 000","< 5 000 000",
                                                   "< 10 000 000", "< 20 000 000", "> 20 000 000"))

# Fix Year labels
levels(BRA_shp_lvl1_df_POP_melted$Year)=as.character(seq(2010,2018))

# Plot
BRA_POP_Plot_LVL1=ggplot(BRA_shp_lvl1_df_POP_melted) +
  aes(long, lat, group=group, fill = Population) +
  scale_fill_brewer(palette="Blues") +
  coord_equal() +
  geom_polygon(colour = "black", size = 0.5) +
  facet_wrap(facets= Year ~.) +
  labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica")


#Path
SavePath=file.path("BRA_POP_MAP_2010_2018.jpeg")
#Plot
jpeg(SavePath, width = 30, height = 30, units = 'cm', res = 300)
print(BRA_POP_Plot_LVL1)
dev.off()

###################
## Municip-Level ##

# Get population plots at municipality-level
getBRA_plot=function(Year){
  # Large files so select year of map:
  Year=Year
  
  # # Get municipality shape files
  BRA_shp_lvl2=getShp(country = "Brazil", admin_level = "admin2", format = "df")
  
  # Upload State acronyms
  StateAbbrev=read.csv(file="StateAbbrev.csv")
  # Remove accents
  StateAbbrev$name=iconv(StateAbbrev$name, to='ASCII//TRANSLIT')
  # Keep only state name and acronym
  StateAbbrev=StateAbbrev[,c("subdivision","name")]
  
  # Select municipality-level population
  BRA_POP_MU=BRA_POP[which(BRA_POP$Level=="MU"),]
  # Change Name to name2
  colnames(BRA_POP_MU)[3]="name2"
  # Select only one year
  BRA_POP_MU=BRA_POP_MU[,c("name2", paste0("X", Year))]
  
  # Need to match by Name and Name1
  # Step 1a: add abbrev to population data to distiguish between MU of different states
  BRA_POP_MU$abbrev=substr(BRA_POP_MU$name2,nchar(BRA_POP_MU$name2)-2,nchar(BRA_POP_MU$name2)-1)
  # Step 1b: change abbrevation of state names to full name
  BRA_POP_MU=merge(BRA_POP_MU, StateAbbrev, by.x="abbrev", by.y="subdivision", all.x=T)
  # Step 1c: rename to name1
  colnames(BRA_POP_MU)[ncol(BRA_POP_MU)]="name1"
  # Step 2: remove (XX) string from Name
  BRA_POP_MU$name2 = substr(BRA_POP_MU$name2,1,nchar(BRA_POP_MU$name2)-5)
  # Step 3: check if name match
  table(BRA_POP_MU$name2 %in% BRA_shp_lvl2$name_2) # most in there, remove if False
  BRA_POP_MU=BRA_POP_MU[which(BRA_POP_MU$name2 %in% BRA_shp_lvl2$name_2),]
  # Step 4: fix X2011 column
  if(i==2011){BRA_POP_MU$X2011=as.integer(BRA_POP_MU$X2011)}
  # Check double names
  TestDup=ddply(BRA_POP_MU,c("name2","abbrev"),nrow) #none now
  
  ###########
  ## Clean-up
  
  rm(TestDup, StateAbbrev)
  gc()
  
  ########
  ## Merge
  
  # Need to merge shapefiles and population data with both MU name and State name
  # Step 1: make names into character temporarily
  BRA_shp_lvl2$name_1=as.character(BRA_shp_lvl2$name_1)
  BRA_shp_lvl2$name_2=as.character(BRA_shp_lvl2$name_2)
  # Step 2: merge (left_join will help keep in order)
  BRA_shp_lvl2=left_join(BRA_shp_lvl2, BRA_POP_MU, by = c("name_1" = "name1","name_2" = "name2"))
  # Step 3: reorder and change back to factors
  BRA_shp_lvl2$name_1=factor(BRA_shp_lvl2$name_1)
  BRA_shp_lvl2$name_2=factor(BRA_shp_lvl2$name_2)
  # Step 4: save/load
  # save(BRA_shp_lvl2, file=paste0("BRA_shp_lvl2_", Year, ".RData"))
  # load(file=paste0("BRA_shp_lvl2_", Year, ".RData"))
  
  ###########
  ## Clean-up
  
  rm(BRA_POP_MU)
  gc()
  
  #######
  ## Plot
  
  # Categorize population variable
  Population <- cut(as.numeric(BRA_shp_lvl2[,paste0("X",Year)]),
                    breaks=c(-Inf, 1000, 10000, 100000, 1000000, 10000000,  Inf),
                    labels=c("< 1 000","< 10 000","< 100 000",
                             "< 1 000 000", "< 10 000 000", "> 10 000 000"))
  
  
  # # With colors
  # BRA_plot=ggplot(data=BRA_shp_lvl2) +
  #   geom_polygon(color=NA, aes(long, lat, group=group, fill=Population)) +
  #   scale_fill_brewer(palette="Blues", na.value = "grey") +
  #   ggtitle(paste("Brazilian", Year, "Population")) +
  #   coord_equal() +
  #   theme_minimal() +
  #   labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica")
  
  # With colors
  BRA_plot=ggplot(data=BRA_shp_lvl2) +
    geom_polygon(color=NA, aes(long, lat, group=group, fill=Population)) +
    scale_fill_brewer(palette="Blues", na.value = "grey") +
    ggtitle(Year) +
    coord_equal() +
    theme_minimal() +
    theme(plot.title = element_text(size=22, hjust = 0.5),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  
  return(BRA_plot)
}

# Run for each year separately
for(i in seq(2010,2018)){
  #Run
  BRA_plot=getBRA_plot(i)
  #Path
  SavePath=file.path(paste0("POP_MAP_", i, ".png"))
  #Plot
  BRA_plot
  dev.copy(png, SavePath,
           width = 800, height = 800, units = "px", pointsize = 12,
           res = 100)
  dev.off()
  plot.new()
}

