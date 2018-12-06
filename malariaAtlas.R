###################################################
### malariaAtlas: #################################
### an R interface to global malariometric data ###
### hosted by the Malaria Atlas Project ###########
###################################################

# install.packages("malariaAtlas")
library("malariaAtlas")


#####################
# Data Availability #

# Check availabilty of parasite rate (PR) survey point data
isAvailable_pr(country = "Brazil")

# Vector occurance (VO)
isAvailable_vec(country = "Brazil")


#################
# Parasite Rate #

# Full PvPR (parasite rate) Database
All_PvPfPR=getPR(country = "all", species = "both")
autoplot(All_PvPfPR)

# Brazil PvPR Database
BRA_PvPfPR=getPR(country = "Brazil", species = "both")
autoplot(BRA_PvPfPR)

# Full PvPR (parasite rate) Database
All_PvPR=getPR(country = "all", species = "Pv")
autoplot(All_PvPR)

# Brazil PvPR Database
BRA_PvPR=getPR(country = "Brazil", species = "Pv")
autoplot(BRA_PvPR)

# Get references
PvPR_Refs1=unique(BRA_PvPR$citation1)
PvPR_Refs2=unique(BRA_PvPR$citation2)
PvPR_Refs3=unique(BRA_PvPR$citation3)

####################
# Vector Occurance #

# Brazil PvVO Database
BRA_VO=getVecOcc(country = "Brazil")
autoplot(BRA_VO)

# Get references
BRA_VO_Refs=unique(BRA_VO$citation)

# Frequency of species
prop.table(table(BRA_VO$species_plain))
# Top 3:  Anopheles darlingi (37.5%)
#         Anopheles albitarsis (33.3%)
#         Anopheles nuneztovari (20.4%)
#         Other (8.8%)

###############
# Shape Files #

# Get shape file for Brazil

# # admin0
BRA_shp_lvl0_df=getShp(country = "Brazil", admin_level = "admin0", format = "df")
BRA_shp_lvl0_sp=getShp(country = "Brazil", admin_level = "admin0", format = "spatialpolygon")
# # Plot
autoplot(BRA_shp_lvl0_df)
# # Save
# save(BRA_shp_lvl0_df, file="BRA_shp_lvl0_df.RData")
# save(BRA_shp_lvl0_sp, file="BRA_shp_lvl0_sp.RData")
# Load
# load(file = "BRA_shp_lvl0_df.RData")
# load(file = "BRA_shp_lvl0_sp.RData")

# # admin1
BRA_shp_lvl1_df=getShp(country = "Brazil", admin_level = "admin1", format = "df")
BRA_shp_lvl1_sp=getShp(country = "Brazil", admin_level = "admin1", format = "spatialpolygon")
# # Plot
autoplot(BRA_shp_lvl1_df)
# # Save
# save(BRA_shp_lvl1_df, file="BRA_shp_lvl1_df.RData")
# save(BRA_shp_lvl1_sp, file="BRA_shp_lvl1_sp.RData")
# Load
# load(file = "BRA_shp_lvl1_df.RData")
# load(file = "BRA_shp_lvl1_sp.RData")

# # admin2
BRA_shp_lvl2_df=getShp(country = "Brazil", admin_level = "admin2", format = "df")
BRA_shp_lvl2_sp=getShp(country = "Brazil", admin_level = "admin2", format = "spatialpolygon")
# # Plot
autoplot(BRA_shp_lvl2_df)
# # Save
# save(BRA_shp_lvl2_df, file="BRA_shp_lvl2_df.RData")
# save(BRA_shp_lvl2_sp, file="BRA_shp_lvl2_sp.RData")
# Load
# load(file = "BRA_shp_lvl2_df.RData")
# load(file = "BRA_shp_lvl2_sp.RData")

# admin3: checked and same as admin2 = municipalities

###########
# Rasters #

# Available rasters
available_rasters=listRaster()

# Create function to get rasters more easily
getRasterPlots=function(title, shp, df){
  
  cat(paste("Get raster for", title, "\n"))
  rst=getRaster(surface = title,
                    shp = shp)
  
  # Save
  # save(rst, file="rst.RData")
  
  cat("Convert to datafame\n")
  rst_df=as.MAPraster(rst)
  
  # Save
  # save(rst_df, file = "rst_df.RData")
  
  cat("Map raster\n")
  plot=autoplot(rst_df, shp_df = df)
  
  return(plot)
}

#################################
# Get raster for Pv endemicity

PvEnd=getRasterPlots(title = "Plasmodium vivax Endemicity",
                     shp = BRA_shp_lvl1_sp,
                     df = BRA_shp_lvl1_df)
#Plot
PvEnd

#Get description
available_rasters[which(available_rasters$title=="Plasmodium vivax Endemicity"),4]

#Add PR layer
PvEnd_PR <- getPR(country = c("Brazil"), species = "Pv")

#Combine
PvEnd[[1]] +
  geom_point(data = PvEnd_PR, aes(longitude, latitude, fill = positive / examined, size = examined), shape = 21)+
  scale_size_continuous(name = "Survey Size")+
  ggtitle("Raw PvPR Survey points + Plasmodium vivax Endemicity")


#################
# G6PD deficiency

G6PDd=getRasterPlots(title = "G6PD Deficiency Allele Frequency",
                     shp = BRA_shp_lvl1_sp,
                     df = BRA_shp_lvl1_df)
#Plot
G6PDd

#Get description
available_rasters[which(available_rasters$title=="G6PD Deficiency Allele Frequency"),4]


##################
# Dominant Vectors

DVec=getRasterPlots(title = "Dominant Vectors",
                     shp = BRA_shp_lvl1_sp,
                     df = BRA_shp_lvl1_df)
#Plot
DVec

#Get description
available_rasters[which(available_rasters$title=="Dominant Vectors"),4]

##################
# Vectors

#Plot vector surveys
VectorSurveys=autoplot(BRA_PvVO)

#Vector1
Vec1=getRasterPlots(title = "Anopheles albitarsis species complex",
                    shp = BRA_shp_lvl1_sp,
                    df = BRA_shp_lvl1_df)
#Plot
Vec1

#Vector2
Vec2=getRasterPlots(title = "Anopheles aquasalis Curry, 1932",
                    shp = BRA_shp_lvl1_sp,
                    df = BRA_shp_lvl1_df)
#Plot
Vec2

#Vector3
Vec3=getRasterPlots(title = "Anopheles darlingi Root, 1926",
                    shp = BRA_shp_lvl1_sp,
                    df = BRA_shp_lvl1_df)
#Plot
Vec3

#Vector4
Vec4=getRasterPlots(title = "Anopheles marajoara Galv?o & Damasceno, 1942",
                    shp = BRA_shp_lvl1_sp,
                    df = BRA_shp_lvl1_df)
#Plot
Vec4

#Vector5
Vec5=getRasterPlots(title = "Anopheles nuneztovari species complex",
                    shp = BRA_shp_lvl1_sp,
                    df = BRA_shp_lvl1_df)
#Plot
Vec5






