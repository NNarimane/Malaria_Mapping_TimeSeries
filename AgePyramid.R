######################
### Age Pyramids

library("reshape2")
library("ggplot2")
library("plyr")
library("dplyr")
library("RColorBrewer")

setwd("C:/Users/nnekkab/Desktop/MAPS")

############################################
## Upload Census and Population Estimates ##

# 2010 Census Data
BRA_AGE_SEX_MUNICIP_2010=read.csv("BRA_AGE_SEX_MUNICIP_2010.csv", stringsAsFactors = F, na.strings=c(""," ","-","NA"))

# Change age groups from Portuguese to English
BRA_AGE_SEX_MUNICIP_2010$Group=gsub(" a ", " to ", BRA_AGE_SEX_MUNICIP_2010$Group)
BRA_AGE_SEX_MUNICIP_2010$Group=gsub("anos", "years", BRA_AGE_SEX_MUNICIP_2010$Group)
BRA_AGE_SEX_MUNICIP_2010$Group=gsub("ou mais", "or more", BRA_AGE_SEX_MUNICIP_2010$Group)

# Identify groups to plot
Age_Groups=c("0 to 4 years","5 to 9 years","10 to 14 years","15 to 19 years","20 to 24 years",
             "25 to 29 years","30 to 34 years","35 to 39 years","40 to 44 years","45 to 49 years",
             "50 to 54 years","55 to 59 years","60 to 64 years","65 to 69 years","70 to 74 years",
             "75 to 79 years","80 to 84 years","85 to 89 years","90 to 94 years","95 to 99 years",
             "100 years or more")

# Melt to get one gender column
BRA_AGE_GENDER=melt(BRA_AGE_SEX_MUNICIP_2010, id.vars = c("Level","Code","Name","Group"),
                    measure.vars = c("Total","Men","Women"))
colnames(BRA_AGE_GENDER)=c("Level","Code","Name","Group","Gender","Population" )

# Select, factor and order age groups
BRA_AGE_GENDER=BRA_AGE_GENDER[which(BRA_AGE_GENDER$Group %in% Age_Groups),]
BRA_AGE_GENDER$Group=ordered(BRA_AGE_GENDER$Group, levels = Age_Groups)

# Make one gender negative population value
BRA_AGE_GENDER$Population <- ifelse(BRA_AGE_GENDER$Gender == "Men", -1*BRA_AGE_GENDER$Population, BRA_AGE_GENDER$Population)

###############
# Country-level
Age_Country=BRA_AGE_GENDER[which(BRA_AGE_GENDER$Level=="BR" & 
                                 BRA_AGE_GENDER$Gender!="Total"),]

###############
# Region-level
Age_Region=BRA_AGE_GENDER[which(BRA_AGE_GENDER$Level=="GR" & 
                                BRA_AGE_GENDER$Gender!="Total"),]
# Factor
Age_Region$Name=as.factor(Age_Region$Name)

###############
# State-level
Age_State=BRA_AGE_GENDER[which(BRA_AGE_GENDER$Level=="UF" & 
                              BRA_AGE_GENDER$Gender!="Total"),]
# Factor
Age_State$Name=as.factor(Age_State$Name)
# Remove two unknown states
Age_State=Age_State[which(Age_State$Name != "Fernando de Noronha" &
                          Age_State$Name != "Guanabara"),]


#######################
######## Plots ########
#######################

###############
# Country-level
Age_Country_Plot=ggplot(Age_Country, aes(x = Group, y = Population, fill = Gender)) + 
  geom_bar(data = subset(Age_Country, Gender == "Women"), stat = "identity") + 
  geom_bar(data = subset(Age_Country, Gender == "Men"), stat = "identity") + 
  scale_y_continuous(labels = paste0(as.character(c(seq(10, 0, -5), seq(5, 10, 5))), " million")) +
  # scale_fill_brewer(palette="Blues") +
  # scale_fill_manual(values=c("#466cb9", "#9fb5df")) +
  scale_fill_manual(values=c("#08519c","#2171b5")) +
  ggtitle(paste("Brazilian Population Pyramid, 2010")) +
  labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank())

#Plot
jpeg("BRA_AGE_PYRAMID_2010_COUNTRY.jpeg", width = 30, height = 30, units = 'cm', res = 300)
print(Age_Country_Plot)
dev.off()

#############
# Region

Age_Region_Plot=ggplot(Age_Region, aes(x = Group, y = Population, fill = Gender)) +
  geom_bar(data = subset(Age_Region, Gender == "Women"), stat = "identity") +
  geom_bar(data = subset(Age_Region, Gender == "Men"), stat = "identity") +
  scale_y_continuous(labels = paste0(as.character(c(seq(10, 0, -5), seq(5, 10, 5))), " million")) +
  scale_fill_manual(values=c("#466cb9", "#9fb5df")) +
  ggtitle(paste("Brazilian Population Pyramid, 2010 - Regions")) +
  labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        strip.text = element_text(face = "bold", size = 12)) +
  facet_wrap(facets= Name ~., nrow = 2) 

jpeg("BRA_AGE_PYRAMID_2010_REGION.jpeg", width = 30*2, height = 15*2, units = 'cm', res = 300)
print(Age_Region_Plot)
dev.off()

# ## Reorder by gender and region
# Age_Region$Gender=factor(Age_Region$Gender, levels = c("Men","Women"))
# Age_Region$Name=factor(Age_Region$Name, levels = (c("Sudeste","Nordeste","Sul","Norte","Centro-Oeste")))
# RegionOrder=c("Sudeste","Nordeste","Sul","Norte","Centro-Oeste")
# Age_Region=Age_Region[order(rev(Age_Region$Gender), rev(Age_Region$Name)),]
# 
# # Interaction=interaction(factor(Age_Region$Gender), Age_Region$Name)
# # levels(Interaction)
# # Interaction=ordered(Interaction, levels = c("Men.Centro-Oeste","Women.Centro-Oeste",
# #         "Men.Sul","Women.Sul",
# #         "Men.Nordeste","Women.Nordeste",
# #         "Men.Sudeste","Women.Sudeste",
# #         "Men.Norte","Women.Norte"))
# 
# 
# # Colors
# blues <- RColorBrewer::brewer.pal(5, "Blues")
# # blues
# # blues=c("#EFF3FF",rev(c("#6BAED6","#08519C","#BDD7E7","#3182BD")))
# oranges <- RColorBrewer::brewer.pal(5, "Oranges")
# oranges
# # oranges=c("#FEEDDE",rev(c("#A63603","#FDBE85","#E6550D","#E6550D")))
# 
# # Combined regions in one plot
# Age_Region_Combined_Plot=dplyr::mutate(Age_Region$Name = factor(Age_Region$Name, levels = rev(RegionOrder))) %>% 
#   ggplot(Age_Region, aes(x = Group, y = Population, fill = interaction(Gender, Name), order=interaction(factor(Gender), Name))) + 
#   geom_bar(data = subset(Age_Region, Gender == "Women"), stat = "identity", position = position_stack(reverse = F)) + 
#   geom_bar(data = subset(Age_Region, Gender == "Men"), stat = "identity", position = position_stack(reverse = F)) + 
#   scale_y_continuous(labels = paste0(as.character(c(seq(10, 0, -5), seq(5, 10, 5))), " million")) +
#   # scale_fill_brewer(palette="Blues") +
#   scale_fill_manual(values = c(blues, oranges)
#                     # labels = c("")
#                     ) +
#   ggtitle(paste("Brazilian Population Pyramid, 2010 - Regions")) +
#   labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica") +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         strip.text = element_text(face = "bold", size = 12)) +
#   guides(fill=guide_legend(title="Regions"))

Age_Region_Combined_Plot=ggplot(Age_Region, aes(x = Group, y = Population, fill = Name)) +
  geom_bar(data = subset(Age_Region, Gender == "Women"), stat = "identity", position = position_stack(reverse = T)) +
  geom_bar(data = subset(Age_Region, Gender == "Men"), stat = "identity", position = position_stack(reverse = T)) +
  scale_y_continuous(labels = paste0(as.character(c(seq(10, 0, -5), seq(5, 10, 5))), " million")) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle(paste("Brazilian Population Pyramid, 2010 - Regions")) +
  labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        strip.text = element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="Regions"))

jpeg("BRA_AGE_PYRAMID_2010_REGION_COMBINED.jpeg", width = 30, height = 30, units = 'cm', res = 300)
print(Age_Region_Combined_Plot)
dev.off()


###############
# State-level

Age_State_Plot=ggplot(Age_State, aes(x = Group, y = Population, fill = Gender)) + 
  geom_bar(data = subset(Age_State, Gender == "Women"), stat = "identity") + 
  geom_bar(data = subset(Age_State, Gender == "Men"), stat = "identity") + 
  scale_y_continuous(labels = paste0(as.character(c(seq(10, 0, -5), seq(5, 10, 5))), " million")) +
  scale_fill_manual(values=c("#466cb9", "#9fb5df")) +
  ggtitle(paste("Brazilian Population Pyramid, 2010 - States")) +
  labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        strip.text = element_text(face = "bold", size = 12)) +
  facet_wrap(facets= Name ~., ncol = 5)

jpeg("BRA_AGE_PYRAMID_2010_STATES.jpeg", width = 30*5, height = 15*6, units = 'cm', res = 300)
print(Age_State_Plot)
dev.off()

# Combined regions in one plot
colourCount = length(unique(Age_State$Name))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

Age_State_Combined_Plot=ggplot(Age_State, aes(x = Group, y = Population, fill = Name)) +
  geom_bar(data = subset(Age_State, Gender == "Women"), stat = "identity", position = position_stack(reverse = T)) +
  geom_bar(data = subset(Age_State, Gender == "Men"), stat = "identity", position = position_stack(reverse = T)) +
  scale_y_continuous(labels = paste0(as.character(c(seq(10, 0, -5), seq(5, 10, 5))), " million")) +
  scale_fill_manual(values = rev(getPalette(colourCount))) +
  ggtitle(paste("Brazilian Population Pyramid, 2010 - States")) +
  labs(caption = "Source: Instituto Brasileiro de Geografia e Estatistica") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        strip.text = element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="States"))

jpeg("BRA_AGE_PYRAMID_2010_STATE_COMBINED.jpeg", width = 30, height = 30, units = 'cm', res = 300)
print(Age_State_Combined_Plot)
dev.off()