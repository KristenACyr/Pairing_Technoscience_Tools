
library("viridis")
library(ggplot2)
library(dplyr)
library("ggalluvial")
library(stringr)
library("ggh4x")
library(forcats)
library(ggpubr)
library(tidyr)
library("DescTools")
library(sunburstR)

#Setting working directory for Masters Chapter folder
setwd("D:/Documents/Masters/Chapters/Chapter 1/Tables")


DM = read.csv("Dataframe_1.csv")


########Change not recorded to NA's to calculate
#Kept the NA's so I can count total NA;s in end##########
DM = DM %>%
  mutate(Depth_groups = na_if(Depth_groups, "Not Reported"),
         Length_Groups = na_if(Length_Groups, "Not Reported"))



DM = DM %>%
  mutate(Recording_Hours = na_if(Recording_Hours, "Not Reported"),
         Tracking_Days = na_if(Tracking_Days, "Not Reported"))


#############################################################

# TP = DM %>%
#   select(Manuscript) %>%
#   unique() %>%
#   count()
# 
# 
# 
# 
# 
# #Make new columns based off summary stats calculated for depth, size, 
# #tracking time and recording time
# #Make new column that groups deepest depth recorded
# levels <- c(-Inf, 3, 40, 6500)
# labels <- c("Surface to 3m", "3m to 40m", ">40m")
# DM = DM %>%
#   mutate(Depth_groups = cut(Max_C_Depth_m, levels, labels = labels)) 
# 
# 
# 
# #Grouped breaks for minimum length
# levels <- c(3.9, 24, 140, 1600)
# labels <- c("Small", "Medium", "Large")
# DM = DM %>%
#   mutate(Length_Groups = cut(Min_length_Tag_cm, levels, labels = labels))
# 
# 
# 
# 
# 
# 
# 
# 
# #Grouped breaks for max tracking time
# levels <- c(0.1, 5, 151, 1764)
# labels <- c("Short Term", "Medium Term", "Long Term")
# DM = DM %>%
#   mutate(Tracking_Days = cut(Max_Days_Tracked, levels, labels = labels))
# 
# 
# df = DM %>%
#   select(Max_Days_Tracked, Tracking_Days)
# 
# 
# #Grouped breaks for max recording time
# levels <- c(0.15, 3.95, 72, 369)
# labels <- c("Short term", "Medium Term", "Long Term")
# DM = DM %>%
#   mutate(Recording_Hours = cut(Max_Hours_Recorded, levels, labels = labels))
# 
# 
# 
# 




###Make a dataframe with columns that have grouped breaks for sunchart
Decision_sunchart = DM %>%
  select(Manuscript, Taxa, Sub_discipline, Discipline, Total_Field_Seasons,
         Water_Type,Depth_groups, Length_Groups, Tracking_Days, Recording_Hours, 
         Intermediate_Device_Pairing)



write.csv(Decision_sunchart, "Decision_Sunchart_R_EXPORT.csv",row.names = FALSE)

Decision_sunchart = Decision_sunchart %>%
  drop_na(Depth_groups|Length_Groups|Tracking_Days|Recording_Hours)
TP = Decision_sunchart %>%
  select(Manuscript) %>%
  unique() 

# write.csv(TP,
#           "D:/Documents/Masters/Chapters/Chapter 1/Tables/Summary_stats\\ManuINCinSUN.csv",
#           row.names=FALSE)

##Get rid of duplicates
Decision_sunchart = Decision_sunchart %>%
  group_by(Taxa, Water_Type, Discipline, Sub_discipline, Total_Field_Seasons,
           Depth_groups, Length_Groups, Tracking_Days, Recording_Hours, 
           Intermediate_Device_Pairing) %>%
  summarise(total = n_distinct(Manuscript))


Decision_sunchart$Intermediate_Device_Pairing = gsub("Animal-borne", "AnimalBorne", 
                                     Decision_sunchart$Intermediate_Device_Pairing)

Decision_sunchart$V1 = paste(Decision_sunchart$Taxa,"-", Decision_sunchart$Water_Type)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Discipline)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Sub_discipline)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Total_Field_Seasons)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Depth_groups)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Length_Groups)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Tracking_Days)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Recording_Hours)
Decision_sunchart$V1 = paste(Decision_sunchart$V1,"-", Decision_sunchart$Intermediate_Device_Pairing)

Decision_sunchart = Decision_sunchart %>%
  ungroup() %>%
  select(V1, total)




p = sunburst(
  Decision_sunchart,
  withD3 = TRUE,
  colors= htmlwidgets::JS("
function() {
  debugger
  // this will be d3 version 4
  const node = d3.select(this).datum()
  let color
  
  if(node.depth > 0) {  // 2nd level
    const ancestors = node.depth === 1 ? [node.parent, node] : node.ancestors().reverse()
    // inefficient to define every time but will do this way for now
    color = d3.scaleOrdinal(d3.schemeCategory10)
      .domain(ancestors[0].children.map(d=>d.data.name))
    return(d3.color(color(ancestors[1].data.name)).brighter((node.depth - 1)/4))
  }
}
  ")
)

p
ggsave('sunburst.png', plot = p)
