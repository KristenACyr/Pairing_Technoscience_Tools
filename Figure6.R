library(ggsankey)
library("tokenizers")
library(dplyr)
library(tidyr)
library(viridisLite)
library(viridis)
library(ggplot2)
library(forcats)



#Master metadata file.
Master = read.csv("Dataframe_1.csv")


#Change the name of ecological disciplines

Master$Discipline = gsub("Other Drivers of Movement and Behavior", 
                         "Other Drivers",Master$Discipline)


#Get the dataframe which has total number of Manuscripts for
#each Discipline, and what method was used so it can be plotted
#in ggalluvia
Alluvial <- Master %>%
  dplyr::select(Discipline, Device_Pairing_Abbreviation, Manuscript) %>%
  distinct(Discipline, Device_Pairing_Abbreviation, Manuscript)

#Aggregate the data to count how many manuscripts report each combination of tools
Alluvial_count <- Alluvial %>%
  count(Device_Pairing_Abbreviation, Discipline)



#Prepare the data for an alluvial plot
df <- Alluvial_count %>%
  make_long(Device_Pairing_Abbreviation, Discipline)


sum = Alluvial_count %>% 
  group_by(Device_Pairing_Abbreviation) %>% 
  summarise(total = sum(n))


unique(df$node)
df <- df %>% filter(node!="")  

# df$node = gsub("AC", "Acoustic",df$node)
# df$node = gsub("R", "Radio",df$node)
# df$node = gsub("B", "Baited",df$node)
# df$node = gsub("SA", "Satellite",df$node)
# df$node = gsub("AB", "Animal-borne",df$node)
# df$node = gsub("DD", "Data Logger",df$node)

sum = df %>% 
  filter(x == "Device_Pairing_Abbreviation") %>%
  group_by(node) %>%
  summarise(total = n())


df$node <- factor(
  df$node,
  levels =
    c( "DD + AB + Mobile", "SA + Mobile", "PIT + Stationary",
       "PIT + R + Stationary", "DD + Mobile", "AC + Mobile + Stationary", 
       "AC + AB", "AC + DD + AB", "AC + PIT + R + Stationary", 
      "SA + Stationary", "DD + PIT + Mobile", "DD + PIT + AB",
      "DD + Stationary", "AC + DD + Stationary", "DD + SA + AB",
      "SA + AB", "R + Stationary", "AC + SA + B", "AC + PIT + Stationary",
      "DD + AB", "AC + Mobile", "AC + B", "AC + Stationary", 
      "Resource-Use Management", "Fisheries Management","Conservation Measures", 
      "Reproductive Ecology", "Other Drivers", "Methodological","Behavioural Ecology")
)

unique(sum)

#Format the alluvial plot

width <- .4

p <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 1, node.color = "black", show.legend = FALSE, width = width) +
  theme_void() +
  theme(
    plot.margin = unit(rep(5.5, 4), "pt")
  ) +
  #I have tried to play around with this code to change scale but to no avail
  scale_fill_viridis_d(begin = 1.0, end = 0)


# Get the data from the flows layer
dat <- layer_data(last_plot(), 1) |>
  filter(x == 2 - width / 2) |>
  distinct(fill, flow_end_ymax, .keep_all = TRUE)



p = p +
  geom_rect(data = dat, aes(
    xmin = x, xmax = x + width,
    ymin = flow_end_ymin, ymax = flow_end_ymax,
    fill = label
  ), inherit.aes = FALSE) +
  geom_sankey_label(size = 5, color = "black", fill = "white") +
  guides(fill = "none") 



p





























######################Sub_disciplines######################
#################NOT WORKIING##########################

Alluvial2 <- Master %>%
  dplyr::select(Discipline, Sub_discipline, Manuscript) %>%
  distinct(Discipline, Sub_discipline, Manuscript)

#Aggregate the data to count how many manuscripts report each combination
Alluvial_count2 <- Alluvial2 %>%
  count(Discipline, Sub_discipline)

#Reorder the factor levels for Device_Pairing_Abbreviation and Discipline by frequency
#So they show up in order on alluvial plot
Alluvial_count2 <- Alluvial_count2 %>%
  mutate(
    Sub_discipline = fct_infreq(Sub_discipline),
    Discipline = fct_infreq(Discipline)
  )



#Prepare the data for an alluvial plot
df2 <- Alluvial_count2 %>%
  make_long(Discipline, Sub_discipline)





unique(df2$node)
df2 <- df2 %>% filter(node!="")  

# df$node = gsub("Mobile", "MO",df$node)
# # df2$node = gsub("animal", "Animal",df2$node)
# df$node = gsub("Stationary", "ST",df$node)

sum = df2 %>% 
  filter(x == "Discipline") %>%
  group_by(node) %>%
  summarise(total = n())


df2$node <- factor(
  df2$node,
  levels =
    c( "DD + AB + Mobile", "SA + Mobile", "PIT + Stationary",
       "PIT + R + Stationary", "DD + Mobile", "AC + Mobile + Stationary", 
       "AC + AB", "AC + DD + AB", "AC + PIT + R + Stationary", 
       "SA + Stationary", "DD + PIT + Mobile", "DD + PIT + AB",
       "DD + Stationary", "AC + DD + Stationary", "DD + SA + AB",
       "SA + AB", "R + Stationary", "AC + SA + B", "AC + PIT + Stationary",
       "DD + AB", "AC + Mobile", "AC + B", "AC + Stationary", 
       "Resource-Use Management", "Fisheries Management","Conservation Measures", 
       "Reproductive Ecology", "Other Drivers", "Methodological","Behavioural Ecology")
)

unique(sum)

width <- .4

p <- ggplot(df2, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 1, node.color = "black", show.legend = FALSE, width = width) +
  theme_void() +
  theme(
    plot.margin = unit(rep(5.5, 4), "pt")
  ) +
  #I have tried to play around with this code to change scale but to no avail
  scale_fill_viridis_d()



ggplot(df2, aes(x=x, next_x=next_x, 
                node=node, 
                next_node=next_node, 
                fill=factor(node), 
                label=node, 
                color=factor(node)))+
  geom_sankey(show.legend = FALSE) +
  theme_void() +
  theme(
    plot.margin = unit(rep(5.5, 4), "pt")
  ) +
  #I have tried to play around with this code to change scale but to no avail
  scale_fill_viridis_d() 




# Get the data from the flows layer
dat <- layer_data(last_plot(), 1) |>
  filter(x == 2 - width / 2) |>
  distinct(fill, flow_end_ymax, .keep_all = TRUE)



p = p +
  geom_rect(data = dat, aes(
    xmin = x, xmax = x + width,
    ymin = flow_end_ymin, ymax = flow_end_ymax,
    fill = label
  ), inherit.aes = FALSE) +
  geom_sankey_label(size = 5, color = "black", fill = "white") +
  guides(fill = "none") 



p

path_out = "E:/Documents/Masters/Chapters/Chapter 1/Images"
ggsave("Fig2Extra.jpeg",
       plot = p, 
       path = path_out,
       device = "png",
       dpi=300, 
       width=400, 
       height=300, 
       units = "mm",
       bg="white")

