library(ggpubr)
library("rphylopic")
library("viridis")
library(ggplot2)
library(dplyr)
library("ggtext")
library(extrafont)
loadfonts(device = "win")
library("patchwork")
library("egg")
library(stringr)
library(tidyverse)

#Setting working directory for Masters Chapter folder
setwd("F:/Documents/Masters/Chapters/Chapter 1/Tables")

###Import master dataframe 
inter_perYear = read.csv("Dataframe_1.csv")
combo_diversity = read.csv("Dataframe_2.csv")









###Phylopic
##Make phylopics

Bivalve = get_uuid(name = "Bivalvia")
Bivalve_pic <- get_phylopic(Bivalve)

#Cephalopoda
Cephalopoda = get_uuid(name="Alloteuthis africana")
Cephalopoda_pic = get_phylopic(Cephalopoda)

#Cetacea
cetacea = get_uuid(name = "Kogia simus")
cetacea_pic = get_phylopic(cetacea)

#Chondrichthyes
##Make phylopics
Chondrichthyes = get_uuid(name = "Strophodus rebecae")
Chondrichthyes_pic <- get_phylopic(Chondrichthyes)


##Chondrostei
Chondrostei = get_uuid(name = "Acipenser")
Chondrostei_pic <- get_phylopic(Chondrostei)

#Decapoda
Decapoda = get_uuid((name = "Cancer magister"))
decapoda_pic = get_phylopic(Decapoda)

##Gastropoda
Gastropoda = get_uuid(name = "Gastropoda")
Gastropoda_pic <- get_phylopic(Gastropoda)


#Phocidae
Phocidae = get_uuid(name = "Phocidae")
Phocidae_pic = get_phylopic(Phocidae)


##Reptilia
Reptilia = get_uuid(name = "Eretmochelys imbricata")
Reptilia_pic <- get_phylopic(Reptilia)



##Teleostei
Teleostei = get_uuid(name = "Salmoninae")
Teleostei_pic <- get_phylopic(Teleostei)


































cols = c("#f89540","#b5de2b", "#26828e", "#440154FF", "#414487FF", "#b218d8", 
         "#22A884FF","#7AD151FF","#FDE725FF","#d51167")




###Extract publication year to look at temporal trends
inter_perYear$Year_Publication = 
  str_extract(inter_perYear$Manuscript, "_\\d{4}") |> str_remove("_")

#Create dataframe to graph trends in total devices used over time
combo_trends = inter_perYear %>% 
  dplyr::select(Intermediate_Device_Pairing, Manuscript, 
         Year_Publication, Total_Devices) %>% 
  unique()






###Bin the years into 3 year increments
combo_trends$Year_Publication = as.numeric(combo_trends$Year_Publication)

breaks <- seq(1986, 2023, by = 3)
labels <- paste(breaks[-length(breaks)], breaks[-1] - 1, sep = "-")


combo_trends$Year_Publication_Cut <- cut(
  combo_trends$Year_Publication, 
  breaks, labels, right = FALSE)

levels(combo_trends$Year_Publication_Cut)
#[1] "3"  "4"  "7"  "9"  "10"

#add new factor level. i.e 88 in our example
combo_trends$Year_Publication_Cut = 
  factor(combo_trends$Year_Publication_Cut, 
         levels=c(levels(combo_trends$Year_Publication_Cut), "2022"))


#convert all NA's to 2022
combo_trends$Year_Publication_Cut[is.na(combo_trends$Year_Publication_Cut)] = "2022"




###Count the total number of manuscripts over years and the total devices used
combo_trends_sum <- combo_trends %>% 
  group_by(Year_Publication_Cut, Total_Devices) %>%
  summarise(count = n(), .groups = "drop") # Count occurrences


##Make total devices as factor to be coloured as different categories
combo_trends_sum$Total_Devices = as.factor(combo_trends_sum$Total_Devices)







#####Graph the temporal trends in using paired devices
temporal_trends = ggplot(
  data = combo_trends_sum,
  aes(x = Year_Publication_Cut,
      y = count, 
      fill = Total_Devices)) +
  geom_col(width = .75) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 28),
    expand = expansion(mult = c(0, 0))
  ) +
  
  theme_bw() +
  theme(
    # get rid of grey grid marks in the back
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    # set horizontal grid marks to follow y-values across
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1,
                               size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    
    axis.title.x = element_text(
      size = 20, 
      margin = margin(30, 0, 0, 0)
    ),
    axis.title.y = element_text(
      size = 20, 
      margin = margin(0, 30, 0, 0)
    ),
    
    
    plot.margin = unit(c(1.5,0, .87, 0.1),
                       "inches")) +
  
  
  # change axis titles
  xlab("Publication Year") +
  ylab("Total Number of Studies") +
  labs(fill = "Total Devices \nPaired")

temporal_trends 



path_out = "E:/Documents/Masters/Chapters/Chapter 1/Images"
ggsave("Fig4Combined.jpeg",
       plot = temporal_trends , 
       path = path_out,
       device = "png", 
       dpi=300, 
       width=300, 
       height=230, 
       units = "mm")








TaxaCombo_Plot = ggplot(
  data = combo_diversity,
  aes(x = reorder(Specific_Device_Pairing, Number.of.Unique.Species, FUN = base::sum), 
      y = Number.of.Unique.Species, 
      fill = Taxa)) +
  scale_fill_manual(values = cols) +
  geom_col(width = .75) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0,50),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_bw() +
  theme(
    # get rid of grey grid marks in the back
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    # set horizontal grid marks to follow y-values across
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 19),
    axis.text.x = element_text(size = 19),
    
    axis.title.x = element_text(
      size = 20, 
      margin = margin(20, 0, 0, 0)
    ),
    axis.title.y = element_text(
      size = 20, 
      margin = margin(0, 20, 0, 0)
    ),
    
    
    plot.margin = unit(c(1, 0.5, .75, 1.25),
                       "inches"),
    
    legend.position = "none") +
  # change axis titles
  xlab("Device Pairings") +
  ylab("Total Number of Species")

TaxaCombo_Plot = TaxaCombo_Plot + coord_flip() 







TaxaCombo_Plot1 = TaxaCombo_Plot +
  #Bivalve Call
  add_phylopic(Bivalve_pic, alpha = 1, x = 28, y = 24, ysize = 1.25, fill =  "#f89540") +
  annotate("text", x = 28, y=26.5, label= "Bivalvia", size = 16/.pt, hjust = 0) +
  
  
  #Cephalopoda Call
  add_phylopic(Cephalopoda_pic, alpha = 1, x = 26.5, y = 23.75, ysize = 1, fill =  "#b5de2b") +
  annotate("text", x = 26.5, y=26.5, label= "Cephalopoda", size = 16/.pt, hjust = 0) +
  
  #Cetacea Call
  add_phylopic(cetacea_pic, alpha = 1, x = 25, y = 23.75, ysize = 1, fill =  "#26828e") +
  annotate("text", x = 25, y=26.5, label= "Cetacea", size = 16/.pt, hjust = 0) +
  
  #Chondrichthyan calling
  add_phylopic(Chondrichthyes_pic, alpha = 1, x = 23.5,y = 23.5, ysize = 1.5, fill =  "#440154FF") +
  annotate("text", x = 23.5, y=26.5, size = 16/.pt, label= "Chondricthyes", hjust = 0) +
  
  #Chondrostei calling
  add_phylopic(Chondrostei_pic, alpha = 1, x = 22,y = 23.5, ysize = .75, fill =  "#414487FF") +
  annotate("text", x = 22, y=26.5, label= "Chondrostei", size = 16/.pt, hjust = 0) +
  
  
  #Decapoda calling
  add_phylopic(decapoda_pic, alpha = 1, x = 20.5,y = 24, ysize = 1.5, fill =  "#b218d8") +
  annotate("text", x = 20.5, y=26.5, label= "Decapoda", size = 16/.pt, hjust = 0) +
  
  
  #Gastropoda calling
  add_phylopic(Gastropoda_pic, alpha = 1, x = 18.9,y = 24, ysize = 1.25, fill =  "#22A884FF") +
  annotate("text", x = 18.9, y=26.5, label= "Gastropoda", size = 16/.pt, hjust = 0) +
  
  #Phocidae calling
  add_phylopic(Phocidae_pic, alpha = 1, x = 17.5,y = 24, ysize = 1.25, fill =  "#7AD151FF") +
  annotate("text", x = 17.5, y=26.5, label= "Phocidae", size = 16/.pt, hjust = 0) +
  
  
  #Reptilia calling
  add_phylopic(Reptilia_pic, alpha = 1, x = 16,y = 24, ysize = 1.5, fill =  "#FDE725FF") +
  annotate("text", x = 16, y=26.5, label= "Reptilia", size = 16/.pt, hjust = 0) +
  
  #Teleostei calling
  add_phylopic(Teleostei_pic, alpha = 1, x = 14.5,y = 24, ysize = 1, fill =  "#d51167") +
  annotate("text", x = 14.5, y=26.5, label= "Teleostei", size = 16/.pt, hjust = 0) 

TaxaCombo_Plot1 = TaxaCombo_Plot1 + 
  scale_y_continuous(
  limits = c(0, 35),
  expand = expansion(mult = c(0, 0))  # No extra space added on the y-axis
)



path_out = "E:/Documents/Masters/Chapters/Chapter 1/Images"
ggsave("Figure4ALT.jpeg",
       plot = TaxaCombo_Plot1, 
       path = path_out,
       device = "png", 
       dpi=300, 
       width=600, 
       height=300, 
       units = "mm")


p = ggarrange(temporal_trends, TaxaCombo_Plot1,
              ncol = 2,
              nrow = 1) 


p

path_out = "E:/Documents/Masters/Chapters/Chapter 1/Images"
ggsave("Figure4.jpeg",
       plot = p, 
       path = path_out,
       device = "png", 
       dpi=300, 
       width=600, 
       height=300, 
       units = "mm")






























####Offloading on its own




TaxaCombo_Plot1 = TaxaCombo_Plot +
  #Bivalve Call
  add_phylopic(Bivalve_pic, alpha = 1, x = 28, y = 25, ysize = 1.25, fill =  "#f89540") +
  annotate("text", x = 28, y=26.5, label= "Bivalvia", size = 16/.pt, hjust = 0) +
  
  
  #Cephalopoda Call
  add_phylopic(Cephalopoda_pic, alpha = 1, x = 26.5, y = 25, ysize = 1, fill =  "#b5de2b") +
  annotate("text", x = 26.5, y=26.5, label= "Cephalopoda", size = 16/.pt, hjust = 0) +
  
  #Cetacea Call
  add_phylopic(cetacea_pic, alpha = 1, x = 25, y = 24.5, ysize = 1, fill =  "#26828e") +
  annotate("text", x = 25, y=26.5, label= "Cetacea", size = 16/.pt, hjust = 0) +
  
  #Chondrichthyan calling
  add_phylopic(Chondrichthyes_pic, alpha = 1, x = 23.5,y = 24.5, ysize = 1.5, fill =  "#440154FF") +
  annotate("text", x = 23.5, y=26.5, size = 16/.pt, label= "Chondricthyes", hjust = 0) +
  
  #Chondrostei calling
  add_phylopic(Chondrostei_pic, alpha = 1, x = 22,y = 24.5, ysize = .75, fill =  "#414487FF") +
  annotate("text", x = 22, y=26.5, label= "Chondrostei", size = 16/.pt, hjust = 0) +
  
  
  #Decapoda calling
  add_phylopic(decapoda_pic, alpha = 1, x = 20.5,y = 25, ysize = 1.5, fill =  "#b218d8") +
  annotate("text", x = 20.5, y=26.5, label= "Decapoda", size = 16/.pt, hjust = 0) +
  
  
  #Gastropoda calling
  add_phylopic(Gastropoda_pic, alpha = 1, x = 18.9,y = 25, ysize = 1.25, fill =  "#22A884FF") +
  annotate("text", x = 18.9, y=26.5, label= "Gastropoda", size = 16/.pt, hjust = 0) +
  
  #Phocidae calling
  add_phylopic(Phocidae_pic, alpha = 1, x = 17.5,y = 25, ysize = 1.25, fill =  "#7AD151FF") +
  annotate("text", x = 17.5, y=26.5, label= "Phocidae", size = 16/.pt, hjust = 0) +
  
  
  #Reptilia calling
  add_phylopic(Reptilia_pic, alpha = 1, x = 16,y = 25, ysize = 1.5, fill =  "#FDE725FF") +
  annotate("text", x = 16, y=26.5, label= "Reptilia", size = 16/.pt, hjust = 0) +
  
  #Teleostei calling
  add_phylopic(Teleostei_pic, alpha = 1, x = 14.5,y = 24.5, ysize = 1, fill =  "#d51167") +
  annotate("text", x = 14.5, y=26.5, label= "Teleostei", size = 16/.pt, hjust = 0) 

TaxaCombo_Plot1 = TaxaCombo_Plot1 + 
  scale_y_continuous(
    limits = c(0, 32),
    expand = expansion(mult = c(0, 0))  # No extra space added on the y-axis
  )



path_out = "E:/Documents/Masters/Chapters/Chapter 1/Images"
ggsave("Figure4ALT.jpeg",
       plot = TaxaCombo_Plot1, 
       path = path_out,
       device = "png", 
       dpi=300, 
       width=400, 
       height=275, 
       units = "mm")









#Total articles per year
total_articles = combo_trends_sum %>% 
  group_by(Year_Publication_Cut) %>% 
  summarise(total = sum(count),
            prop = (total/81) * 100)

#Total times devices used diffrent numbers and proportions
total_devices_overYears = combo_trends_sum %>% 
  group_by(Total_Devices, Year_Publication_Cut) %>% 
  summarise(
    total_dev = sum(count),  # Sum counts for each group of devices and year
    .groups = "drop"  # Ungroup after summarising
  ) %>%
  group_by(Year_Publication_Cut) %>%  # Group by year to get the total for each year
  mutate(
    prop_dev = total_dev / sum(total_dev)  # Calculate proportion for each device group
  ) %>%
  ungroup()

#Get proportion overall of total devices
Total_Devices = combo_trends_sum %>% 
  group_by(Total_Devices) %>% 
  summarise(total = sum(count),
            prop = total/81)



#Get total number of species
count(combo_diversity$Number.of.Unique.Species)

