library(dplyr)
library(ggplot2)
library(stringr)
library("ggh4x")
library(forcats)
library(ggpubr)
library(grid)
library(tidyr)
library(viridis)
library(patchwork)

#Setting working directory for Masters Chapter folder
setwd("F:/Documents/Masters/Chapters/Chapter 1/Tables")
path_out = "F:/Documents/Masters/Chapters/Chapter 1/Images"

SDC = read.csv("Dataframe_1.csv")



#Calculate total number of unique publications per groups
SDC = SDC %>%
  group_by(Length_Groups, Depth_groups, Water_Type, 
           Device_Pairing_Abbreviation) %>%
  summarise(total = n_distinct(Manuscript))


SDC$Device_Pairing_Abbreviation = gsub("AC", "Acoustic",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("R", "Radio",SDC$Device_Pairing_Abbreviation)

SDC$Device_Pairing_Abbreviation = gsub("SA", "Satellite",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("AB", "Animal-borne",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("DD", "Data Logger",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("B", "Baited",SDC$Device_Pairing_Abbreviation)

depth = SDC %>% 
  group_by(Depth_groups,Device_Pairing_Abbreviation) %>% 
  summarise(total = sum(total))

length = SDC %>% 
  group_by(Length_Groups, Device_Pairing_Abbreviation) %>% 
  summarise(total = sum(total))

watertype <- SDC %>% 
  group_by(Water_Type) %>% 
  summarise(total = sum(total)) %>%
  mutate(proportion = total / sum(total))

ggplot(watertype, aes(y = "Water Types", fill = Water_Type, x = proportion)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Proportion of Different Water Types", x = "Proportion", y = NULL) +
  theme(axis.text.y = element_blank()) +  
  theme_classic()









device_totals <- length %>%
  group_by(Device_Pairing_Abbreviation) %>%
  summarise(total_articles = sum(total)) %>%
  ungroup()

# Merge total counts back into original dataframe
length <- length %>%
  left_join(device_totals, by = "Device_Pairing_Abbreviation")


device_totals <- depth %>%
  group_by(Device_Pairing_Abbreviation) %>%
  summarise(total_articles = sum(total)) %>%
  ungroup()

# Merge total counts back into original dataframe
depth <- depth %>%
  left_join(device_totals, by = "Device_Pairing_Abbreviation")




device_labels <- tibble(
  Device_Pairing_Abbreviation = unique(depth$Device_Pairing_Abbreviation)  # Get unique values
) %>%
  left_join(
    depth %>%
      group_by(Device_Pairing_Abbreviation) %>%
      summarise(total = sum(total)),  # Summarize total counts
    by = "Device_Pairing_Abbreviation"
  ) %>%
  arrange(total) %>%  # Order by total count (ascending)
  mutate(Device_Pairing_Abbreviation = fct_reorder(Device_Pairing_Abbreviation, total))

device_labels_plot <- 
  device_labels |>
  ggplot(
    aes(
      x = 1,
      y = Device_Pairing_Abbreviation,
      label = Device_Pairing_Abbreviation
    )
  ) +
  geom_text(size = 4.5) +
  theme_void()

device_labels_plot



ggsave("Figure5ALT_LABELS.jpeg",
       plot = device_labels_plot, 
       path = path_out,
       device = "png", 
       dpi=300,
       width = 4,  # Set width to 8 inches
       height = 10 / 1.5,  # Set height to maintain the aspect ratio (around 12 inches)
       units = "in",
       bg="lightblue")












str(device_label_plot)





p <- ggplot(data = depth,
            aes(x = total, 
                y = reorder(Device_Pairing_Abbreviation, total_articles),
                fill = factor(Depth_groups, levels = c("Surface to 3m", "3m to 40m", ">40m", "Not Reported")))) +
  geom_col(width = .5) +
  scale_fill_viridis(discrete = TRUE, 
                     breaks = c("Surface to 3m", "3m to 40m", ">40m", "Not Reported"),
                     direction = -1, end = .75) +   # Clip the data (without removing)
  scale_x_continuous(lim = c(22,0), expand = c(0, 0), trans = "reverse") +
  scale_y_discrete(c(levels(length$Device_Pairing_Abbreviation), "extra_space"),
                   expand = expansion(mult = c(0, 0.1)),
                   position = "right"
  ) + # Move y-axis to the right side of the plot
  theme_classic() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),  # Remove y-axis labels for this plot
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 20),
        plot.margin = margin(1,3,5,1, "cm"),
        plot.background = element_rect(fill = "lightblue", color = NA)) + 
  annotate(
    geom = "label",
    x = 19.60,
    y = 24.25,
    label = "Surface to 3m",
    fill = "#5ec962",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 16.20,
    y = 24.25,
    label = "3m to 40m",
    fill = "#21918c",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 13.85,
    y = 24.25,
    label = ">40m",
    fill = "#3b528b",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 11.15,
    y = 24.25,
    label = "Not Reported",
    fill = "#440154",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5
  ) 







# Right-side plot (Length) - Bars extending to the right
b <- ggplot(data = length,
            aes(x = total, 
                y = reorder(Device_Pairing_Abbreviation, total_articles),
                fill = factor(Length_Groups, levels = c("Small", "Medium", "Large", "Not Reported")))) +
  geom_col(width = .5) +
  scale_fill_viridis(discrete = TRUE, 
                     breaks = c("Small", "Medium", "Large", "Not Reported"),
                     labels = c("Small (<24cm)", "Medium (24-140cm)", "Large (>140cm)", "Not Reported"),
                     option = "plasma",
                     direction = -1, end = 0.75) +
  scale_x_continuous(lim = c(0,22), expand = c(0, 0)) +
  scale_y_discrete(c(levels(length$Device_Pairing_Abbreviation), "extra_space"),
                   expand = expansion(mult = c(0, 0.1))# Add more space at the top (10% more)
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 20),
        plot.margin = margin(1,1,5,3, "cm"),
        plot.background = element_rect(fill = "lightblue", color = NA)) +
  annotate(
    geom = "label",
    x = 19.60,
    y = 24.25,
    label = "Small (<24cm)",
    fill = "#f89540",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5) + 
  
  annotate(
    geom = "label",
    x = 14.85,
    y = 24.25,
    label = "Medium (24-146cm)",
    fill = "#cc4778",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 10,
    y = 24.25,
    label = "Large (>146cm)",
    fill = "#7e03a8",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 5.95,
    y = 24.25,
    label = "Not Reported",
    fill = "#440154",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines"),
    size = 5
  ) 


# Combine plots into one layout with equal width allocation
figure = ggpubr::ggarrange(p, b, ncol = 2, nrow = 1, widths = c(1, 1), common.legend = FALSE) 
figure




sum(depth$total)

path_out = "E:/Documents/Masters/Chapters/Chapter 1/Images"
ggsave("Figure5ALT.jpeg",
       plot = figure, 
       path = path_out,
       device = "png", 
       dpi=300, 
       width = 18.25,  # Set width to 8 inches
       height = 15 / 1.5,  # Set height to maintain the aspect ratio (around 12 inches)
       units = "in", 
       bg="lightblue")




f = ggplot(watertype, aes(y = "Water Types", fill = Water_Type, x = proportion)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Proportion of Different Water Types", x = "Proportion", y = NULL) +
  theme(axis.text.y = element_blank()) +  
  theme_void() +
  theme(
    title = element_blank(),
    legend.position = "none"
  )

ggsave("Proportion.jpeg",
       plot = f, 
       path = path_out,
       device = "png", 
       dpi=300, 
       width=650, 
       height=300, 
       units = "mm",
       bg="white")
