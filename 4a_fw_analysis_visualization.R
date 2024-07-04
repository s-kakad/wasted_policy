#### SCRIPT 4A/4: DATA VISUALISATION: DATA IMPORT AND FEDERAL LEVEL ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(ggh4x)
library(viridis)
library(ggrepel)
library(cowplot)

### IMPORT DATA FOR VISUALISATION ###
#import datasets to avoid running previous scripts (if needed)
likely_fw_kg_per_capita_df <- read.csv("3_likely_diversion_kg_per_capita.csv")
likely_federal_fw_kg_per_capita_df <- read.csv("3_likely_federal_diversion_kg_per_capita.csv")

#assign appropriate class and change order of levels
likely_federal_fw_kg_per_capita_df$solution_level <- as.factor(likely_federal_fw_kg_per_capita_df$solution_level)
likely_federal_fw_kg_per_capita_df$solution_level <- factor(likely_federal_fw_kg_per_capita_df$solution_level,
                                                            levels = c("Updated", "All", "Recycling", "Repurpose",
                                                                       "Rescue", "Prevention"),
                                                            labels = c("EPA-2021", "EPA-2016", "Recycling", "Repurpose",
                                                                       "Rescue", "Prevention"))

likely_fw_kg_per_capita_df$state <- as.factor(likely_fw_kg_per_capita_df$state)
likely_fw_kg_per_capita_df$state <- factor(likely_fw_kg_per_capita_df$state, levels = state.abb)
likely_fw_kg_per_capita_df$solution_level <- as.factor(likely_fw_kg_per_capita_df$solution_level)
likely_fw_kg_per_capita_df$solution_level <- factor(likely_fw_kg_per_capita_df$solution_level,
                                                            levels = c("Updated", "All", "Recycling", "Repurpose",
                                                                       "Rescue", "Prevention"),
                                                            labels = c("EPA-2021", "EPA-2016", "Recycling", "Repurpose",
                                                                       "Rescue", "Prevention"))

state_order <- factor(likely_fw_kg_per_capita_df$state[1:50], levels = state.abb, ordered = TRUE)


#calculate target and current food waste at the federal level (in kg/capita)
fw_target <- 74.389 #US 2030 target (based on 2016 baseline)
fw_current_weighted <- mean(likely_federal_fw_kg_per_capita_df$total_fw_kg_per_capita) #weighted by population

#the EPA uses a scaling-up approach to estimate national food waste generation
#thus we also calculate the non-weighted food waste generation average across the 50 states
fw_current_average <- mean(likely_fw_kg_per_capita_df$total_fw_kg_per_capita) #note 167.6141 > 158.2879

### PLOT LIKELY FOOD WASTE DIVERSION POTENTIAL AT THE FEDERAL LEVEL ###
#plot graph
#for food waste diversion potential, simply display the estimated diversion potential
tiff('likely_federal_diversion.tiff', units = "cm", width = 15, height = 7, res = 300)

plot_federal_diversion <- likely_federal_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = c("ivory4", "ivory4", "ivory4", "ivory4", "blue", "purple"),
               alpha = 0.9,
               linewidth = 4) +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.7,
               linewidth = 1.5) +
#  geom_point(aes(x = solution_level,
#                 y = likely_fw_baseline_mean_kg_per_capita),
#             color = "white",
#             size = 1,
#             shape = 18) +
#  geom_point(aes(x = solution_level,
#                 y = likely_fw_alternative_mean_kg_per_capita),
#             color = "white",
#             size = 1,
#             shape = 18) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  geom_hline(yintercept = fw_current_weighted,
             color = "cornflowerblue",
             linewidth = 0.85) +
  geom_hline(yintercept = fw_current_average,
             color = "cornflowerblue",
             linewidth = 0.5,
             linetype = "dotted") +
  scale_y_continuous(trans = "log2",
                     limits = c(0.5, 220),
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 200),
                     labels = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  coord_flip() +
  theme_economist_white(gray_bg = FALSE) +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  xlab("") +
  ylab("Food waste diversion potential (kg/capita)")
  #theme(axis.title = element_text (size = 9))

print(plot_federal_diversion)
dev.off()

### PLOT PROJECTED FOOD WASTE GENERATION AT THE FEDERAL LEVEL ###
#plot graph
#for project food waste generation, displayed data corresponds to current food waste generation - diversion potential
tiff('likely_federal_generation.tiff', units = "cm", width = 15, height = 7, res = 300)

plot_federal_generation <- likely_federal_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level, 
                   y = fw_current_weighted - likely_fw_baseline_low_kg_per_capita, 
                   yend = fw_current_weighted - likely_fw_baseline_high_kg_per_capita),
               colour = "darkslateblue",
               alpha = 0.85,
               linewidth = 4) +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level,
                   y = fw_current_weighted - likely_fw_alternative_low_kg_per_capita, 
                   yend = fw_current_weighted - likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 1.5) +
#               position = position_jitter(height = 0, width = 0.25)) +
#  geom_point(aes(x = solution_level,
#                 y = fw_current_weighted - likely_fw_baseline_mean_kg_per_capita),
#             color = "white",
#             size = 1,
#             shape = 18) +
#  geom_point(aes(x = solution_level,
#                 y = fw_current_weighted - likely_fw_alternative_mean_kg_per_capita),
#             color = "white",
#             size = 1,
#             shape = 18) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  geom_hline(yintercept = fw_current_weighted,
             color = "cornflowerblue",
             linewidth = 0.85) +
  geom_hline(yintercept = fw_current_average,
             color = "cornflowerblue",
             linewidth = 0.5,
             linetype = "dotted") +
  coord_flip() +
  theme_economist_white(gray_bg = FALSE) +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  xlab("") +
  ylab("Projected food waste generation (kg/capita)")
  #theme(axis.title = element_text (size = 9))

print(plot_federal_generation)
dev.off()

### COMBINED GRAPHS ###
#plot both graphs side-by-side
federal_combined_plot <- plot_grid(plot_federal_diversion, plot_federal_generation)
save_plot("federal_combined_plot.tiff", federal_combined_plot,
          ncol = 2,
          nrow = 1,
          base_asp = 1.1)

#### DATA VISUALISATION CONTINUES IN SCRIPT 4B/4 ####