#### SCRIPT 4/4: DATA VISUALISATION ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(ggh4x)
library(viridis)
library(ggrepel)
library(cowplot)

### IMPORT DATA FOR VISUALISATION
likely_fw_kg_per_capita_df <- read.csv("likely_diversion_kg_per_capita.csv") # to avoid running code in R script 3
likely_federal_fw_kg_per_capita_df <- read.csv("likely_federal_diversion_kg_per_capita.csv") # to avoid running code in R script 3

fw_target <- 74.389 # US 2030 target for food waste in kg per capita (based on 2016 baseline)
fw_current <- mean(likely_federal_fw_kg_per_capita_df$total_fw_kg_per_capita) # current US food waste generation in kg/capita (based on 2022 data)

### PLOT LIKELY WASTED FOOD DIVERSION POTENTIAL & PROJECTED FOOD WASTE GENERATION AT THE FEDERAL LEVEL ###
#change order of levels for policy categories
likely_federal_fw_kg_per_capita_df$solution_level <- factor(likely_federal_fw_kg_per_capita_df$solution_level,
                                                        levels = c("Updated", "All", "Recycling", "Animal Feed",
                                                                   "Rescue", "Prevention"))

#plot graph
#for food waste diversion potential, simply display the estimated diversion potential
tiff('likely_federal_diversion.tiff', units = "cm", width = 15, height = 7, res = 300)

plot_federal_diversion <- likely_federal_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 1.5) +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "darkslateblue",
               alpha = 0.75,
               linewidth = 4) +
  geom_hline(yintercept = fw_current,
             color = "cornflowerblue",
             linewidth = 0.85) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  scale_y_continuous(trans = "log2",
                     limits = c(0.5, 220),
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 200),
                     labels = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ylab("Federal wasted diversion potential (kg/capita)")

print(plot_federal_diversion)
dev.off()

#plot graph
#for project food waste generation, displayed data corresponds to current food waste generation - diversion potential
tiff('likely_federal_generation.tiff', units = "cm", width = 15, height = 7, res = 300)

plot_federal_generation <- likely_federal_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level,
                   y = fw_current - likely_fw_alternative_low_kg_per_capita, 
                   yend = fw_current - likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 1.5) +
  geom_segment(data = likely_federal_fw_kg_per_capita_df,
               aes(x = solution_level,
                   xend = solution_level, 
                   y = fw_current - likely_fw_baseline_low_kg_per_capita, 
                   yend = fw_current - likely_fw_baseline_high_kg_per_capita),
               colour = "darkslateblue",
               alpha = 0.75,
               linewidth = 4) +
  geom_hline(yintercept = fw_current,
             color = "cornflowerblue",
             linewidth = 0.85) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ylab("Projected federal food waste generation (kg/capita)")

print(plot_federal_generation)
dev.off()

### PLOT CURRENT FOOD WASTE GENERATION LEVELS AT THE STATE LEVEL ###
#plot graph
plot_state_generation <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "All") %>% # does not matter, just need one solution only to rearrange states by food waste generation
  arrange(state) %>%
  mutate(state = factor(state, levels = state[order(desc(total_fw_kg_per_capita))])) %>% # or desc(state.abb) for OG plot
  ggplot() +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = "orange3",
             size = 2) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  geom_hline(yintercept = fw_current,
             color = "cornflowerblue",
             linewidth = 0.85) +
  scale_y_continuous(limits = c(70, 500),
                     breaks = c(100, 200, 300, 400),
                     labels = c(100, 200, 300, 400)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Current food waste generation (kg/capita)")
  
pdf("fw_generation_levels_reordered.pdf") # fw_generation_levels.pdf for OG plot
print(plot_state_generation)
dev.off()

### PLOT LIKELY WASTED FOOD DIVERSION POTENTIAL & PROJECTED FOOD WASTE GENERATION AT THE STATE LEVEL ('ALL' VS 'UPDATED' SCENARIOS) ###
#determine the mean wasted food diversion potential based on the "All" scenario
likely_fw_kg_per_capita_df_reordered_all <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "All") %>%
  rowwise() %>% 
  mutate(mymean_all = mean(c(likely_fw_baseline_low_kg_per_capita, 
                             likely_fw_baseline_high_kg_per_capita,
                             likely_fw_alternative_low_kg_per_capita,
                             likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean_all) %>%
  mutate(state = factor(state, levels = state[order(mymean_all)]))

#arrange by order of mean based on the "All" scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_all$state)))

#plot graph
#for food waste diversion potential, simply display the estimated diversion potential
plot_state_diversion <- likely_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "All", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "All", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.35,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "purple",
               alpha = 0.75,
               linewidth = 1.5) +
  #geom_point(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
  #           aes(x = state, y = total_fw_kg_per_capita),
  #           color = "orange3",
  #           size = 1.75) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  #scale_y_continuous(trans = "log2",
  #                   limits = c(0.85, 200),
  #                   breaks = c(1, 2, 5, 10, 20, 50, 100, 200),
  #                   labels = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ylab("Wasted food diversion potential (kg/capita)")

pdf("likely_fw_diversion.pdf")
print(plot_state_diversion)
dev.off()

#for project food waste generation, displayed data corresponds to current food waste generation - diversion potential
#determine the mean projected food wasted generation based on the "All" scenario
likely_fw_kg_per_capita_df_reordered_updated <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "Updated") %>%
  rowwise() %>% 
  mutate(mymean_updated = mean(c(total_fw_kg_per_capita - likely_fw_baseline_low_kg_per_capita, 
                             total_fw_kg_per_capita - likely_fw_baseline_high_kg_per_capita,
                             total_fw_kg_per_capita - likely_fw_alternative_low_kg_per_capita,
                             total_fw_kg_per_capita - likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(desc(mymean_updated)) %>%
  mutate(state = factor(state, levels = state[order(mymean_updated)]))

#arrange by order of mean based on the "Updated" scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_updated$state)))

#plot graph
plot_state_projected_generation <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "Updated") %>%
  ggplot() +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = total_fw_kg_per_capita - likely_fw_alternative_low_kg_per_capita, 
                   yend = total_fw_kg_per_capita - likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.85,
               linewidth = 0.75) +
  geom_segment(aes(x = state,
                   xend = state, 
                   y = total_fw_kg_per_capita - likely_fw_baseline_low_kg_per_capita, 
                   yend = total_fw_kg_per_capita - likely_fw_baseline_high_kg_per_capita),
               colour = "darkslateblue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = "orange3",
             size = 2) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  scale_y_continuous(trans = "log10",
                     limits = c(70, 465),
                     breaks = c(100, 200, 300, 400),
                     labels = c(100, 200, 300, 400)) +
  #scale_y_break(c(300, 400)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste generation (kg/capita)")

pdf("likely_fw_generation.pdf")
print(plot_state_projected_generation)   
dev.off()

### PLOT SIDE-BY-SIDE DIVERSION POTENTIAL FOR 'ALL' & UDPATED' SCENARIOS ###
#plot 1 by order of "All": same as likely_fw_diversion.pdf but with additional shading
#arrange by order of mean based on the "All" scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_all$state)))

#plot graph
plot_combined_left <- likely_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "All", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "All", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.35,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "purple",
               alpha = 0.45,
               linewidth = 1.5) +
  #geom_point(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
  #           aes(x = state, y = total_fw_kg_per_capita),
  #           color = "orange3",
  #           size = 1.75) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  #scale_y_continuous(trans = "log2",
  #                   limits = c(0.85, 200),
  #                   breaks = c(1, 2, 5, 10, 20, 50, 100, 200),
  #                   labels = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ylab("Wasted food diversion potential (kg/capita)")

#plot 2 by order of "Updated": shading now applied to the other scenario
#determine the mean wasted food diversion potential based on the "Updated" scenario
likely_fw_kg_per_capita_df_reordered_updated <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "Updated") %>%
  rowwise() %>% 
  mutate(mymean_updated = mean(c(likely_fw_baseline_low_kg_per_capita, 
                                 likely_fw_baseline_high_kg_per_capita,
                                 likely_fw_alternative_low_kg_per_capita,
                                 likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean_updated) %>%
  mutate(state = factor(state, levels = state[order(mymean_updated)]))

#arrange by order of mean based on the "Updated" scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_updated$state)))

#plot graph
plot_combined_right <- likely_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "All", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.35,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "All", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.45,
               linewidth = 1.5) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "purple",
               alpha = 0.85,
               linewidth = 1.5) +
  #geom_point(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "Updated", ],
  #           aes(x = state, y = total_fw_kg_per_capita),
  #           color = "orange3",
  #           size = 1.75) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  #scale_y_continuous(trans = "log2",
  #                   limits = c(0.85, 200),
  #                   breaks = c(1, 2, 5, 10, 20, 50, 100, 200),
  #                   labels = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ylab("Wasted food diversion potential (kg/capita)")

#plot both graphs side-by-side
combined_plot <- plot_grid(plot_combined_left, plot_combined_right)
save_plot("combined_plot.pdf", combined_plot,
          ncol = 1,
          nrow = 2,
          base_asp = 2.1)

### PLOTTING THE CONTRIBUTION OF INDIVIDUAL POLICY TYPES TOWARDS FOOD WASTE DIVERSION ###
#for this, box plot makes most sense, otherwise too many variables are displayed on the lollipop graph
likely_fw_kg_per_capita_df_alt <- read.csv(file = "likely_fw_kg_per_capita_alt.csv", header = TRUE)
likely_fw_kg_per_capita_df_alt <- likely_fw_kg_per_capita_df_alt[, 2:7]
likely_fw_kg_per_capita_df_alt$solution_level <- factor(likely_fw_kg_per_capita_df_alt$solution_level,
                                                        levels = c("Prevention", "Rescue", "Animal Feed", "Recycling",
                                                                   "All", "Updated"))

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.6 * IQR(x) | x > quantile(x, 0.75) + 1.6 * IQR(x))
}

boxplot <- likely_fw_kg_per_capita_df_alt %>%
  filter(solution_level != "All" & solution_level != "Updated") %>%
  group_by(solution_level) %>%
  mutate(outlier = if_else(is_outlier(diversion_potential_kg_per_capita), state, NA_character_)) %>%
  ggplot(aes(x = solution_level,
             y = diversion_potential_kg_per_capita,
             fill = solution_level)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = F) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 1) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = c(0, 25, 50, 75, 100)) +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Wasted food diversion potential (kg/capita)")

pdf("likely_fw_diversion_per_policy_boxplot.pdf")
print(boxplot)   
dev.off()

### STATISTICAL ANALYSIS FOR DIFFERENCE ACROSS POLICY CATEGORIES ###
library("car")
library("magrittr")
library("dplyr")
library("Rmisc")
library("ggpubr")
library("multcomp")
library("agricolae")

detach(package:Rmisc)
detach(package:plyr)

solution_levels <- c("Prevention", "Rescue", "Animal Feed", "Recycling", "All", "Updated")

likely_fw_kg_per_capita_df_alt$solution_level <- as.factor(likely_fw_kg_per_capita_df_alt$solution_level)

anova_fw <- likely_fw_kg_per_capita_df_alt %>%
  filter(solution_level %in% solution_levels[1:4]) %>%
  group_by(solution_level)

anova_test <- aov(diversion_potential_kg_per_capita ~ solution_level, data = anova_fw)
anova(anova_test)
cld_solution_level <- HSD.test(anova_test, trt = c("solution_level"), alpha = 0.05, console = TRUE)

