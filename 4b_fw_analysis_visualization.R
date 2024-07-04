#### SCRIPT 4B/4: DATA VISUALISATION: STATE LEVEL ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(ggh4x)
library(viridis)
library(ggrepel)
library(cowplot)

#for data import, see script 3A/4

### PLOT CURRENT FOOD WASTE GENERATION LEVELS AT THE STATE LEVEL ###
#plot graph
tiff('fw_generation_levels.tiff', units = "cm", width = 15, height = 15, res = 300)

plot_state_generation <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "EPA-2016") %>% #does not matter, just need one solution only to rearrange states by food waste generation
  arrange(desc(state)) %>%
  mutate(state = factor(state, levels = state[state_order])) %>% # or desc(total_fw_kg_per_capita) for ordered plot
  ggplot() +
  geom_point(aes(x = state, y = total_fw_kg_per_capita),
             color = "orange3",
             size = 2) +
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
  scale_y_continuous(limits = c(70, 500),
                     breaks = c(100, 200, 300, 400),
                     labels = c(100, 200, 300, 400)) +
  coord_flip() +
  theme_classic() +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  xlab("") +
  ylab("Current food waste generation (kg/capita)")

print(plot_state_generation)
dev.off()

### PLOT LIKELY FOOD WASTE DIVERSION POTENTIAL AT THE STATE LEVEL ('EPA-2016' VS 'EPA-2021' SCENARIOS) ###
#determine the mean food waste diversion potential by state based on the EPA-2016 scenario
likely_fw_kg_per_capita_df_reordered_all <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "EPA-2016") %>%
  rowwise() %>% 
  mutate(mymean_all = mean(c(likely_fw_baseline_low_kg_per_capita, 
                             likely_fw_baseline_high_kg_per_capita,
                             likely_fw_alternative_low_kg_per_capita,
                             likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean_all) %>%
  mutate(state = factor(state, levels = state[order(mymean_all)]))

#arrange by order of mean based on the EPA-2016 scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_all$state)))

#plot graph
#for food waste diversion potential, simply display the estimated diversion potential
tiff('likely_fw_diversion.tiff', units = "cm", width = 15, height = 15, res = 300)

plot_state_diversion <- likely_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2016", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2016", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2021", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.35,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2021", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "purple",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  coord_flip() +
  theme_classic() +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  xlab("") +
  ylab("Food waste diversion potential (kg/capita)")

print(plot_state_diversion)
dev.off()

### PLOT PROJECTED FOOD WASTE GENERATION AT THE STATE LEVEL ('EPA-2016' VS 'EPA-2021' SCENARIOS) ###
#projected food waste generation = current food waste generation - diversion potential
#determine the mean projected food wasted generation based on the EPA-2021 scenario
likely_fw_kg_per_capita_df_reordered_updated <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "EPA-2021") %>%
  rowwise() %>% 
  mutate(mymean_updated = mean(c(total_fw_kg_per_capita - likely_fw_baseline_low_kg_per_capita, 
                                 total_fw_kg_per_capita - likely_fw_baseline_high_kg_per_capita,
                                 total_fw_kg_per_capita - likely_fw_alternative_low_kg_per_capita,
                                 total_fw_kg_per_capita - likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(desc(mymean_updated)) %>%
  mutate(state = factor(state, levels = state[order(mymean_updated)]))

#arrange by order of mean based on the EPA-2021 scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_updated$state)))

#plot graph
tiff('likely_fw_generation.tiff', units = "cm", width = 15, height = 15, res = 300)
plot_state_projected_generation <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "EPA-2021") %>%
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
  coord_flip() +
  theme_classic() +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  xlab("") +
  ylab("Likely food waste generation (kg/capita)")

print(plot_state_projected_generation)   
dev.off()

### PLOT SIDE-BY-SIDE DIVERSION POTENTIAL FOR EPA-2016 & EPA-2021 SCENARIOS ###
#plot 1 by order of "EPA-2016": same as likely_fw_diversion.tiff but with additional shading
#arrange by order of mean based on the EPA-2016 scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_all$state)))

#plot graph
plot_state_diversion_shaded <- likely_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2016", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2016", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.75,
               linewidth = 1.5) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2021", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.35,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2021", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "purple",
               alpha = 0.45,
               linewidth = 1.5) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  coord_flip() +
  theme_classic() +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  xlab("") +
  ylab("Food waste diversion potential (kg/capita)")

#plot 2 by order of "EPA-2021": shading now applied to the other scenario
#determine the mean food waste diversion potential based on the EPA-2021 scenario
likely_fw_kg_per_capita_df_reordered_updated <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "EPA-2021") %>%
  rowwise() %>% 
  mutate(mymean_updated = mean(c(likely_fw_baseline_low_kg_per_capita, 
                                 likely_fw_baseline_high_kg_per_capita,
                                 likely_fw_alternative_low_kg_per_capita,
                                 likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(mymean_updated) %>%
  mutate(state = factor(state, levels = state[order(mymean_updated)]))

#arrange by order of mean based on the EPA-2021 scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_updated$state)))

#plot graph
plot_state_diversion_reordered <- likely_fw_kg_per_capita_df %>%
  ggplot() +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2016", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.35,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2016", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "blue",
               alpha = 0.35,
               linewidth = 1.5) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2021", ],
               aes(x = state,
                   xend = state,
                   y = likely_fw_alternative_low_kg_per_capita, 
                   yend = likely_fw_alternative_high_kg_per_capita),
               colour = "black",
               alpha = 0.65,
               linewidth = 0.75) +
  geom_segment(data = likely_fw_kg_per_capita_df[likely_fw_kg_per_capita_df$solution_level == "EPA-2021", ],
               aes(x = state,
                   xend = state, 
                   y = likely_fw_baseline_low_kg_per_capita, 
                   yend = likely_fw_baseline_high_kg_per_capita),
               colour = "purple",
               alpha = 0.95,
               linewidth = 1.5) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 0.85) +
  coord_flip() +
  theme_classic() +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  xlab("") +
  ylab("Food waste diversion potential (kg/capita)")

#plot both graphs side-by-side
plot_combined_state_diversion <- plot_grid(plot_state_diversion_shaded, plot_state_diversion_reordered)
save_plot("combined_state_diversion.tiff", plot_combined_state_diversion,
          ncol = 1,
          nrow = 2,
          base_asp = 2.1)

#### DATA VISUALISATION CONTINUES IN SCRIPT 4C/4 ####