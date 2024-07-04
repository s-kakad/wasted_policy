#### SCRIPT 4B/4: DATA VISUALISATION: BY POLICY CATEGORY ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(ggh4x)
library(viridis)
library(ggrepel)
library(cowplot)

### PLOTTING THE CONTRIBUTION OF INDIVIDUAL POLICY CATEGORY TOWARDS FOOD WASTE DIVERSION ###
#define outlier function to selectively display state labels on the graph later on
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#plot graph
tiff('likely_fw_diversion_per_policy.tiff', units = "cm", width = 15, height = 15, res = 300)

boxplot_policy_groups <- likely_fw_kg_per_capita_df %>%
  filter(solution_level != "EPA-2016" & solution_level != "EPA-2021") %>%
  group_by(solution_level) %>%
  mutate(outlier = if_else(is_outlier((likely_fw_baseline_mean_kg_per_capita + likely_fw_alternative_mean_kg_per_capita)/2), state, NA_character_),
         solution_level = factor(solution_level, levels = c("Prevention", "Rescue", "Repurpose", "Recycling"))) %>%
  ggplot(aes(x = solution_level,
             y = (likely_fw_baseline_mean_kg_per_capita + likely_fw_alternative_mean_kg_per_capita)/2,
             fill = solution_level)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(color = "black", size = 0.5, alpha = 0.9, width = 0.09) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = F) +
  geom_hline(yintercept = fw_target,
             color = "aquamarine4",
             linewidth = 1) +
  scale_y_continuous(limits = c(0, 90),
                     breaks = c(0, 25, 50, 75)) +
#  theme_economist_white(gray_bg = FALSE) +
  theme_classic() +
  theme(axis.ticks.y = element_blank()) +
  theme(panel.grid.major.y = element_line(linewidth = 0.1, color = "lightgrey")) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Food waste diversion potential (kg/capita)")

print(boxplot_policy_groups)   

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

anova_fw <- likely_fw_kg_per_capita_df %>%
  filter(solution_level %in% solution_levels[1:4]) %>%
  group_by(solution_level) %>%
  mutate(likely_fw_diversion_overall_mean = (likely_fw_baseline_mean_kg_per_capita +
                                             likely_fw_alternative_mean_kg_per_capita)/2)

anova_test <- aov(likely_fw_diversion_overall_mean ~ solution_level, data = anova_fw)
anova(anova_test)
#significant one-way ANOVA: F(3, 196) = 85.71 (p < 0.001)
#note, can also run the ANOVA independently for each scenario (instead of averaging), with similar results:
#baseline: F(3, 196) = 93.709 (p < 0.001)
#alternative: F(3, 196) = 77.715 (p < 0.001)

cld_solution_level <- HSD.test(anova_test, trt = c("solution_level"), alpha = 0.05, console = TRUE)
#recycling significantly different from the remaining 3 policy categories

#### END OF SCRIPT 4/4 ####
#### END OF PROJECT ####