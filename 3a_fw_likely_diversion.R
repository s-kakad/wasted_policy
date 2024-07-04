#### SCRIPT 3A/4:LIKELY FOOD WASTE DIVERSION QUANTIFICATION IN TONS ###
library(tidyverse)
library(dplyr)
library(ggplot2)

### MERGING APPLICABLE FOOD WASTE WITH POLICY SCORING AND POPULATION DATA ###
#import total food waste generation file from script 1 (if needed)
total_fw_tons_df <- read.csv("1_total_fw_tons.csv")

#import applicable food waste file from script 2 (if needed)
applicable_fw_tons_df <- read.csv("2_applicable_fw_tons.csv")

#prepare dataframes for merging
total_fw_tons_df$state <- as.factor(total_fw_tons_df$state)
total_fw_tons_df$state <- factor(total_fw_tons_df$state, levels = state.abb)
total_fw_tons_df$population <- as.numeric(total_fw_tons_df$population)
total_fw_tons_df$solution_level <- as.factor(total_fw_tons_df$solution_level)
total_fw_tons_df$solution_level <- factor(total_fw_tons_df$solution_level, levels = solution_levels)
total_fw_tons_df$policy_qual_score <- as.factor(total_fw_tons_df$policy_qual_score)
total_fw_tons_df$factor_low <- as.numeric(total_fw_tons_df$factor_low)
total_fw_tons_df$factor_high <- as.numeric(total_fw_tons_df$factor_high)
total_fw_tons_df$total_fw <- as.numeric(total_fw_tons_df$total_fw)

applicable_fw_tons_df$state <- as.factor(applicable_fw_tons_df$state)
applicable_fw_tons_df$state <- factor(applicable_fw_tons_df$state, levels = state.abb)
applicable_fw_tons_df$solution_level <- as.factor(applicable_fw_tons_df$solution_level)
applicable_fw_tons_df$solution_level <- factor(applicable_fw_tons_df$solution_level, levels = solution_levels)
applicable_fw_tons_df$applicable_baseline <- as.numeric(applicable_fw_tons_df$applicable_baseline)
applicable_fw_tons_df$applicable_alternative <- as.numeric(applicable_fw_tons_df$applicable_alternative)

#merge dataframes
merged_fw_tons_df <- merge(applicable_fw_tons_df, total_fw_tons_df)
merged_fw_tons_df <- merged_fw_tons_df %>% arrange(state)
rm(applicable_fw_tons_df, total_fw_tons_df)

### LIKELY FOOD WASTE DIVERSION QUANTIFICATION ###
names(merged_fw_tons_df)

#select and reorder columns and save as new dataframe for subsequent data transformation
likely_fw_tons_df <- merged_fw_tons_df %>% select(state,
                                        solution_level,
                                        total_fw,
                                        population,
                                        applicable_baseline,
                                        applicable_alternative,
                                        factor_low,
                                        factor_high)


#calculate likely diversion ranges by multiplying scenarios by factors (data still in total US short tons)
likely_fw_tons_df <- likely_fw_tons_df %>%
  mutate(likely_fw_baseline_low = applicable_baseline*factor_low,
         likely_fw_baseline_high = applicable_baseline*factor_high,
         likely_fw_alternative_low = applicable_alternative*factor_low,
         likely_fw_alternative_high = applicable_alternative*factor_high)

#keep only columns of interest for subsequent data visualisation
likely_fw_tons_df <- likely_fw_tons_df[, c(1:4, 9:12)]
likely_fw_tons_df <- likely_fw_tons_df %>%
  arrange(state, solution_level)

### DATAFRAME TRANSFORMATION ###
#run for loop to add a new row for solution_levels "All" and "Updated" to find
#total diversion per state for EPA-2016 ("All") and EPA-2021 ("Updated") scenarios
#since they are a combination of different solutions, we need to run a for loop for each scenario separately
#first for the "All" scenario (original EPA-2016 methodology)
for (s in state.abb) {
  scenario <- solution_levels[5]
  baseline_low <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$likely_fw_baseline_low %>%
    sum()
  baseline_high <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$likely_fw_baseline_high %>%
    sum()
  alternative_low <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$likely_fw_alternative_low %>%
    sum()
  alternative_high <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$likely_fw_alternative_high %>%
    sum()
  fw_tot <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$total_fw %>%
    mean()
  pop <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$population %>%
    mean()
  likely_fw_tons_df[nrow(likely_fw_tons_df) + 1, 1] <- s
  likely_fw_tons_df[nrow(likely_fw_tons_df), 2] <- scenario
  likely_fw_tons_df[nrow(likely_fw_tons_df), 3] <- fw_tot
  likely_fw_tons_df[nrow(likely_fw_tons_df), 4] <- pop
  likely_fw_tons_df[nrow(likely_fw_tons_df), 5] <- baseline_low
  likely_fw_tons_df[nrow(likely_fw_tons_df), 6] <- baseline_high
  likely_fw_tons_df[nrow(likely_fw_tons_df), 7] <- alternative_low
  likely_fw_tons_df[nrow(likely_fw_tons_df), 8] <- alternative_high
  }

#run a second loop for the "Updated" scenario (reflecting the EPA-2021's updated methodology)
for (s in state.abb) {
  scenario <- solution_levels[6]
  baseline_low <- likely_fw_tons_df %>%
    filter(state == s) %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Repurpose")) %>%
    .$likely_fw_baseline_low %>%
    sum()
  baseline_high <- likely_fw_tons_df %>%
    filter(state == s) %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Repurpose")) %>%
    .$likely_fw_baseline_high %>%
    sum()
  alternative_low <- likely_fw_tons_df %>%
    filter(state == s) %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Repurpose")) %>%
    .$likely_fw_alternative_low %>%
    sum()
  alternative_high <- likely_fw_tons_df %>%
    filter(state == s) %>%
    filter(solution_level %in% c("Prevention", "Rescue", "Repurpose")) %>%
    .$likely_fw_alternative_high %>%
    sum()
  fw_tot <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$total_fw %>%
    mean()
  pop <- likely_fw_tons_df %>%
    filter(state == s) %>%
    .$population %>%
    mean()
  likely_fw_tons_df[nrow(likely_fw_tons_df) + 1, 1] <- s
  likely_fw_tons_df[nrow(likely_fw_tons_df), 2] <- scenario
  likely_fw_tons_df[nrow(likely_fw_tons_df), 3] <- fw_tot
  likely_fw_tons_df[nrow(likely_fw_tons_df), 4] <- pop
  likely_fw_tons_df[nrow(likely_fw_tons_df), 5] <- baseline_low
  likely_fw_tons_df[nrow(likely_fw_tons_df), 6] <- baseline_high
  likely_fw_tons_df[nrow(likely_fw_tons_df), 7] <- alternative_low
  likely_fw_tons_df[nrow(likely_fw_tons_df), 8] <- alternative_high
}

rm(s, scenario, fw_tot, pop, baseline_low, baseline_high, alternative_low, alternative_high)
likely_fw_tons_df <- likely_fw_tons_df %>%
  arrange(solution_level, state)

### DIVERSION DATA AT THE FEDERAL LEVEL ###
#Given that different states have different populations, we need to do this before converting into kg/capita
likely_federal_fw_tons_df <- likely_fw_tons_df %>% group_by(solution_level) %>%
  summarize(total_fw = sum(total_fw),
            population = sum(population),
            likely_fw_baseline_low = sum(likely_fw_baseline_low),
            likely_fw_baseline_high = sum(likely_fw_baseline_high),
            likely_fw_alternative_low = sum(likely_fw_alternative_low),
            likely_fw_alternative_high = sum(likely_fw_alternative_high)) %>%
            tibble()

rm(merged_fw_tons_df)

#### SCRIPT CONTINUES IN 3B/4 ####