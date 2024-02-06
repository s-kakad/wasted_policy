#### SCRIPT 3B/4:LIKELY FOOD WASTE DIVERSION QUANTIFICATION IN KG/CAPITA ###
library(tidyverse)
library(dplyr)
library(ggplot2)

#1 US short ton = 0.907185 metric tonne & 1 metric tonne = 1000 kg

### FEDERAL LEVEL (50 states included in this policy analysis, not all US territories) ###
likely_federal_fw_kg_per_capita_df <- likely_federal_fw_tons_df %>%
      mutate(total_fw_kg_per_capita = total_fw*0.907185*1000/population,
             likely_fw_baseline_low_kg_per_capita = likely_fw_baseline_low*0.907185*1000/population,
             likely_fw_baseline_high_kg_per_capita = likely_fw_baseline_high*0.907185*1000/population,
             likely_fw_alternative_low_kg_per_capita = likely_fw_alternative_low*0.907185*1000/population,
             likely_fw_alternative_high_kg_per_capita = likely_fw_alternative_high*0.907185*1000/population) %>%
    select(solution_level, total_fw_kg_per_capita, likely_fw_baseline_low_kg_per_capita, likely_fw_baseline_high_kg_per_capita,
           likely_fw_alternative_low_kg_per_capita, likely_fw_alternative_high_kg_per_capita)

write.csv(likely_federal_fw_kg_per_capita_df, "likely_federal_diversion_kg_per_capita.csv", row.names = FALSE)

### STATE LEVEL ###
likely_fw_kg_per_capita_df <- likely_fw_tons_df %>%
  mutate(total_fw_kg_per_capita = total_fw*0.907185*1000/population,
         likely_fw_baseline_low_kg_per_capita = likely_fw_baseline_low*0.907185*1000/population,
         likely_fw_baseline_high_kg_per_capita = likely_fw_baseline_high*0.907185*1000/population,
         likely_fw_alternative_low_kg_per_capita = likely_fw_alternative_low*0.907185*1000/population,
         likely_fw_alternative_high_kg_per_capita = likely_fw_alternative_high*0.907185*1000/population) %>%
  select(state, solution_level, total_fw_kg_per_capita, likely_fw_baseline_low_kg_per_capita, likely_fw_baseline_high_kg_per_capita,
         likely_fw_alternative_low_kg_per_capita, likely_fw_alternative_high_kg_per_capita) %>%
  arrange(solution_level)

write.csv(likely_fw_kg_per_capita_df, "likely_diversion_kg_per_capita.csv", row.names = FALSE)

### FURTHER TRANSFORMATION TO HAVE ALL DIVERSION DATA IN A SINGLE COLUMN ###
names(likely_fw_kg_per_capita_df)
likely_fw_kg_per_capita_df_alt <- data.frame(state = rep(likely_fw_kg_per_capita_df$state, 4),
                                    solution_level = rep(likely_fw_kg_per_capita_df$solution_level, 4),
                                    total_fw_kg_per_capita = rep(likely_fw_kg_per_capita_df$total_fw_kg_per_capita, 4),
                                    scenario = c(rep("baseline", 600), rep("alternative", 600)),
                                    conversion_factor = c(rep("low", 300), rep("high", 300), rep("low", 300), rep("high", 300)),
                                    diversion_potential_kg_per_capita = c(likely_fw_kg_per_capita_df$likely_fw_baseline_low_kg_per_capita,
                                                                 likely_fw_kg_per_capita_df$likely_fw_baseline_high_kg_per_capita,
                                                                 likely_fw_kg_per_capita_df$likely_fw_alternative_low_kg_per_capita,
                                                                 likely_fw_kg_per_capita_df$likely_fw_alternative_high_kg_per_capita))


likely_fw_kg_per_capita_df_alt$scenario <- as.factor(likely_fw_kg_per_capita_df_alt$scenario)
likely_fw_kg_per_capita_df_alt$conversion_factor <- as.factor(likely_fw_kg_per_capita_df_alt$conversion_factor)
likely_fw_kg_per_capita_df_alt$solution_level <- factor(likely_fw_kg_per_capita_df_alt$solution_level,
                                                        levels = c("Prevention", "Rescue", "Animal Feed", "Recycling",
                                                                   "All", "Updated"))

likely_fw_kg_per_capita_df_alt <- likely_fw_kg_per_capita_df_alt %>%
  arrange(solution_level, state, scenario, conversion_factor)

write.csv(likely_fw_kg_per_capita_df_alt, "likely_fw_kg_per_capita_alt.csv", row.names = TRUE)

#rm(likely_federal_fw_tons_df, likely_fw_tons_df)
