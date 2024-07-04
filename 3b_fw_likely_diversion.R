#### SCRIPT 3B/4:LIKELY FOOD WASTE DIVERSION QUANTIFICATION IN KG/CAPITA ###
library(tidyverse)
library(dplyr)
library(ggplot2)

#note: 1 US short ton = 0.907185 metric tonne & 1 metric tonne = 1000 kg

### FEDERAL LEVEL (50 states included in this policy analysis, not all US territories) ###
likely_federal_fw_kg_per_capita_df <- likely_federal_fw_tons_df %>%
      mutate(total_fw_kg_per_capita = total_fw*0.907185*1000/population,
             likely_fw_baseline_low_kg_per_capita = likely_fw_baseline_low*0.907185*1000/population,
             likely_fw_baseline_high_kg_per_capita = likely_fw_baseline_high*0.907185*1000/population,
             likely_fw_alternative_low_kg_per_capita = likely_fw_alternative_low*0.907185*1000/population,
             likely_fw_alternative_high_kg_per_capita = likely_fw_alternative_high*0.907185*1000/population,
             likely_fw_baseline_mean_kg_per_capita = (likely_fw_baseline_low_kg_per_capita + likely_fw_baseline_high_kg_per_capita)/2,
             likely_fw_alternative_mean_kg_per_capita = (likely_fw_alternative_low_kg_per_capita + likely_fw_alternative_high_kg_per_capita)/2) %>%
    select(solution_level, total_fw_kg_per_capita, likely_fw_baseline_low_kg_per_capita, likely_fw_baseline_high_kg_per_capita,
           likely_fw_alternative_low_kg_per_capita, likely_fw_alternative_high_kg_per_capita,
           likely_fw_baseline_mean_kg_per_capita, likely_fw_alternative_mean_kg_per_capita)

write.csv(likely_federal_fw_kg_per_capita_df, "3_likely_federal_diversion_kg_per_capita.csv", row.names = FALSE)

### STATE LEVEL ###
likely_fw_kg_per_capita_df <- likely_fw_tons_df %>%
  mutate(total_fw_kg_per_capita = total_fw*0.907185*1000/population,
         likely_fw_baseline_low_kg_per_capita = likely_fw_baseline_low*0.907185*1000/population,
         likely_fw_baseline_high_kg_per_capita = likely_fw_baseline_high*0.907185*1000/population,
         likely_fw_alternative_low_kg_per_capita = likely_fw_alternative_low*0.907185*1000/population,
         likely_fw_alternative_high_kg_per_capita = likely_fw_alternative_high*0.907185*1000/population,
         likely_fw_baseline_mean_kg_per_capita = (likely_fw_baseline_low_kg_per_capita + likely_fw_baseline_high_kg_per_capita)/2,
         likely_fw_alternative_mean_kg_per_capita = (likely_fw_alternative_low_kg_per_capita + likely_fw_alternative_high_kg_per_capita)/2) %>%
  select(state, solution_level, total_fw_kg_per_capita, likely_fw_baseline_low_kg_per_capita, likely_fw_baseline_high_kg_per_capita,
         likely_fw_alternative_low_kg_per_capita, likely_fw_alternative_high_kg_per_capita,
         likely_fw_baseline_mean_kg_per_capita, likely_fw_alternative_mean_kg_per_capita) %>%
  arrange(solution_level)

write.csv(likely_fw_kg_per_capita_df, "3_likely_diversion_kg_per_capita.csv", row.names = FALSE)

rm(likely_federal_fw_tons_df, likely_fw_tons_df)

#### END OF SCRIPT 3/4 ####
