#### SCRIPT 2/4: SOLUTION EXTRACTION AND APPLICABLE FOOD WASTE QUANTIFICATION ####
library(tidyverse)
library(dplyr)
library(ggplot2)

### RAW FOOD WASTE DIVERSION POTENTIAL QUANTIFICATION (BY RELEVANT SOLUTION) ###
#the following files contain the raw diversion potential values for the 9 and 14
#selected solutions included in the baseline and alternative scenarios, respectively
#due to the waterfall methodology (see Methods section of the corresponding manuscript),
#two datasets are needed, one for each scenario, because adding or removing a solution
#will influence the value for all remaining solutions
solutions_baseline_df <- read.csv("2_fw_solutions_baseline_2022.csv")
solutions_alt_df <- read.csv("2_fw_solutions_alternative_2022.csv")

#select relevant columns
solutions_baseline_df <- solutions_baseline_df %>% select(state,
                                              solution_group, 
                                              solution_name,
                                              annual_tons_diversion_potential) #in US short tons

solutions_alt_df <- solutions_alt_df %>% select(state,
                                                    solution_group, 
                                                    solution_name,
                                                    annual_tons_diversion_potential) #in US short tons

#rename column names for solution levels and diversion potential of food waste
solutions_baseline_df <- solutions_baseline_df %>% rename(solution_level = solution_group, diversion_detail = annual_tons_diversion_potential)
solutions_alt_df <- solutions_alt_df %>% rename(solution_level = solution_group, diversion_detail = annual_tons_diversion_potential)

#convert to appropriate class
solutions_baseline_df$state <- as.factor(solutions_baseline_df$state)
solutions_baseline_df$solution_level <- as.factor(solutions_baseline_df$solution_level)
solutions_baseline_df$solution_name <- as.character(solutions_baseline_df$solution_name)
solutions_baseline_df$diversion_detail <- format(round(solutions_baseline_df$diversion_detail, 3), nsmall = 3, scientific = FALSE)
solutions_baseline_df$diversion_detail <- as.numeric(solutions_baseline_df$diversion_detail)

solutions_alt_df$state <- as.factor(solutions_alt_df$state)
solutions_alt_df$solution_level <- as.factor(solutions_alt_df$solution_level)
solutions_alt_df$solution_name <- as.character(solutions_alt_df$solution_name)
solutions_alt_df$diversion_detail <- format(round(solutions_alt_df$diversion_detail, 3), nsmall = 3, scientific = FALSE)
solutions_alt_df$diversion_detail <- as.numeric(solutions_alt_df$diversion_detail)

#change state names to state abbreviation for better compatibility later on
#note this can be done because levels of state are in the same order as R vector state.abb
levels(solutions_baseline_df$state) <- state.abb
levels(solutions_alt_df$state) <- state.abb

## IMPORTANT ##
#for solutions_alt_df, divide education campaigns by 2 since 50% assigned to recycling and 50% to prevention
#note this does not apply to donation education as both liability protection and tax incentives
#were assigned to rescue policy category (thus 50% + 50% = 100%)
#it is easier to perform this step here than changing allocations later on
solutions_alt_df$diversion_detail[solutions_alt_df$solution_name %in%
                                c("Consumer Education Campaigns", "K-12 Education Campaigns")] <- 
  (solutions_alt_df$diversion_detail[solutions_alt_df$solution_name %in% 
                                   c("Consumer Education Campaigns", "K-12 Education Campaigns")])/2

#change policy category assignment of solution "Livestock Feed" from "Recycling" to "Repurpose"
solutions_baseline_df <- solutions_baseline_df %>%
  mutate(solution_level = ifelse(solution_name == "Livestock Feed", "Repurpose", as.character(solution_level)))
solutions_baseline_df$solution_level <- as.factor(solutions_baseline_df$solution_level)

solutions_alt_df <- solutions_alt_df %>%
  mutate(solution_level = ifelse(solution_name == "Livestock Feed", "Repurpose", as.character(solution_level)))
solutions_alt_df$solution_level <- as.factor(solutions_alt_df$solution_level)

#change dataframe order
solutions_baseline_df <- solutions_baseline_df %>%
  arrange(state, solution_level, solution_name)

solutions_alt_df <- solutions_alt_df %>%
  arrange(state, solution_level, solution_name)

#create vectors with relevant solutions ("Updated" represents the updated EPA-2021 methodology and = All - Recycling)
solution_levels <- c("Prevention", "Rescue", "Repurpose", "Recycling", "All", "Updated")

### APPLICABLE FOOD WASTE DIVERSION POTENTIAL QUANTIFICATION (BY POLICY CATEGORY) ###
#create empty dataframe with column names and nrow = 0
applicable_fw_tons_df = data.frame(matrix(nrow = 0, ncol = 4))
colnames(applicable_fw_tons_df) = c("state", "solution_level", "applicable_baseline", "applicable_alternative")

#run a for loop that calculates applicable diversion potential per level per state
#assign each loop run to dataframe above, with both baseline and alternative scenarios
for (s in state.abb) {
  for (l in solution_levels[1:4]) {
    baseline <- solutions_baseline_df %>%
      filter(state == s & solution_level == l) %>%
      .$diversion_detail %>%
      sum()
    alternative <- solutions_alt_df %>%
      filter(state == s & solution_level == l) %>%
      .$diversion_detail %>%
      sum()
    temp <- c(s, l, baseline, alternative)
    applicable_fw_tons_df[nrow(applicable_fw_tons_df) + 1, ] <- temp
  }
}
rm(s, l, baseline, alternative, temp)

write.csv(applicable_fw_tons_df, "2_applicable_fw_tons.csv", row.names = FALSE)
rm(solutions_alt_df, solutions_baseline_df)

#### END OF SCRIPT 2/4 ####