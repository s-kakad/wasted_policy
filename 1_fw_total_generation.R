#### SCRIPT 1/4: TOTAL FOOD WASTE GENERATION QUANTIFICATION ####
library(tidyverse)
library(dplyr)
library(readr)

### FOOD WASTE DATA EXTRACTION ###
fw_tot_raw_df <- read.csv("1_food_surplus_summary_2022.csv")
#note for further iterations the "2022" suffix will change
#note that while the file is titled 'surplus', we will extract wasted tons ("tons_waste")
#to align with EPA methodology, which defines food waste as: "food from food retail, food service,
#and households that has been removed from the human supply chain" (EPA, 2024)

#select relevant columns
#remember to set working directory e.g. setwd("~/your-folder-name/wasted-policy-project")
fw_tot_raw_df <- fw_tot_raw_df %>% filter(year == 2022) %>%
                              select(state,
                                     sector,
                                     tons_waste) #in US short tons (0.907185 metric tonne)

#convert to appropriate class
fw_tot_raw_df$state <- as.factor(fw_tot_raw_df$state)
fw_tot_raw_df$sector <- as.factor(fw_tot_raw_df$sector)
fw_tot_raw_df$tons_waste <- format(round(fw_tot_raw_df$tons_waste, 3), nsmall = 3, scientific = FALSE)
fw_tot_raw_df$tons_waste <- as.numeric(fw_tot_raw_df$tons_waste)

#change state names to state abbreviation for better compatibility later on
#note this can be done because levels of state are in the same order as R vector state.abb
levels(fw_tot_raw_df$state) <- state.abb

#quantify food waste for 2022 apart from farming and manufacturing (not included in EPA definition)
fw_tot_processed_df <- fw_tot_raw_df %>%
                            filter(sector != "Farm" & sector != "Manufacturing") %>%
                            group_by(state) %>%
                            summarize(total_fw = format(round(sum(tons_waste), 3), nsmall = 3))

fw_tot_processed_df$total_fw <- as.numeric(fw_tot_processed_df$total_fw)

### MERGING TOTAL FOOD WASTE GENERATION WITH POPULATION AND POLICY SCORING DATA ###
fw_policy_scores_df <- read.csv("1_policy_scores_2022.csv")
#contains 2022 population data, qualitative policy scores and corresponding high and low conversation factors

#prepare dataframe for merging
fw_policy_scores_df$state <- as.factor(fw_policy_scores_df$state)
fw_policy_scores_df$state <- factor(fw_policy_scores_df$state, levels = state.abb)
fw_policy_scores_df$population <- as.numeric(fw_policy_scores_df$population)
fw_policy_scores_df$solution_level <- as.factor(fw_policy_scores_df$solution_level)
fw_policy_scores_df$policy_qual_score <- as.factor(fw_policy_scores_df$policy_qual_score)
fw_policy_scores_df$factor_low <- as.numeric(fw_policy_scores_df$factor_low)
fw_policy_scores_df$factor_high <- as.numeric(fw_policy_scores_df$factor_high)

#merge dataframes
total_fw_tons_df <- merge(fw_policy_scores_df, fw_tot_processed_df)
total_fw_tons_df <- total_fw_tons_df %>% arrange(state)

write.csv(total_fw_tons_df, "1_total_fw_tons.csv", row.names = FALSE)
rm(fw_tot_raw_df, fw_policy_scores_df, fw_tot_processed_df)

#### END OF SCRIPT 1/4 ####