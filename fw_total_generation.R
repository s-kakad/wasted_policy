#### SCRIPT 1/4: TOTAL FOOD WASTE GENERATION QUANTIFICATION ####
library(tidyverse)
library(dplyr)
library(readr)

### FOOD WASTE DATA EXTRACTION ###
fw_tot_raw_df <- read.csv("ReFED_food_surplus_summary_2022.csv") # note for further iterations the "2022" suffix will change
#note that while the file is titled 'surplus', we will extract waste tons to align with EPA methodology

#select relevant columns
fw_tot_raw_df <- fw_tot_raw_df %>% filter(year == 2022) %>%
                select(state, sector, tons_waste)

#convert to appropriate class
fw_tot_raw_df$state <- as.factor(fw_tot_raw_df$state)
fw_tot_raw_df$sector <- as.factor(fw_tot_raw_df$sector)
fw_tot_raw_df$tons_waste <- format(round(fw_tot_raw_df$tons_waste, 3), nsmall = 3, scientific = FALSE)
fw_tot_raw_df$tons_waste <- as.numeric(fw_tot_raw_df$tons_waste)

#change state names to state abbreviation for better compatibility later on
#note this can be done because levels of state are in the same order as R vector state.abb
levels(fw_tot_raw_df$state) <- state.abb

#quantify food waste for 2022 apart from farming and manufacturing (i.e. surplus)
fw_tot_processed_df <- fw_tot_raw_df %>%
                            filter(sector != "Farm" & sector != "Manufacturing") %>%
                            group_by(state) %>%
                            summarise(total_fw = format(round(sum(tons_waste), 3), nsmall = 3))

fw_tot_processed_df$total_fw <- as.numeric(fw_tot_processed_df$total_fw)

write.csv(fw_tot_processed_df, "maximum_fw_tons.csv", row.names = FALSE)
rm(fw_tot_raw_df)

#these numbers have been added to the fw_masterfile_2022.csv, alongside 2022 population data and policy scores
#note data is in US short tons (1 short ton = 0.907185 metric tonne)

rm(fw_tot_processed_df)
