likely_fw_kg_per_capita_df_reordered_all <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "All") %>%
  rowwise() %>% 
  mutate(mymean_all = mean(c(total_fw_kg_per_capita - likely_fw_baseline_low_kg_per_capita, 
                                 total_fw_kg_per_capita - likely_fw_baseline_high_kg_per_capita,
                                 total_fw_kg_per_capita - likely_fw_alternative_low_kg_per_capita,
                                 total_fw_kg_per_capita - likely_fw_alternative_high_kg_per_capita))) %>%
  arrange(desc(mymean_all)) %>%
  mutate(state = factor(state, levels = state[order(mymean_all)]))

#arrange by order of mean based on the "All" scenario
likely_fw_kg_per_capita_df <- likely_fw_kg_per_capita_df %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = levels(likely_fw_kg_per_capita_df_reordered_all$state)))

#plot graph
plot_state_projected_generation_all <- likely_fw_kg_per_capita_df %>%
  filter(solution_level == "All") %>%
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
  #scale_y_continuous(trans = "log10",
  #                   limits = c(10, 465),
  #                   breaks = c(10, 50, 100, 200, 300, 400),
  #                   labels = c(10, 50, 100, 200, 300, 400)) +
  #scale_y_break(c(300, 400)) +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Likely food waste generation (kg/capita)")

pdf("likely_fw_generation_all.pdf")
print(plot_state_projected_generation_all)   
dev.off()
