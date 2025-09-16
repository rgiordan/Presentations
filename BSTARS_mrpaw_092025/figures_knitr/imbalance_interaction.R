basic_data$imb_plot_df %>%
  mutate(reg_clean=case_when(
    reg_clean == basic_data$pert_col_clean ~ paste0(" ---> ", reg_clean),
    TRUE ~ reg_clean)) %>%
  filter(interaction) %>%
  ggplot() + 
  geom_bar(
    aes(fill=name, y=value, x=reg_clean),
    position="dodge", stat="identity") +
  # theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
  #       axis.ticks.x=element_blank()) +
  coord_flip() + xlab(NULL) + ylab(NULL) + labs(fill="Method")