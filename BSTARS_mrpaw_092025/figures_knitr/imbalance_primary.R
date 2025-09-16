basic_data$imb_plot_df %>%
  filter(!interaction) %>%
  ggplot() + 
  geom_bar(
    aes(fill=name, y=value, x=reg_clean),
    position="dodge", stat="identity") +
  coord_flip() + xlab(NULL) + ylab(NULL) + labs(fill="Method")