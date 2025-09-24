
get_primary_imbalance_plot <- function(basic_data) {
  basic_data$imb_plot_df %>%
    filter(!interaction) %>%
    ggplot() +
    geom_bar(
      aes(fill=name, y=value, x=reg_clean),
      position="dodge", stat="identity") +
    coord_flip() + xlab(NULL) + ylab(NULL) + labs(fill="Method")
}

alex_primary_imb <- get_primary_imbalance_plot(alex$basic_data)
lax_primary_imb <- get_primary_imbalance_plot(lax$basic_data)