
make_imbalance_plots <- function(analysis_list) {

  basic_data  <- analysis_list$basic_data
  sim_data <- analysis_list$sim_data

  primary_plt <-
    basic_data$imb_plot_df %>%
      filter(!interaction) %>%
      ggplot() +
      geom_bar(
        aes(fill=name, y=value, x=reg_clean),
        position="dodge", stat="identity") +
      coord_flip() + xlab(NULL) + ylab(NULL) + labs(fill="Method")


  bad_regs <- basic_data$basic_metrics$imb_df %>%
    mutate(mrp_imbalance=mrp_x - poststrat_x) %>%
    filter(interaction) %>%
    arrange(abs(mrp_imbalance)) %>%
    tail(10) %>%
    pull(reg)

  bad_regs <- union(bad_regs, sim_data$col_pert)

  interaction_imb_df <-
    basic_data$imb_plot_df %>%
    filter(interaction) %>%
    filter(reg %in% bad_regs) %>%
    mutate(reg_clean=case_when(
        reg_clean == basic_data$pert_col_clean ~ paste0(" ---> ", reg_clean),
        TRUE ~ reg_clean))



  interaction_plt <-
    interaction_imb_df %>%
    ggplot() +
    geom_bar(
      aes(fill=name, y=value, x=reg_clean),
      position="dodge", stat="identity") +
    coord_flip() + xlab(NULL) + ylab(NULL) + labs(fill="Method")


  return(list(interaction_plt=interaction_plt, primary_plt=primary_plt))
}


alex_imb_plots <- make_imbalance_plots(alex)
lax_imb_plots <- make_imbalance_plots(lax)