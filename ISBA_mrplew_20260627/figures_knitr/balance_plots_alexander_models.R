
# Combine the different models into one dataframe
balance_comb_df <- alexander$balance_df %>% mutate(model_name="original")
mrp_list <- list()
mrp_list[["original"]]  <- alexander$save_list$mrp
for (model_name in names(alexander_model_list)) {
  balance_comb_df <-
    bind_rows(balance_comb_df, 
              alexander_model_list[[model_name]]$balance_df %>%
                mutate(model_name=!!model_name))
  mrp_list[[model_name]] <- alexander_model_list[[model_name]]$base_mrp
}

balance_comb_df <-
  balance_comb_df %>%
  filter(enough_data) %>%
  filter(method %in% c("mrp", "raking")) %>%
  CleanNames("alexander")


# Identify regressors that we want to plot.  Keep
# anything that induces a MrP change above some threshold
# in at least one model.
if (FALSE) {
    # Threshold on percent MrP change
    threshold <- 20
    keep_regs <- 
    balance_comb_df %>%
    filter(abs(pct_diff) > threshold) %>%
    pull(reg) %>%
    unique()
}
threshold <- 0.03
keep_regs <- 
    balance_comb_df %>%
    filter(abs(difference) > threshold) %>%
    pull(reg) %>%
    unique()
length(keep_regs)

balance_comb_plot_df <-
  balance_comb_df %>%
  filter(reg %in% keep_regs)

# Plot all the balance plots in the same format
xmax <- 1.1 * max(abs(balance_comb_plot_df$difference))

PlotAlexanderModelBalance <- function(title, model_name) {
  balance_comb_plot_df %>%
    filter(model_name==!!model_name) %>%
    PlotBalance() +
    ylim(0, xmax) +
    ggtitle(sprintf("%s\nMrp = %0.3f",
                    title,
                    mrp_list[[model_name]]))
}

grid.arrange(
  PlotAlexanderModelBalance(
    "Original model", "original"),
  PlotAlexanderModelBalance(
    "Original model + (1 | decade_married:educ)", "interaction")
  ,
  PlotAlexanderModelBalance(
    "Original model + decade_married:educ", "fixedeffects_interaction")
  , ncol=1
)

