
# Set the order and labels of the cash transfers studies
cash_levels <- c()
cash_labels <- c()
for (regressor in c("treatp", "treatnp")) {
    regressor_label <-
        case_when(regressor == "treatp" ~ "Poor",
                  regressor == "treatnp" ~ "Non-poor",
                  TRUE ~ "error")
    for(time in c(8,9,10)) {
        cash_levels <- c(cash_levels, sprintf("%s, t=%d", regressor, time))
        cash_labels <- c(
            cash_labels,
            sprintf("%s, period %d", regressor_label, time))
    }
}
stopifnot(setequal(cash_levels, unique(cash_env$results_df$study_case)))


results_df <-
    cash_env$results_df %>%
    filter(analysis=="rerun") %>%
    mutate(study_case=ordered(
        study_case, levels=cash_levels, labels=cash_labels))

base_df <-
    cash_env$base_df %>%
    mutate(study_case=ordered(
        study_case, levels=cash_levels, labels=cash_labels))


table_df <-
  FormatRefitTable(results_df, base_df) %>%
  filter(study_case %in% c("Poor, period 10")) %>%
  mutate(study_case=ordered(study_case))


# I didn't save the total number of observations so infer it from the
# proportion and number dropped.
n_obs <- table_df$n_drop / table_df$prop_drop
stopifnot(length(unique(n_obs)) == 1)
n_obs <- as.integer(unique(n_obs))

RenderSimpleLatexTable(
    table_df,
    label="cash_transfers_re_run_table",
    caption=paste0("Cash transfers results (N = ", n_obs,
      ") \\citep{angelucci2009indirect}"))

# caption=paste0(
#     "Cash transfers results for the final study period. ",
#     GetTableCaptionBoilerplate())
