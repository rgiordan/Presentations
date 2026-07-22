results_df <-
    microcredit_env$results_df  %>%
    filter(analysis=="rerun") %>%
    filter(site == "Mexico") %>%
    mutate(study_case=ordered(site))

base_df <-
    microcredit_env$base_df %>%
    filter(site == "Mexico") %>%
    mutate(study_case=ordered(site))

table_df <- FormatRefitTable(results_df, base_df)

# I didn't save the total number of observations so infer it from the
# proportion and number dropped.
n_obs <- table_df$n_drop / table_df$prop_drop
stopifnot(length(unique(n_obs)) == 1)
n_obs <- as.integer(unique(n_obs))

RenderSimpleLatexTable(
    table_df,
    label="mc_profit_results",
    caption=paste0(
        "Microcredit Mexico results (N = ", n_obs,
        ") \\citep{angelucci2015microcredit}."
    ))

    #
    # paste0(
    #   "Microcredit regressions for the profit outcome. ",
    #   GetTableCaptionBoilerplate())
