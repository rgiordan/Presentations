results_df <-
    microcredit_env$results_df  %>%
    filter(analysis=="rerun") %>%
    filter(site == "Mexico") %>%
    mutate(study_case=ordered(site))

base_df <-
    microcredit_env$base_df %>%
    filter(site == "Mexico") %>%
    mutate(study_case=ordered(site))

FormatRefitTable(results_df, base_df) %>%
  RenderSimpleLatexTable(
    label="mc_profit_results",
    caption="Microcredit Mexico results \\citep{angelucci2015microcredit}.")

    #
    # paste0(
    #   "Microcredit regressions for the profit outcome. ",
    #   GetTableCaptionBoilerplate())
