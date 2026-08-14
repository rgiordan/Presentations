worst_state <- election_env$worst_state
best_state <- election_env$best_state
election_env$wide_df %>%
    mutate(se=case_when(is.na(se) ~ mcmc_se, TRUE ~ se)) %>%
    filter(par %in% c("natl_vote", worst_state, best_state)) %>%
    mutate(par=ordered(
        par,
        levels=c("natl_vote", worst_state, best_state),
        labels=c("National vote",
                 sprintf("%s (worst-fit state)", worst_state),
                 sprintf("%s (best-fit state)", best_state)))) %>%
    VarianceCompPlot(cov, se) +
    scale_y_log10() +
    ylab("Variance (log 10 scale)") +
    facet_grid(. ~ par, scales="free")
