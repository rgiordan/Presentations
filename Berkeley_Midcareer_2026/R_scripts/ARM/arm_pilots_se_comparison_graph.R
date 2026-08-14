plt <-
  arm_pilots_env$plot_df %>%
    VarianceCompPlot(se, se_se) +
    facet_grid(. ~ param_name, scales="free", labeller = "label_parsed") +
    geom_hline(aes(yintercept=0), color="black") +
    scale_y_log10() +
    ylab("Standard error (log 10 scale)")
print(plt)
