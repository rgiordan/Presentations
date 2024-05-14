
df <- arm_env$combined_df_long_labeled

# Set boundaries for the axes.
norm_xbound <-
    with(arm_env$combined_df_wide_labeled,
         quantile(c(abs(bayes_bootstrap_diff_norm),
                    abs(ij_bootstrap_diff_norm),
                    2), 0.98, na.rm=TRUE) * 1.05 )

normdiff_metrics <- c("bayes_bootstrap_diff_norm", "ij_bootstrap_diff_norm")
plt <- ggplot(df %>% filter(metric %in% normdiff_metrics,
                            big_n_label == "N >= 240")) +
    geom_density(aes(x=value, y=..density.., fill=metric_label), alpha=0.5) +
    xlim(-norm_xbound, norm_xbound) +
    xlab("Relative error for the bootstrap") +
    ylab("Density of distinct covariance estimates") +
    facet_grid( ~ has_sigma_label, scales="free") +
    theme(legend.position="bottom") +
    scale_fill_discrete(name="") +
    ggtitle("ARM models with N >= 240")
print(plt)
