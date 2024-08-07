
df <-
    arm_env$combined_df_long_labeled %>%
    filter(metric == "ij_bootstrap_diff_z")

# Set boundaries for the axes.
z_xbound <- quantile(c(abs(df$value), 2), 0.98, na.rm=TRUE) * 1.05

# Set labels for the facets
label_df <-
    group_by(df, has_sigma_label, big_n_label) %>%
    summarize(reject_prop=sprintf("Rejected:\n%0.1f%%",
      100 * mean(abs(value) > z_threshold)))

num_bins <- 30
plt <- ggplot(df) +
    geom_histogram(aes(x=value, y=..density..), alpha=0.5, bins=num_bins) +
    xlim(-z_xbound, z_xbound) +
    geom_text(aes(x=4, y=0.3, label=reject_prop), data=label_df) +
    xlab("Z-score for testing difference from bootstrap covariance") +
    ylab("Proportion of distinct covariance estimates") +
    geom_vline(aes(xintercept=-z_threshold), color="red") +
    geom_vline(aes(xintercept=z_threshold), color="red") +
    facet_grid(has_sigma_label ~ big_n_label, scales="free")
print(plt)
