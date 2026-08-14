


df <- arm_env$combined_df_long_labeled
reldiff_df <-
    df %>%
    filter(metric == "ij_bootstrap_diff_z") %>%
    group_by(metric_label, big_n_label, has_sigma_label) %>%
    mutate(ij_boot_match=abs(value) > z_threshold) %>%
    summarize(prop_match = mean(ij_boot_match), .groups="keep") %>%
    pivot_wider(names_from="metric_label", values_from="prop_match")


reldiff_tab <- xtable(reldiff_df, digits=c(0,0,0,3))
names(reldiff_tab)[1] <- ""
names(reldiff_tab)[2] <- ""
names(reldiff_tab)[3] <- sprintf("Reject prop. (%d%% level)", p_threshold * 100)

cat("\\begin{center}\n")
print(reldiff_tab, floating=FALSE, include.rownames=FALSE)
cat("\\end{center}\n")
