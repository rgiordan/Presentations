plot_line_df <-
    microcredit_refit_env$line_df %>%
    filter(site == "Mexico")
ymax <- max(c(plot_line_df$pred, plot_line_df$refit))
ymin <- min(c(0, plot_line_df$pred, plot_line_df$refit))

analysis_df <- microcredit_refit_env$analysis_df

orig_betahat <-
    analysis_df %>%
    filter(site == "Mexico") %>%
    pull(betahat)

reg_se <-
    analysis_df %>%
    filter(site == "Mexico") %>%
    pull(reg_se)

plot1 <- plot_line_df %>%
    ggplot(aes(group=site, x=prop_drop)) +
    geom_line(aes(y=pred, color="Prediction")) +
    theme(legend.title = element_blank()) +
    ylim(ymin, ymax) +
    ylab(TeX("Change in $\\phi$")) +
    geom_hline(aes(yintercept=0)) +
    geom_ribbon(
        aes(ymin=ymin), ymax=reg_se * 1.96, alpha=0.1) +
    xlab("Proportion of points dropped")

plot2 <- plot1 +
    geom_line(aes(y=refit, color="Refit"))
