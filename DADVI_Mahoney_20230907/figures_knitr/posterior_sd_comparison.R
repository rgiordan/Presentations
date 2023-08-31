posterior_comp_df <- post_env$posterior_comp_df

arm_legend <- GetColorLegend(unique(posterior_comp_df$method_2))

arm_sd_plot <-
    posterior_comp_df %>%
    filter(is_arm, method_1 == "LR") %>%
    PlotPostComparison(sd_rel_rmse_1, sd_rel_rmse_2) +
    xlab(TeX("LRVB relative error $\\epsilon_{LRVB}^{\\sigma}$ (log10 scale)")) +
    ylab(TeX("Stochastic VI relative error $\\epsilon_{...}^{\\sigma}$ (log10 scale)")) +
    ggtitle("Standard deviation relative error\n(ARM models)")

posterior_comp_df <- post_env$posterior_comp_df

nonarm_legend <- GetColorLegend(
    unique(posterior_comp_df$method_2),
    include_models=TRUE,
    models=filter(posterior_comp_df, !is_arm) %>% pull(model) %>% unique())

nonarm_sd_plot <-
    posterior_comp_df %>%
    filter(!is_arm, method_1 == "LR") %>%
    PlotPostComparison(
      sd_rel_rmse_1, sd_rel_rmse_2, plot_dens=FALSE, model_label=TRUE) +
    xlab(TeX("LRVB relative error $\\epsilon_{LRVB}^{\\sigma}$ (log10 scale)")) +
    ylab(TeX("Stochastic VI relative error $\\epsilon_{...}^{\\sigma}$ (log10 scale)")) +
    ggtitle("Standard deviation relative error\n(non-ARM models)")

grid.arrange(
    arm_sd_plot, arm_legend,
    nonarm_sd_plot, nonarm_legend,
    widths=c(1.5,1,1.5,1)
)
