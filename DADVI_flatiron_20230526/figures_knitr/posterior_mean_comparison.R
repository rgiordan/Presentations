posterior_comp_df <- post_env$posterior_comp_df

arm_legend <- GetColorLegend(unique(posterior_comp_df$method_2))
arm_mean_plot <-
    posterior_comp_df %>%
    filter(is_arm, method_1 == "DADVI") %>%
    PlotPostComparison(mean_z_rmse_1, mean_z_rmse_2) +
    xlab(TeX("DADVI relative error $\\epsilon_{DADVI}^{\\mu}$ (log10 scale)")) +
    ylab(TeX("Stochastic VI relative error $\\epsilon_{...}^{\\mu}$ (log10 scale)")) +
    ggtitle("Mean relative error\n(ARM models)")

posterior_comp_df <- post_env$posterior_comp_df

nonarm_legend <- GetColorLegend(
    unique(posterior_comp_df$method_2),
    include_models=TRUE,
    models=filter(posterior_comp_df, !is_arm) %>% pull(model) %>% unique())

nonarm_mean_plot <-
    posterior_comp_df %>%
    filter(!is_arm, method_1 == "DADVI") %>%
    PlotPostComparison(
      mean_z_rmse_1, mean_z_rmse_2, plot_dens=FALSE, model_label=TRUE) +
    xlab(TeX("DADVI relative error $\\epsilon_{DADVI}^{\\mu}$ (log10 scale)")) +
    ylab(TeX("Stochastic VI relative error $\\epsilon_{...}^{\\mu}$ (log10 scale)")) +
    ggtitle("Mean relative error\n(non-ARM models)")

grid.arrange(
    arm_mean_plot, arm_legend,
    nonarm_mean_plot, nonarm_legend,
    widths=c(1.5,1,1.5,1)
)
