
PlotVar <- function(v) {
    simple_sim_env$cov_wide2_df %>% filter(variable == !!v) %>%
        VarianceCompPlot(mean, se) +
        facet_grid(. ~ variable_label, scales="free", labeller=label_parsed) +
        ylab("Variance")
}

legend <- GetLegend(PlotVar("x"))


grid.arrange(
    PlotVar("x") + theme(legend.position="None"),
    PlotVar("log_sigma") + theme(legend.position="None"),
    PlotVar("log_Sigma[z:(Intercept),(Intercept)]") + theme(legend.position="None"),
    legend,
    widths=c(1,1,1,0.5)
)
