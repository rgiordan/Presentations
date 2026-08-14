

MakePlot <- function(v, df, y_label, use_legend=FALSE) {
    plt <-
        df %>% filter(variable == v) %>%
        VarianceCompPlot(mean, se) +
        facet_grid(. ~ variable_label, scales="free", labeller=label_parsed) +
        ylab(y_label)
    if (!use_legend) {
        plt <- plt + theme(legend.position="None")
    }
    return(plt)
}


cov_wide_df <- bats_env$cov_wide_df
cross_cov_wide_df <- bats_env$cross_cov_wide_df

plots <- list()
for (v in unique(cov_wide_df$variable)) {
    plots[[v]] <- MakePlot(v, cov_wide_df, "Variance")
}
for (v in unique(cross_cov_wide_df$variable)) {
    plots[[v]] <- MakePlot(v, cross_cov_wide_df, "Covariance")
}

grid.arrange(
    plots[[1]],
    plots[[2]],
    plots[[3]],
    plots[[4]],
    plots[[5]],
    plots[[6]],
    ncol=3
)
