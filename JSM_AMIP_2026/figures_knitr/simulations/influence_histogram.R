df_plot <- sim_viz_env$save_list$df
alpha <- 0.05
df_plot <- df_plot %>%
    mutate(drop= x <= quantile(df_plot$x, alpha),
           alpha=!!alpha)
bins <- 200
ggplot(df_plot) +
    geom_histogram(aes(x=x, y=..count..), bins=bins) +
    geom_histogram(aes(x=x, y=..count..), bins=bins,
                   data=filter(df_plot, drop), fill="red") +
    xlab(TeX("$N \\psi$")) + ylab("") +
    ggtitle(TeX("Influence score histogram (N = 10000, $\\alpha$ = 0.05)"))
