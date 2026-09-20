
make_2d_pooling_plot <- function(pooling_df, value, limits=c(-1, 1)) {
  squished_value <- sign(value) * sqrt(abs(value))
  if (max(squished_value) > limits[2]) {
    warning("Outside upper range!")
  }
  if (min(squished_value) < limits[1]) {
    warning("Outside lower range!")
  }
  
  pooling_df %>%
    mutate(value=squished_value) %>%
    ggplot() +
    geom_tile(aes(x=decade_married_rk, y=educ_group, fill=value)) +
    scale_fill_gradient2(
      low  = "red",
      mid = "white",
      high = "blue",
      na.value = "gray90",   # color for states with no data
      limits = limits,
      midpoint=0,
      name = "Value") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank()) +
    xlab("Decade married") + ylab("Education")
}

limit <- 1.0
group_cols <- c("decade_married_rk", "educ_group")
pooling_df <- alexander_pooling$pooling_df
grid.arrange(
  make_2d_pooling_plot(pooling_df, pooling_df$w_raking, limits=c(-limit, limit)) +
    ggtitle("Raking"),
  make_2d_pooling_plot(pooling_df, pooling_df$w_mrplew, limits=c(-limit, limit)) +
    ggtitle("MrP"),
  ncol=2
)
