
make_us_plot <- function(map_df, value) {
  squished_value <- sign(value) * sqrt(abs(value))

  map_df %>%
    mutate(value=squished_value) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = value)) +
    geom_polygon(color = "gray70", linewidth = 0.3) +
    scale_fill_gradient2(
      low  = "red",
      mid = "white",
      high = "blue",
      na.value = "gray90",   # color for states with no data
      limits = c(-1, 1),
      midpoint=0,
      name = "Value"
    ) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme_void() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5))
}


MakeUSPoolingPlot <- function(pooling_env) {
  map_df <- pooling_env$map_df
  legend_plt <- 
    make_us_plot(map_df, map_df$w_raking) +
    labs(title = TeX("Raking weight / $N_S$"))+
    theme(legend.position  = "right")
  legend_plt <- GetLegend(legend_plt)

  raking_plt <-
    make_us_plot(map_df, map_df$w_raking) +
    labs(title = TeX("Raking weight / $N_S$"))+
    theme(legend.position  = "none")

  mrp_plt <-
    make_us_plot(map_df, map_df$w_mrplew) +
    labs(title = TeX("MrPlew weight / $N_S$"))+
    theme(legend.position  = "none")

  grid.arrange(
    raking_plt,
    mrp_plt, 
    legend_plt,
    ncol=3,
    widths=c(1,1,0.25))
}
