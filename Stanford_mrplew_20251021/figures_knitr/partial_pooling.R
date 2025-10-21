pp_df <- pp_env$pp_df

print(
    ggplot(pp_df) +
  geom_raster(aes(x=region_pop, y=region_sur, fill=infl_norm)) +
  scale_fill_gradient(low="black", high = "white", limits = c(0, 1)) +
  labs(fill = "Proportion of total weight") +
  xlab("Population region") +
  ylab("Survey region")
)

