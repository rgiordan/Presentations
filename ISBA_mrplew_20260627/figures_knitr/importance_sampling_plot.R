

is_label <- "Importance sampling (true posterior)"
is_df <- laxphilips_importance_sampling$is_df
is_df %>%
  filter(neff > 1000, yalt == "xcol") %>%
  ggplot() +
  geom_point(aes(x=epsilon, y=mrp1 - mrp0, color=is_label)) +
  geom_line(aes(x=epsilon, y=mrp1 - mrp0, color=is_label)) +
  geom_point(aes(x=epsilon, y=pred_diff, color="MrPlew")) +
  geom_line(aes(x=epsilon, y=pred_diff, color="MrPlew")) +
  ylab(TeX("$\\hat{\\mu}^{MrP}(\\epsilon) - \\hat{\\mu}^{MrP}$$")) +
  xlab(TeX("$\\epsilon$")) +
  MethodColorScale(extra_levels=is_label)