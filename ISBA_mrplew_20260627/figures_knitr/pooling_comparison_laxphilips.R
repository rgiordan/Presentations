
comp_df <- bind_rows(
  laxphilips_MO_pooling$pooling_df %>% mutate(analysis="MO"),
  laxphilips_CA_pooling$pooling_df %>% mutate(analysis="CA")
) %>%
  pivot_wider(id_cols=c(state, region), names_from=analysis, 
              values_from=c(w_raking, w_mrplew))

grid.arrange(
  ggplot(comp_df) +
    geom_point(aes(x=w_raking_MO, y=w_raking_CA)) +
    xlab("Weights targeting MO") + ylab("Weights targeting CA") +
    ggtitle("Raking state weights"),
  ggplot(comp_df) +
    geom_point(aes(x=w_mrplew_MO, y=w_mrplew_CA)) +
    xlab("Weights targeting MO") + ylab("Weights targeting CA") +
    ggtitle("Mrplew state weights"),
  ncol=2
)
