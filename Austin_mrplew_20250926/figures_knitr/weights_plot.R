
plt <- 
  ggplot(alex$basic_data$basic_metrics$weight_df) +
  geom_point(aes(x=w_mrpaw, y=w_raking * length(w_raking)), alpha=0.2) +
  geom_abline() +
  geom_vline(aes(xintercept=0)) +
  xlab(TeX("$w^{MRP}_i$")) +
  ylab(TeX("$w^{CAL}_i$"))
print(plt)