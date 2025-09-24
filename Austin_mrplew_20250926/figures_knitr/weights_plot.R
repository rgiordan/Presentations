make_weight_scatterplot <- function(analysis_list) {
  plt <-
    ggplot(analysis_list$basic_data$basic_metrics$weight_df) +
    geom_point(aes(x=w_mrpaw, y=w_raking * length(w_raking)), alpha=0.2) +
    geom_abline() +
    geom_vline(aes(xintercept=0)) +
    xlab(TeX("$w^{MRP}_i$")) +
    ylab(TeX("$w^{CAL}_i$"))
  return(plt)
}


alex_weight_scatterplot <- make_weight_scatterplot(alex)
lax_weight_scatterplot <- make_weight_scatterplot(lax)


