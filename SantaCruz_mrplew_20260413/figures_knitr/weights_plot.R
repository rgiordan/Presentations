# make_weight_scatterplot <- function(analysis_list) {
#   plt <-
#     ggplot(analysis_list$basic_data$basic_metrics$weight_df) +
#     geom_point(aes(x=w_mrpaw, y=w_raking * length(w_raking)), alpha=0.2) +
#     geom_abline() +
#     geom_vline(aes(xintercept=0)) +
#     xlab(TeX("$w^{MRP}_i$")) +
#     ylab(TeX("$w^{CAL}_i$"))
#   return(plt)
# }


# alex_weight_scatterplot <- make_weight_scatterplot(alex)
# lax_weight_scatterplot <- make_weight_scatterplot(lax)


analysis_list <- alex
grid.arrange(
  ggplot(analysis_list$basic_data$basic_metrics$weight_df) +
    geom_histogram(aes(x=w_mrpaw), bins=100) +
    geom_vline(aes(xintercept=0)) +
    xlab(TeX("MrPlew weights $w^{MRP}_i$")) +
    xlim(-6, 20)
  ,
  ggplot(analysis_list$basic_data$basic_metrics$weight_df) +
    geom_histogram(aes(x=w_raking * length(w_raking)), bins=100) +
    geom_vline(aes(xintercept=0)) +
    xlab(TeX("Raking weights $w^{WGT}_i$")) +
    geom_text(aes(x=10, y=1000,
    label="Note: There are a \nfew outlier\nweights >> 20")) +
    xlim(-6, 20)
  , ncol=2
)