# This defines but does not print three refit plots

clean_sim_method <- function(method) {
  case_when(
    method == "mrp" ~ "MRP",
    method == "raking" ~ "Raking",
    method == "Truth" ~ "Target",
    TRUE ~ "UNKNOWN"
  )
}

lwd_val <- 0.6

method_levels <- c("MRP", "Raking", "Target")
prediction_constants_df <-
  sim_data$prediction_constants_df %>%
  mutate(method=clean_sim_method(method)) %>%
  mutate(method=factor(method, levels=method_levels))


predictions_band_df <-
  sim_data$predictions_band_df %>%
  mutate(method=clean_sim_method(method)) %>%
  mutate(method=factor(method, levels=method_levels))

predictions_band_df$method

method_colors <- c(
  "MRP" = "#F8766D",
  "Raking" = "#00BA38",
  "Target" = "#619CFF"
)


base_plot <-
  predictions_band_df %>%
    ggplot(aes(x=delta)) +
    xlab(TeX("$\\delta$")) +
    ylab(sprintf(
      "Change in target population mean\n(Recall MrP = %0.2f)",
      basic_data$basic_metrics$mrp)) +
    scale_color_manual(values = method_colors) +
    scale_fill_manual(values = method_colors) +
    guides(fill="none") + labs(color="Method") +
    ylim(0, 0.062)


pred_plot_1 <-
    base_plot +
    geom_line(aes(y=value, x=delta, color=method, group=name),
              data=prediction_constants_df, lwd=lwd_val)


pred_plot_2 <-
  pred_plot_1 +
  geom_ribbon(alpha=0.2, aes(ymin=q10, ymax=q90, fill=method, group=name))

method_linetypes <- c(
  "Mean pred." = "solid",
  "Binary pred." = "dotted",
  "Refit" = "dashed"
)


delta_df <- refit_data$delta_df
pred_plot_3 <-
  base_plot +
  geom_ribbon(alpha=0.2, aes(ymin=q10, ymax=q90, fill=method, group=name)) +
  geom_line(aes(y=value, x=delta, color=method, group=name, linetype="Mean pred."),
          data=prediction_constants_df, lwd=lwd_val) +
  geom_line(aes(x=delta, y=pred_mrp_diff, color="MRP", linetype="Binary pred."),
            lwd=lwd_val, data=delta_df) +
  geom_line(aes(x=delta, y=raking_diff, color="Raking", linetype="Binary pred."),
            data=delta_df, lwd=lwd_val) +
  labs(linetype="") +
  guides(colour = guide_legend(order = 1),
         linteype = guide_legend(order = 2)) +
  scale_linetype_manual(values=method_linetypes)

pred_plot_4 <-
    pred_plot_3 +
  geom_line(aes(x=delta, y=mrp_diff_refit, color="MRP", linetype="Refit"),
            lwd=lwd_val, data=delta_df)
