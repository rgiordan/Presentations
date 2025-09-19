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

method_colors <- c(
  "MRP" = "#F8766D",
  "Raking" = "#00BA38",
  "Target" = "#619CFF"
)

method_linetypes <- c(
  "Mean pred." = "solid",
  "Prediction" = "dashed",
  "Actual" = "solid"
)

delta_df <- refit_data$delta_df

base_plot <-
  ggplot() +
  xlab(TeX("$\\delta$")) +
  ylab(sprintf(
    "Change in target population mean\n(Recall MrP = %0.2f)",
    basic_data$basic_metrics$mrp)) +
  scale_color_manual(values = method_colors) +
  scale_fill_manual(values = method_colors) +
  guides(fill="none") + labs(color="Method") +
  ylim(0, 0.062) +
  geom_line(aes(y=value, x=delta, color=method, group=name), lwd=lwd_val,
            data=prediction_constants_df %>%
              filter(name == "pststrt_diff")) +
  geom_line(aes(x=delta, y=pred_mrp_diff, color="MRP", linetype="Prediction"),
            lwd=lwd_val, data=delta_df) +
  geom_line(aes(x=delta, y=raking_diff, color="Raking", linetype="Actual"),
            data=delta_df, lwd=lwd_val) +
  labs(linetype="") +
  guides(colour = guide_legend(order = 1),
         linteype = guide_legend(order = 2)) +
  scale_linetype_manual(values=method_linetypes)


plt1 <-
    base_plot +
    geom_line(aes(x=delta, y=mrp_diff_refit, color="MRP", linetype="Actual"),
                lwd=lwd_val, data=delta_df)

