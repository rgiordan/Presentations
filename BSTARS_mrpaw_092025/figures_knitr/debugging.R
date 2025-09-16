# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/BSTARS_mrpaw_092025"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))
#source(file.path(paper_directory, "figures_knitr/predictions_plot.R"), print.eval=TRUE)
#source(file.path(paper_directory, "figures_knitr/imbalance_primary.R"), print.eval=TRUE)
#source(file.path(paper_directory, "figures_knitr/imbalance_interaction.R"), print.eval=TRUE)






clean_sim_method <- function(method) {
  case_when(
    method == "mrp" ~ "MRP",
    method == "raking" ~ "Raking",
    method == "Truth" ~ "Target",
    TRUE ~ "UNKNOWN"
  )
}

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

pred_plot_1 <- 
  predictions_band_df %>%
    ggplot(aes(x=delta)) +
    geom_line(aes(y=value, x=delta, color=method, group=name), 
              data=prediction_constants_df, lwd=2) +
    xlab(TeX("$\\delta$")) +
    ylab(sprintf(
      "Change in target population mean\n(Recall MrP = %0.2f)", basic_data$basic_metrics$mrp)) +
    scale_color_manual(values = method_colors) +
    scale_fill_manual(values = method_colors) +
    guides(fill="none") + labs(color="Method")
  

pred_plot_2 <- 
  pred_plot_1 +
  geom_ribbon(alpha=0.2, aes(ymin=q10, ymax=q90, fill=method, group=name))

delta_df <- refit_data$delta_df
pred_plot_3 <- 
  pred_plot_1 +
  geom_line(aes(x=delta, y=mrp_diff_refit, color="MRP", linetype="Refit"), lwd=2, data=delta_df) +
  geom_line(aes(x=delta, y=pred_mrp_diff, color="MRP", linetype="Prediction"), lwd=2, data=delta_df) +
  geom_line(aes(x=delta, y=raking_diff, color="Raking"), data=delta_df, lwd=2) + 
  labs(linetype="Prediction or Posterior")
pred_plot_3

