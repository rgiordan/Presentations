# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/Austin_mrplew_20250926"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))

source("figures_knitr/predictions_plot.R")
source("figures_knitr/imbalance_primary.R")
source("figures_knitr/imbalance_interaction.R")

#source(file.path(paper_directory, "figures_knitr/weights_plot.R"), print.eval=TRUE)

source("figures_knitr/predictions_plot.R")
alex_refit_plots$base_plot
alex_refit_plots$plt1



sim_data <- alex$sim_data
sim_data$col_pert
sim_data$predictions_df %>%
  ggplot() +
  geom_line(aes(x=delta, y=raking_diff_continuous, color="raking")) +
  geom_line(aes(x=delta, y=raking_diff, color="raking bin")) +
  geom_line(aes(x=delta, y=pststrt_diff, color="poststrat")) +
  geom_line(aes(x=delta, y=mrp_pred_continuous, color="mrp"))
  