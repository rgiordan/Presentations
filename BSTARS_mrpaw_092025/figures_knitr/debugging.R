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
source(file.path(paper_directory, "figures_knitr/weights_plot.R"), print.eval=TRUE)

head(basic_metrics$weight_df)

plt <- 
  ggplot(basic_metrics$weight_df) +
  geom_point(aes(x=w_mrpaw, y=w_raking * length(w_raking)), alpha=0.2) +
  geom_abline() +
  geom_vline(aes(xintercept=0)) +
  xlab(TeX("$w^{MRP}_i$")) +
  ylab(TeX("$w^{CAL}_i$"))

plt

as.numeric(refit_data$mean_mrpaw_time, units = "secs")

