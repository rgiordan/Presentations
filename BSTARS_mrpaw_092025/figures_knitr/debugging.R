# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/BSTARS_mrpaw_092025"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))
source(file.path(paper_directory, "figures_knitr/predictions_plot.R"), print.eval=TRUE)

sim_data$imb_df
