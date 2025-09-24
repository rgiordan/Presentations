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

source(file.path(paper_directory, "figures_knitr/weights_plot.R"), print.eval=TRUE)


