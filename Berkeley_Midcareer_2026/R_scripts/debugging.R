# Use this script to debug and edit the knit graphs without re-compiling in latex.

git_repo_loc <- system("git rev-parse --show-toplevel", intern=TRUE)
base_dir <- file.path(git_repo_loc, "Berkeley_Midcareer_2026")

knitr_debug <- FALSE # Set to true to see error output
simple_cache <- FALSE # Set to true to cache knitr output for this analysis.
single_column <- FALSE
setwd(base_dir)
source(file.path(base_dir, "R_scripts/initialize.R"))


source(file.path(base_dir, "R_scripts/logistic_otto_sim/load_data.R"))
source(file.path(base_dir, "R_scripts/logistic_otto_sim/generate_histogram_plots.R"))


