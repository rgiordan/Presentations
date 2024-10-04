# Use this script to debug and edit the knit graphs without re-compiling in latex.

#git_repo_loc <- system("git rev-parse --show-toplevel", intern=TRUE)
base_dir <- "/home/rgiordan/Documents/git_repos/InfinitesimalJackknifeWorkbench/writing/bayes"

knitr_debug <- FALSE # Set to true to see error output
simple_cache <- FALSE # Set to true to cache knitr output for this analysis.
single_column <- FALSE
setwd(base_dir)
source(file.path(base_dir, "R_scripts/initialize.R"))


source("R_scripts/ARM/load_data.R")
source("R_scripts/ARM/load_pilots_data.R")
source("R_scripts/BPA/load_data.R")
source("R_scripts/election/load_data.R")
source("R_scripts/poisson_sim/load_data.R")
source("R_scripts/singular_example/load_data.R")

source("R_scripts/ARM/define_pilots_macros.R")


source("R_scripts/ARM/arm_pilots_interval_comparison_graph.R", print.eval=TRUE)
#source("R_scripts/ARM/arm_pilots_se_comparison_graph.R", print.eval=TRUE)
