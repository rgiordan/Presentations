# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/ISBA_mrplew_20260627"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))

source(file.path(paper_directory, "figures_knitr/bootstrap_plot.R"))
boot_df
lax_var_bayes_plt
laxphilips$save_list$mrp_sd_rootn
