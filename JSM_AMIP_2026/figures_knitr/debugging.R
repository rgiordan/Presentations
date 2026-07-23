# Use this script to debug and edit the knit graphs without re-compiling in latex.

#base_dir <- "/home/rgiordan/Documents/git_repos/AdversarialInfluenceWorkbench"
base_dir <- "/home/rgiordan/Documents/git_repos/Presentations"

paper_directory <- file.path(base_dir, "JSM_AMIP_2026")

knitr_debug <- FALSE # Set to true to see error output
cash_cache <- FALSE
ohie_cache <- FALSE
microcredit_cache <- FALSE

setwd(paper_directory)
source(file.path(paper_directory, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))

