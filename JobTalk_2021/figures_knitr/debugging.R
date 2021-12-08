# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/AdversarialInfluenceWorkbench"
#base_dir <- "/Users/rachaelmeager/AdversarialInfluenceWorkbench"

paper_directory <- file.path(base_dir, "writing/output/")

knitr_debug <- FALSE # Set to true to see error output
cash_cache <- FALSE
ohie_cache <- FALSE
microcredit_cache <- FALSE

setwd(paper_directory)
source(file.path(paper_directory, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))

# Now you can source individual files from the paper to see how they look
# without re-compiling.

# source(file.path(paper_directory,
#                  "figures_knitr/OHIE/OHIE-table_results.R"),
#        echo=knitr_debug, print.eval=TRUE)


# source(file.path(paper_directory,
#                  "figures_knitr/microcredit_mixture/microcredit_mix_refit_table.R"),
#        echo=knitr_debug, print.eval=TRUE)


source(file.path(paper_directory,
                 "figures_knitr/microcredit_mixture/microcredit_mix_sd_refit_table.R"),
       echo=knitr_debug, print.eval=TRUE)


