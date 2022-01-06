# Use this script to debug and edit the knit graphs without re-compiling in latex.

#base_dir <- "/home/rgiordan/Documents/git_repos/AdversarialInfluenceWorkbench"
base_dir <- "/home/rgiordan/Documents/git_repos/Presentations"

paper_directory <- file.path(base_dir, "JobTalk_2021")

knitr_debug <- FALSE # Set to true to see error output
cash_cache <- FALSE
ohie_cache <- FALSE
microcredit_cache <- FALSE

setwd(paper_directory)
source(file.path(paper_directory, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))






# source(file.path(paper_directory,
#                  "figures_knitr/microcredit/refit_line.R"),
#        echo=knitr_debug, print.eval=TRUE)
# 
# print(plot2)


source(file.path(paper_directory,
                 "figures_knitr/microcredit/microcredit_profit_results_table.R"),
       echo=knitr_debug, print.eval=TRUE)


# source(file.path(paper_directory,
#                  "figures_knitr/cash_transfers/cash_transfers_results_table.R"),
#        echo=knitr_debug, print.eval=TRUE)



# 
# source(file.path(paper_directory,
#                  "figures_knitr/OHIE/OHIE-table_results.R"),
#        echo=knitr_debug, print.eval=TRUE)
# 
