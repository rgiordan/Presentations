# This file would typically load experimental results and save a single
# Rdata file for each experiment containing the minimal amount of data needed
# to produce the paper.
#
# The reasons for this preprocessing script are that
# - Having a lot of processing / loading of large files in knitr slows down
#   the paper rendering, whichs is really annoying
# - It is much easier to maintain and debug an R script than knitr, and so
#   the complexity and substance of knitr should be kept to a minimum.

git_repo_dir <- "/home/rgiordan/Documents/git_repos/Presentations/"
data_dir <- file.path(git_repo_dir, "TamaraGroupKnitr_20210408/R_scripts/data")
x <- rnorm(1000)
y <- rnorm(1000)
save(x, y, file=file.path(data_dir, "experiment_data.Rdata"))
