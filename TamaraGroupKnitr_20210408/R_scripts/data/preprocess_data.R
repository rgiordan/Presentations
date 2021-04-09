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
num_obs <- 1000
x <- rnorm(num_obs)
eps <- rnorm(num_obs)
beta <- 5
y <- eps + 5 * x

table_df <- rbind(
  data.frame(metric="mean", x=mean(x), y=mean(y)),
  data.frame(metric="sd", x=sd(x), y=sd(y)),
  data.frame(metric="max", x=max(x), y=max(y))
)


save(x, eps, num_obs, file=file.path(data_dir, "experiment_one_data.Rdata"))
save(table_df, num_obs, beta, file=file.path(data_dir, "experiment_two_data.Rdata"))
