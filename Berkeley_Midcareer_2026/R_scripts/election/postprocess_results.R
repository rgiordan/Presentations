library(tidyverse)
library(rstansensitivity)
library(gridExtra)

library(bayesijlib)
library(rstanarmijlib)
library(lubridate)

run_date <- ymd("2016-11-08")

git_repo_dir <- "/home/rgiordan/Documents/git_repos/InfinitesimalJackknifeWorkbench/"
model_dir <- file.path(git_repo_dir, "src/bayes/example-models/us-potus-model")
writing_dir <- file.path(git_repo_dir, "writing/bayes/data/election")


result_filename <- file.path(model_dir, sprintf("potus_2016_combined_results.Rdata"))
output_filename <- file.path(writing_dir, sprintf("potus_2016_results_for_paper.Rdata"))

load(result_filename)

num_mcmc_draws <- nrow(par_draws)
num_exch_obs <- ncol(lp_draws)
num_boots <- nrow(boot_draws)

wide_df <-
  summary_df %>%
  select(-is_natl_vote) %>%
  pivot_longer(cols=-par) %>%
  separate(name, extra="merge", into=c("method", "metric")) %>%
  pivot_wider(id_cols=c(par, method), names_from=metric, values_from=value) 

state_names <- unique(wide_df$par) %>% setdiff("natl_vote")
par_levels <- c("natl_vote", state_names)
par_labels <- c("National Vote", state_names)

wide_df$par_label <- factor(wide_df$par, levels=par_levels, labels=par_labels)

###############################
# misc numbers

num_pollsters <- length(unique(poll_df$pollster))

# What state had the largest relative discrepancy?
error_ranks <- abs(log(summary_df$ij_cov) - log(summary_df$boot_cov)) %>% sort()
error_ranks <- error_ranks[names(error_ranks) != "natl_vote"]
best_state <- names(error_ranks)[1]
worst_state <- names(error_ranks)[length(error_ranks)]

###############################

save(summary_df,
     wide_df,
     num_mcmc_draws,
     num_exch_obs,
     num_pollsters,
     num_boots,
     opt,
     sampling_time,
     poll_df,
     boot_times,
     num_boot_mcmc_draws,
     best_state,
     worst_state,
     file=output_filename)


