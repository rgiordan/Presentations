# Load and process results produced by model_script.R for the ARM Pilots data.

library(tidyverse)
library(rstan)
library(rstansensitivity)
library(gridExtra)

library(bayesijlib)
library(rstanarmijlib)

rstan_options(auto_write=TRUE)

# If TRUE do not run all the bootstraps and do not save.
repo_dir <- "/home/rgiordan/Documents/git_repos/InfinitesimalJackknifeWorkbench/"
base_dir <- file.path(repo_dir, "src/bayes")
stan_examples_dir <- file.path(base_dir, "example-models")
output_dir <- file.path(base_dir, "rstanarm/cluster/output")
writing_dir <- file.path(repo_dir, "writing/bayes/")


model_list_filename <- "rstanarm_ij_model_list.json"
model_list_file <- file(file.path(base_dir, "rstanarm/configs/", model_list_filename), "rb")
model_list <- jsonlite::fromJSON(model_list_file, simplifyDataFrame=FALSE)
close(model_list_file)

output_filename <- sprintf("compiled_results_%s.Rdata", "1116")
load(file=file.path(output_dir, output_filename))

# Data that will be saved for use in the paper.
save_list <- list()

####################################
# Model description dataframe

GetModelDFRow <- function(i) {
    model_config <- model_list[[i]] %>% SetConfigDefaults()
    data.frame(model_index=i,
               num_exchangeable_obs=model_config$num_exchangeable_obs,
               rstan_fun=model_config$rstan_fun,
               family=model_config$family,
               model_name=model_config$model_name,
               dataset=model_config$dataset,
               num_samples=model_config$num_samples,
               num_boots=model_config$num_boots,
               independent=model_config$exchangeable_col == "",
               stringsAsFactors=FALSE)
}

model_df <-
  do.call(bind_rows, lapply(1:length(model_list), GetModelDFRow))

pilot_index <- filter(model_df, model_name == "pilots") %>% pull(model_index)
stopifnot(length(pilot_index) == 1)

# Load the original data
model_config <- model_list[[pilot_index]]
df <- LoadRstanarmDataframe(model_config, stan_examples_dir)

# Load the complete fit results for all methods
res_list <- LoadModelResults(model_config, file_suffix)
num_exch_obs <- ncol(res_list$base_results$mcmc_results$lp_mat)


rstan_fit <- res_list$base_results$mcmc_results$rstan_fit
lme4_fit <- res_list$lme4_results$lme4_fit
# lme4_boot <- TidyLME4Bootstrap(res_list$lme4_results$boot_results) # slow

# I want to examine some custom parameters, so we need to
# recompute the IJ, bayes, and bootstrap results.
ModifyDrawsMat <- function(draws_mat) {
  # Rename columns and create some extra parameters of interest
  param_names <- c(
    "(Intercept)"="offset",
    "log_sigma"="log_resid",
    "log_Sigma[scenario_id:(Intercept),(Intercept)]"="log_scenario",
    "log_Sigma[group_id:(Intercept),(Intercept)]"="log_group"
  )
  draws_df <-
    draws_mat %>%
    as.data.frame() %>%
    select(!!names(param_names))
  names(draws_df) <- param_names[names(draws_df)]
  draws_df <-
    draws_df %>%
    mutate(log_rs_diff = log_resid - log_scenario,
           log_rg_diff = log_resid - log_group,
           log_sg_diff = log_scenario - log_group)

  draws_mat <- as.matrix(draws_df)
  return(draws_mat)
}

draws_mat <- ModifyDrawsMat(res_list$base_results$mcmc_results$draws_mat)

lp_mat <- res_list$base_results$mcmc_results$lp_mat

ij_cov <- ComputeIJCovariance(lp_mat, draws_mat)
bayes_cov <- cov(draws_mat, draws_mat)

se_results <- bayesijlib::ComputeIJStandardErrors(lp_mat, draws_mat, num_blocks=50, num_draws=200)
se_results$bayes_cov_se
se_results$ij_cov_se

# Because we're only looking at differences, we can compute the bootstrap variability
# of the means of the differences using the differences of the means (whew)
colnames(res_list$boot_results$boot_means)
boot_means <- ModifyDrawsMat(res_list$boot_results$boot_means)
boot_cov <- num_exch_obs * cov(boot_means, boot_means)
boot_cov_se <- num_exch_obs * GetCovarianceMatrixSE(
  boot_means, boot_means, correlated_samples=FALSE)
colnames(boot_cov_se) <- colnames(boot_cov)
rownames(boot_cov_se) <- rownames(boot_cov)


num_exch_obs <- res_list$base_results$rstanarm_ij_config$num_exchangeable_obs
ij_df <- TidyCovarianceFrame(ij_cov, se_results$ij_cov_se, "ij")
bayes_df <- TidyCovarianceFrame(
  num_exch_obs * bayes_cov, num_exch_obs * se_results$bayes_cov_se, "bayes")
boot_df <- TidyCovarianceFrame(boot_cov, boot_cov_se, "bootstrap")
join_cols <- c("row_variable", "column_variable",
               "row_variable_name", "column_variable_name",
               "params")
this_comb_df <-
  ij_df %>%
  inner_join(bayes_df, by=join_cols) %>%
  inner_join(boot_df, by=join_cols) %>%
  filter(row_variable == column_variable)


# Save some quantities for the paper
save_list$num_exch_obs <- num_exch_obs
save_list$num_boots <- nrow(res_list$boot_results$boot_means)
save_list$mcmc_time <- res_list$base_results$mcmc_results$fit_time
save_list$num_pars <- ncol(draws_mat)
save_list$boot_time <- res_list$boot_results$boot_time
save_list$num_groups <- df$group_id %>% unique() %>% length()
save_list$num_scenarios <- df$scenario_id %>% unique() %>% length()
save_list$lme4_version <- packageVersion("lme4")

cat(save_list$num_groups, " groups (conditions).\n", 
    save_list$num_scenarios, " scenarios (airports).\n", sep="")
  
# Results
tidy(rstan_fit)
rstan_fit

tidy(lme4_fit)



# The standard errors of the different covariances, to be joined with the
# point estimates of the different covariances
plot_se_df <-
  this_comb_df %>%
  select(row_variable,
         bayes_se, ij_se, bootstrap_se) %>%
  pivot_longer(cols=-row_variable, names_to="method", values_to="cov_se") %>%
  mutate(method=str_replace(method, "_se$", ""))

mean_df <- as.data.frame(draws_mat) %>%
  mutate(draw=1:n()) %>%
  pivot_longer(-draw, names_to="param") %>%
  group_by(param) %>%
  summarize(post_mean=mean(value), post_sd=sd(value))



plot_df <- 
  this_comb_df %>%
  select(row_variable,
         bayes_cov, ij_cov, bootstrap_cov) %>%
  pivot_longer(cols=-row_variable, names_to="method", values_to="cov") %>%
  mutate(method=str_replace(method, "_cov$", "")) %>%
  inner_join(plot_se_df, by=c("row_variable", "method")) %>%
  inner_join(mean_df %>% rename(row_variable=param), by="row_variable") %>%
  mutate(se=sqrt(cov / num_exch_obs), z=abs(post_mean) / se, sig=z > 2)

if (FALSE) {
  View(plot_df)
}

unique(plot_df$row_variable)

save_list$plot_df <- plot_df

if (FALSE) {
  # The reason this is nice is you could come to different conclusions about
  # the analysis
  
  View(plot_df)

  ggplot(plot_df) +
    geom_bar(aes(y=cov, x=method, fill=method),
             stat="identity", position="dodge") +
    geom_errorbar(aes(x=method, ymin=cov - 2 * cov_se, ymax=cov + 2 * cov_se),
                  stat="identity", position=position_dodge(0.9), width=0.2) +
    scale_y_log10() +
    facet_grid(row_variable ~ ., scales="free")
}



##########################################################################3
# Save a file with all the detailed results for the ARM pilots model

paper_filename <- sprintf("arm_pilots_results_original_data_121222.Rdata")
save(save_list,
     file=file.path(writing_dir, "data/ARM", paper_filename))

