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
source(file.path(base_dir, "R_scripts/logistic_otto_sim/generate_shift_plots.R"))












#####################################
# Reproduce in generality

source(file.path(base_dir, "R_scripts/logistic_otto_sim/generate_histogram_plots.R"))


##############################
# Make the sequence of Otto plots 
mrp_df <- logistic_env$mrp_df
#glimpse(mrp_df)

otto_plots <- list()

# For some reason piping doesn't work
base_plot <- 
  get_base_plot(mrp_df, "mrp_orig") +
  xlab("MrP point estimate") +
  ylab("")
otto_plots$mrp <- base_plot %>%
  append_result_panel("mrp_pert", "boot", hist=FALSE) %>%
  append_result_panel("mrp_ij", "ij", hist=FALSE) %>%
  append_result_panel("mrp_otto", "otto", hist=TRUE)
otto_plots$mrp


# For some reason piping doesn't work
base_plot <- 
  get_base_plot(mrp_df, "mrp_var_orig") +
  xlab("MrP variance estimate") +
  ylab("")
otto_plots$mrp_var <- base_plot %>%
  append_result_panel("mrp_var_pert", "boot", hist=FALSE) %>%
  append_result_panel("mrp_var_ij", "ij", hist=FALSE) %>%
  append_result_panel("mrp_var_otto", "otto", hist=TRUE)
otto_plots$mrp_var


base_plot <- 
  get_base_plot(diag_df, "mrp_change_orig") +
  xlab("Covariate shift diagnostic") +
  ylab("")
otto_plots$diag1 <- base_plot %>%
  append_result_panel("mrp_change_true", "boot", hist=FALSE) %>%
  append_result_panel("mrp_change_ij", "ij", hist=FALSE) %>%
  append_result_panel("mrp_change_otto", "otto", hist=TRUE)
  
otto_plots$diag1



