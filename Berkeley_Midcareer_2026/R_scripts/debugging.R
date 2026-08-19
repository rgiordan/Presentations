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


shift_base_plt + survey_hist + correct_reg

shift_base_plt + survey_hist + correct_reg

shift_base_plt + survey_hist + misspec_reg + correct_reg

shift_base_plt + survey_hist + misspec_reg + correct_reg + x2_zone



source(file.path(base_dir, "R_scripts/logistic_otto_sim/generate_shift_plots.R"))
survey_shifted_hist <-
  geom_histogram(aes(x=.data[[col]], 
                     y=..density.. / max(..density..), 
                     fill="shifted survey"), 
                 data=survey_df %>% filter(X2 > x2_thresh),
                 alpha=1.0)

misspec_shifted_reg <-
  geom_smooth(aes(x=.data[[col]], y=yhat1 + 0.02 * (.data[[col]] - mean(.data[[col]])), 
                  linetype="Misspecification (shifted)"),
              data=survey_df, se = FALSE)

shift_base_plt + survey_hist + poststrat_hist
shift_base_plt + survey_hist + correct_reg
shift_base_plt + survey_hist + misspec_reg + correct_reg
shift_base_plt + survey_shifted_hist + survey_hist + misspec_reg + correct_reg + misspec_shifted_reg
shift_base_plt + survey_shifted_hist + survey_hist + misspec_reg + correct_reg + misspec_shifted_reg
