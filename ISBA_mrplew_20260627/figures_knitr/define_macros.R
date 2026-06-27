# You can optinally use DefineMacro here to set latex macros to use R quantities
# in the text.


# Define LaTeX macros that will let us automatically refer
# to simulation and model parameters.
DefineMacro <- function(macro_name, value, digits=3) {
  value_string <- format(value, big.mark=",", digits=digits, scientific=FALSE)
  cat("\\newcommand{\\", macro_name, "}{", value_string, "}\n", sep="")
}

DefineMacros <- function(analysis_env, prefix) {
  save_list <- analysis_env$save_list
  DefineMacro(paste0(prefix, "NSur"), save_list$n_obs_survey)
  DefineMacro(paste0(prefix, "NTar"), save_list$n_obs_poststrat)

  DefineMacro(paste0(prefix, "Ybar"), save_list$ybar, digits=3)
  DefineMacro(paste0(prefix, "MrpMu"), save_list$mrp, digits=3)
  DefineMacro(paste0(prefix, "RakingMu"), save_list$raking_mrp, digits=3)
  DefineMacro(paste0(prefix, "NumBoots"), save_list$num_boots)
  DefineMacro(paste0(prefix, "MCMCTimeMins"),
              as.numeric(save_list$mcmc_time, units = "mins"),
              digits=1)
  DefineMacro(paste0(prefix, "MrplewTimeSecs"),
              as.numeric(save_list$mrplew_time, units = "secs"),
              digits=1)

  # All these standard deviations are after scaling by sqrt{N_sur}
  DefineMacro(paste0(prefix, "MrPSd"), analysis_env$freq_sd_df$mrplew, digits=3)
  DefineMacro(paste0(prefix, "RakingSd"), analysis_env$freq_sd_df$raking, digits=3)
  DefineMacro(paste0(prefix, "MrPPostSd"), analysis_env$save_list$mrp_sd_rootn, digits=3)
}


DefineMacros(alexander, "Alexander")
DefineMacros(stories, "Stories")
DefineMacros(laxphilips, "Lax")


DefineRefitMacros <- function(refit_env, prefix) {
  DefineMacro(paste0(prefix, "Colpert"), 
              paste0("\\texttt{\\detokenize{",
              refit_env$save_list$col_pert, "}}"))

  n_y <- length(refit_env$y_list)
  prop_changed <- mean(abs(refit_env$y_list[[n_y]] - refit_env$y_list[[1]]))
  DefineMacro(paste0(prefix, "Pctflipped"), 100 * prop_changed, digits=1)

  DefineMacro(paste0(prefix, "Shrink"), refit_env$save_list$shrink, digits=1)

}
# Define the refit macros
DefineRefitMacros(alexander_refit, "Alexander")
DefineRefitMacros(laxphilips_refit, "Lax")

# Also consider saving
# alexander_refit$save_list$delta_max
# alexander_refit$save_list$delta_max_post_sds
# alexander_refit$save_list$shrink

