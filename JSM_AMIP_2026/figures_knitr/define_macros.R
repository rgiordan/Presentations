# You can optinally use DefineMacro here to set latex macros to use R quantities
# in the text.


# Define LaTeX macros that will let us automatically refer
# to simulation and model parameters.
DefineMacro <- function(macro_name, value, digits=3) {
  #sprintf_code <- paste("%0.", digits, "f", sep="")
  value_string <- format(value, big.mark=",", digits=digits, scientific=FALSE)
  cat("\\newcommand{\\", macro_name, "}{", value_string, "}\n", sep="")
}


DefineMacro("SimNumObs", sim_env$grid_list$num_obs)
DefineMacro("SimTrueTheta", sim_env$grid_list$theta0, digits=1)

DefineMacro("SimAccNumObs", sim_env$acc_list$num_obs)
DefineMacro("SimAccSigx", sim_env$acc_list$sig_x)
DefineMacro("SimAccSigeps", sim_env$acc_list$sig_eps)
DefineMacro("SimAccPercentMax", 100 * sim_env$acc_list$alpha_max)


mx_analysis_df <- filter(microcredit_refit_env$analysis_df, site == "Mexico")
stopifnot(nrow(mx_analysis_df) == 1)
DefineMacro("MxNoise", mx_analysis_df$noise, digits=4)
DefineMacro("MxBetahat", mx_analysis_df$betahat, digits=3)
DefineMacro("MxNobs", mx_analysis_df$n_obs, digits=0)
