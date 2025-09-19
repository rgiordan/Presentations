# You can optinally use DefineMacro here to set latex macros to use R quantities
# in the text.


# Define LaTeX macros that will let us automatically refer
# to simulation and model parameters.
DefineMacro <- function(macro_name, value, digits=3) {
  #sprintf_code <- paste("%0.", digits, "f", sep="")
  value_string <- format(value, big.mark=",", digits=digits, scientific=FALSE)
  cat("\\newcommand{\\", macro_name, "}{", value_string, "}\n", sep="")
}

basic_metrics <- basic_data$basic_metrics
DefineMacro("AlexNSur", basic_metrics$n_survey)
DefineMacro("AlexNTar", basic_metrics$n_target)
DefineMacro("AlexSurmean", basic_metrics$survey_mean, digits=3)
DefineMacro("AlexMrp", basic_metrics$mrp, digits=3)
DefineMacro("AlexMrpSD", basic_metrics$mrp_sd, digits=3)
DefineMacro("AlexRaking", basic_metrics$raking, digits=3)


DefineMacro("RefitTimeHours", as.numeric(refit_data$total_refit_time, units = "hours"), digits=1)
DefineMacro("MrPawTimeSecs", as.numeric(refit_data$mean_mrpaw_time, units = "secs"), digits=1)
