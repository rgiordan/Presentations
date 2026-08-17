
survey_df <- logistic_env$survey_df
poststrat_df <- logistic_env$poststrat_df

col <- "X2"
shift_plot0 <- 
ggplot() +
  geom_histogram(aes(x=.data[[col]], y=..density../ max(..density..), fill="survey"), 
                 data=survey_df, alpha=0.5) +
  geom_histogram(aes(x=.data[[col]], y=..density.. / max(..density..), fill="poststrat"), 
                 data=poststrat_df, alpha=0.5) +
  geom_smooth(aes(x=.data[[col]], y=yhat0, linetype="Correct specification"), data=survey_df) +
  geom_smooth(aes(x=.data[[col]], y=yhat1, linetype="Misspecification"), data=survey_df) +
  xlab(col) + ylab("") +
  scale_linetype(name="Model") +
  scale_fill_discrete(name="Dataset",
                      breaks=c("poststrat", "survey"),
                      labels=c("Target population",
                               "Survey population\n(used for estimation)"))

shift_plot1 <- 
  shift_plot0 +
  annotate("rect", xmin = 0.878, xmax = Inf, ymin = 0, ymax = 0.05, 
         fill = "purple", alpha = 0.5)