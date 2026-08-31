
survey_df <- logistic_env$survey_df
poststrat_df <- logistic_env$poststrat_df




col <- "X2"
x2_thresh <- 0.878

survey_hist <-
  geom_histogram(aes(x=.data[[col]], y=..density../ max(..density..), fill="survey"), 
                 data=survey_df, alpha=1.0)
poststrat_hist <-
  geom_histogram(aes(x=.data[[col]], y=..density.. / max(..density..), fill="poststrat"), 
                 data=poststrat_df, alpha=0.5)

correct_reg <-
  geom_smooth(aes(x=.data[[col]], y=yhat0, linetype="Correct specification"), 
  data=survey_df, se = FALSE)

misspec_reg <-
  geom_smooth(aes(x=.data[[col]], y=yhat1, linetype="Misspecification"),
  data=survey_df, se = FALSE)

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

x2_zone <-
  annotate("rect", xmin = x2_thresh, xmax = Inf, ymin = 0, ymax = 0.05, 
         fill = "purple", alpha = 0.5)

shift_base_plt <-
  ggplot() +
    xlab(col) + ylab("") +
    scale_linetype(name="Regression model") +
    scale_fill_manual(name="Dataset",
                        breaks=c("survey", "shifted survey", "poststrat"),
                        labels=c("Survey population\n(used for estimation)",
                                "Survey population (shifted)",
                                "Target population"),
                        values=c("orange", "salmon", "dark green")) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.25), limits=c(0, 1)) +
    scale_x_continuous(breaks = seq(from = -0.5, to = 2.5, by = 0.5), limits=c(-0.3, 2.4))



# shift_base_plt + survey_hist + poststrat_hist
# shift_base_plt + survey_hist + correct_reg
# shift_base_plt + survey_hist + misspec_reg + correct_reg
# shift_base_plt + survey_shifted_hist + survey_hist + misspec_reg + correct_reg + misspec_shifted_reg
# shift_base_plt + survey_shifted_hist + survey_hist + misspec_reg + correct_reg + misspec_shifted_reg
