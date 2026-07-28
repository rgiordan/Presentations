
PlotRefitDf <- function(refit_df) {
  ggplot(refit_df, 
         aes(x=regs, color=fit)) +
    geom_point(aes(y=Estimate), position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin=Estimate - 2 * se, ymax=Estimate + 2 * se),
                  position=position_dodge(width = 0.5)) +
    xlab("Regressor") + ylab("OLS estimate") +
    scale_color_discrete(name="")
}

greedy_coreset_plot <- PlotRefitDf(coreset_env$greedy_drop$refit_df) + 
  ggtitle(sprintf(paste0(
    "Effect of using the influence function to drop the\n ",
    "%0.0f%% least influential datapoints"),
    100 * coreset_env$greedy_drop$alpha))


influential_coreset_plot <- PlotRefitDf(coreset_env$influential_keep$refits_df) + 
  ggtitle(
    sprintf(paste0(
      "Effect of using the influence function to keep the\n ",
      "%0.0f%% most influential datapoints"),
      100 * coreset_env$influential_keep$alpha)
  )

gd_plot <- ggplot(coreset_env$gradient_descent$descent_df) +
  geom_point(aes(x=100 * (1 - data_prop), y=par)) +
  xlab("% of points dropped") +
  ylab("OLS estimate of treatment effect") +
  xlim(0, 100)

gd_alpha <- min(coreset_env$gradient_descent$descent_df$data_prop)
gd_refit_plot <- 
  PlotRefitDf(coreset_env$influential_keep$refits_df) + 
  ggtitle(
    sprintf(paste0(
      "Effect of using the gradient descent to keep the\n ",
      "%0.0f%% most influential datapoints"),
      100 * gd_alpha)
  )
