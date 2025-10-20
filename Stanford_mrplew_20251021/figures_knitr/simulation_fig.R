

pred_df <- sim_env$pred_df

print(
ggplot(pred_df) +
    geom_line(aes(x=delta, y=yhat_glm, color="Logistic regression")) +
    geom_line(aes(x=delta, y=yhat_ols, color="Linear regression")) +
    ylab(TeX("$\\hat{y}(\\delta)$  for $x = 0.5$")) +
    xlab(TeX("$\\delta")) +
    theme(legend.title = element_blank())
)

# grid.arrange(
#   ,
#   ggplot(pred_df) +
#     geom_line(aes(x=delta, y=thetahat, color="Logistic regression")) +
#     geom_line(aes(x=delta, y=betahat, color="Linear regression")) +
#     ggtitle("Estimated coefficients")
#   , ncol=2
# )