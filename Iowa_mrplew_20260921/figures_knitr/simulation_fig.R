

pred_df <- sim_env$pred_df
x <- sim_env$x
ylab_text <- sprintf("x = %0.1f", x)
print(
ggplot(pred_df) +
    geom_line(aes(x=delta, y=yhat_glm, color="Logistic regression")) +
    geom_line(aes(x=delta, y=yhat_ols, color="Linear regression")) +
    ylab(TeX("$\\hat{y}(\\delta)$")) +
    xlab(TeX("$\\delta")) +
    theme(legend.title = element_blank()) +
    ggtitle(sprintf("Predictions at x = %0.1f", sim_env$x))
)

# grid.arrange(
#   ,
#   ggplot(pred_df) +
#     geom_line(aes(x=delta, y=thetahat, color="Logistic regression")) +
#     geom_line(aes(x=delta, y=betahat, color="Linear regression")) +
#     ggtitle("Estimated coefficients")
#   , ncol=2
# )