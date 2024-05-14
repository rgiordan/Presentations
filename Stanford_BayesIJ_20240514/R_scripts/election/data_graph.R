poll_df <- election_env$poll_df
pred_df <- election_env$pred_df

ggplot() +
  geom_ribbon(aes(x=t, ymin=mean - 2 * sd, ymax=mean + 2 * sd),
              data=pred_df, alpha=0.2) +
  geom_line(aes(x=t, y=mean), data=pred_df) +
  geom_point(aes(x=t, y=p, color=factor(pollster)), data=poll_df) +
  geom_hline(aes(yintercept=0.5)) +
  theme(legend.position="none") +
  xlab("Time (days)") +
  ylab("Democratic share of national vote") +
  ggtitle("Economist presidential election model (2016 data)")
