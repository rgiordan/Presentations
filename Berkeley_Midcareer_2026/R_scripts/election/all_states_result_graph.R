ggplot(election_env$summary_df %>% filter(!is_natl_vote, par != "DC")) +
  geom_ellipse(aes(x0=boot_cov, y0=ij_cov,
                   a=2 * boot_se / boot_cov, b=2 * ij_se / ij_cov,
                   angle=0),
               alpha=0.05, color=NA, fill="gray") +
  geom_ellipse(aes(x0=boot_cov, y0=bayes_cov,
                   a=2 * boot_se / boot_cov, b=2 * bayes_se / bayes_cov,
                   angle=0),
               alpha=0.05, color=NA, fill="gray") +
  geom_point(aes(x=boot_cov, y=ij_cov, color="ij")) +
  geom_point(aes(x=boot_cov, y=bayes_cov, color="bayes")) +
  geom_abline(aes(slope=1, intercept=0)) +
  scale_x_log10() + scale_y_log10() +
  expand_limits(x=0.001, y=0.001) +
  expand_limits(x=0.001, y=1) +
  xlab("Bootstrap covariance (log10 scale)") +
  ylab("Bayes and IJ covariance (log10 scale)") +
  GetMethodScale()
  # scale_color_discrete(
  #   name="Method", labels=c("IJ", "Bayes"), breaks=c("ij", "bayes"))
