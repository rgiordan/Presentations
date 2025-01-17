

#
# result_comp %>%
#     filter(draw_g, method != "bayes") %>%
#     VarianceCompPlot(diff_n, diff_n_se) +
#     geom_hline(aes(yintercept=0)) +
#     ylab("Difference from simulation") +
#     facet_grid(obs_per_re ~ num_obs,
#                labeller=labeller(
#                    obs_per_re=obs_per_re_lab, num_obs=num_obs_lab))
#

plt <-
  arm_pilots_env$plot_df %>%
    # geom_point(aes(x=method_name, y=post_mean, color=method_name)) +
    VarianceCompPlot(post_mean, se) +
    # geom_errorbar(
    #   aes(x=method_name, ymin=post_mean - 2 * se, ymax=post_mean + 2 * se, color=method_name)) +
    facet_grid(. ~ param_name, scales="free", labeller = "label_parsed") +
    # scale_color_discrete(name="Method") +
    geom_hline(aes(yintercept=0), color="black") +
    #ylab(TeX("$E_{p(\\gamma | X)}\\[g(\\gamma)\\] \\pm 2 \\hat{\\sigma}$"))
    ylab(TeX("Posterior mean $\\pm$ variability estimate"))

print(plt)
