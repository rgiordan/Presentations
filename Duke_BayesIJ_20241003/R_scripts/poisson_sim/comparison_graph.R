result_comp <- re_sim_env$save_list$result_comp

num_obs_vals <- unique(result_comp$num_obs)
num_obs_lab <- sprintf("N = %d", num_obs_vals)
names(num_obs_lab) <- num_obs_vals

obs_per_re_vals <- unique(result_comp$obs_per_re)
obs_per_re_lab <- sprintf("N / G = %d", obs_per_re_vals)
names(obs_per_re_lab) <- obs_per_re_vals


result_comp %>%
    filter(draw_g, method != "bayes") %>%
    VarianceCompPlot(diff_n, diff_n_se) +
    geom_hline(aes(yintercept=0)) +
    ylab("Difference from simulation") +
    facet_grid(obs_per_re ~ num_obs,
               labeller=labeller(
                   obs_per_re=obs_per_re_lab, num_obs=num_obs_lab))
