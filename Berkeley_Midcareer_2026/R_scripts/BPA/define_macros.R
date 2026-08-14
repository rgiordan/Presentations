DefineMacro("batsNumObs", bats_env$num_exch_obs, digits=0)
DefineMacro("batsNumMCMCDraws", bats_env$num_mcmc_draws, digits=0)
DefineMacro("batsNumTimes", max(bats_env$marr_df$recapture_time), digits=0)
DefineMacro("batsMCMCTime",
  as.numeric(bats_env$ij_mcmc_time, units="secs"), digits=1)
DefineMacro("batsBootTime", bats_env$boot_time_sec, digits=0)
DefineMacro("batsBootOverMCMCTime",
  bats_env$boot_time_sec / as.numeric(bats_env$ij_mcmc_time, units="secs"),
  digits=0)
DefineMacro("batsNumSEBlocks", bats_env$num_se_blocks, digits=0)
DefineMacro("batsNumSEDraws", bats_env$num_se_draws, digits=0)
DefineMacro("batsNumBoots", bats_env$num_boots, digits=0)


DefineMacro("batsMeanMeanP", bats_env$mcmc_means["mean_p"], digits=3)
DefineMacro("batsMeanMeanPhi", bats_env$mcmc_means["mean_phi"], digits=3)
DefineMacro("batsMeanLogSigma", bats_env$mcmc_means["log_sigma"], digits=3)

DefineMacro("batsOptMeanP", bats_env$opt_means["mean_p"], digits=3)
DefineMacro("batsOptMeanPhi", bats_env$opt_means["mean_phi"], digits=3)
DefineMacro("batsOptSigma", bats_env$opt_means["sigma"], digits=3)
