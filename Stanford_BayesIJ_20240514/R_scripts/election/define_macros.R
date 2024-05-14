DefineMacro("electionNumObs", election_env$num_exch_obs, digits=0)
DefineMacro("electionNumPollsters", election_env$num_pollsters, digits=0)

DefineMacro("electionNumMCMCDraws", election_env$num_mcmc_draws, digits=0)
DefineMacro("electionNumBoots", election_env$num_boots, digits=0)
DefineMacro("electionBootNumMCMCDraws",
            election_env$num_boot_mcmc_draws, digits=0)

# We ran the base chains in parallel, but the bootstrap chains serially.
# We want to compare the total compute time.
election_mcmc_time <-
  election_env$opt$num_mcmc_chains *
  lubridate::time_length(election_env$sampling_time, unit="min")
DefineMacro("electionMCMCHours",
            election_mcmc_time / 60,
            digits=2)
DefineMacro("electionBootHours",
            sum(election_env$boot_times) / 60,
            digits=2)
DefineMacro("electionBootOverMCMC",
            sum(election_env$boot_times) / election_mcmc_time,
            digits=0)

DefineMacro("electionWorstState", election_env$worst_state, digits=0)
DefineMacro("electionBestState", election_env$best_state, digits=0)
