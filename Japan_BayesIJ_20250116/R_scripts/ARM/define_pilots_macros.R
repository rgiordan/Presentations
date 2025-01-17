
# For Pilots
pilot_env <- arm_pilots_env$save_list
DefineMacro(
    "armPilotMCMCTimeSecs",
    min(as.numeric(pilot_env$mcmc_time, units="secs")), digits=2)
DefineMacro(
    "armPilotBootTimeMins",
    min(as.numeric(pilot_env$boot_time, units="mins")), digits=2)
DefineMacro(
    "armPilotNumObs", pilot_env$num_exch_obs, digits=0)
DefineMacro(
    "armPilotNumParams", pilot_env$num_pars, digits=0)
DefineMacro(
    "armPilotNumGroups", pilot_env$num_groups, digits=0)
DefineMacro(
    "armPilotNumScenarios", pilot_env$num_scenarios, digits=0)
DefineMacro(
    "armPilotNumBoots", pilot_env$num_boots, digits=0)
DefineMacro(
    "lmerVersion", pilot_env$lme4_version, digits=0)
