# sim_env <- LoadIntoEnvironment(
#     file.path(data_path, "simulations", "simulations.Rdata"))
sim_env <- LoadIntoEnvironment(
    file.path(data_path, "simulations", "noise_grid.Rdata"))

sim_viz_env <- LoadIntoEnvironment(
    file.path(data_path, "simulations", "visualization.Rdata"))

cash_env <- LoadIntoEnvironment(
    file.path(data_path, "cash_transfers", "cash_transfers_results.Rdata"))

ohie_env <- LoadIntoEnvironment(
    file.path(data_path, "ohie", "OHIE_results.Rdata"))

microcredit_env <- LoadIntoEnvironment(
    file.path(data_path, "microcredit", "microcredit_results.Rdata"))

microcredit_refit_env <- LoadIntoEnvironment(
    file.path(data_path, "microcredit", "microcredit_fit_paths.Rdata"))


# microcredit_temptation_env <- LoadIntoEnvironment(
#     file.path(data_path, "microcredit", "microcredit_temptation_results.Rdata"))

# mcmix_env <- LoadIntoEnvironment(
#     file.path(data_path, "microcredit_mixture", "microcredit_mixture_results.Rdata"))
