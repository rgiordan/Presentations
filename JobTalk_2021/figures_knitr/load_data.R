# Data note:
# These load data from the old local paper repository, and so these
# files are likely no longer available.  I have checked into github
# copies of the needed Rdata files in the JSM_AMIP_2026 subdirectory
# of the presentations repo.  If you ever need to regenerate the
# figures, consider looking there for data files instead of the old repo.


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
