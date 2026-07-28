sim_env <- LoadIntoEnvironment(
    file.path(data_path, "noise_grid.Rdata"))

sim_viz_env <- LoadIntoEnvironment(
    file.path(data_path, "visualization.Rdata"))

cash_env <- LoadIntoEnvironment(
    file.path(data_path, "cash_transfers_results.Rdata"))

ohie_env <- LoadIntoEnvironment(
    file.path(data_path, "OHIE_results.Rdata"))

microcredit_env <- LoadIntoEnvironment(
    file.path(data_path, "microcredit_results.Rdata"))

microcredit_refit_env <- LoadIntoEnvironment(
    file.path(data_path, "microcredit_fit_paths.Rdata"))

coreset_env <- LoadIntoEnvironment(
    file.path(data_path, "microcredit_coreset_data.Rdata"))


# Define coreset plots
source(file.path(paper_directory, "figures_knitr", "coresets/refits.R"))
