alexander <- LoadIntoEnvironment(file.path(data_path, "alexander_paper_data.Rdata"))
laxphilips <- LoadIntoEnvironment(file.path(data_path, "laxphilips_CA_paper_data.Rdata"))
stories <- LoadIntoEnvironment(file.path(data_path, "stories_paper_data.Rdata"))

alexander$name <- "Name Change"
laxphilips$name <- "Same-Sex Marriage"
stories$name <- "Election Forecasting"

alexander$simple_name <- "alexander"
laxphilips$simple_name <- "laxphilips"
stories$simple_name <- "stories"


# Load individual refit analyses
alexander_col_pert_clean <- "decade_married_rk2009educ_groupBA"
alexander_refit <- LoadIntoEnvironment(
    file.path(data_path, sprintf("alexander_refit_analysis_%s.Rdata",
                                 alexander_col_pert_clean)))

laxphilips_col_pert_clean <- "educat2"
laxphilips_refit <- LoadIntoEnvironment(
    file.path(data_path, sprintf("laxphilips_CA_refit_analysis_%s.Rdata",
                                 laxphilips_col_pert_clean)))


# Partial pooling
laxphilips_CA_pooling <-  LoadIntoEnvironment(
    file.path(data_path, "laxphilips_CA_partial_pooling.Rdata"))

laxphilips_MO_pooling <-  LoadIntoEnvironment(
    file.path(data_path, "laxphilips_MO_partial_pooling.Rdata"))

alexander_pooling <- LoadIntoEnvironment(
    file.path(data_path, "alexander_partial_pooling.Rdata"))


# Importance sampling
laxphilips_importance_sampling <-  LoadIntoEnvironment(
    file.path(data_path, 
    "laxphilips_CA_perteducat2_importance_sampling.Rdata"))


# Alexander alternative models
alexander_model_list <- list()
for (model in c( "interaction",
                 "single_interaction",
                 "fixedeffects",
                 "fixedeffects_interaction")) {
    model_balance <- 
        LoadIntoEnvironment(
            file.path(data_path, 
            sprintf("alexander_%s_balance.Rdata", model)))
    model_balance$simple_name <- "alexander"  # For cleaning regressor namesgit
    alexander_model_list[[model]] <- model_balance
}





# Define plot objects and functions
source(GetScriptLoc("balance_plots.R"))
source(GetScriptLoc("refit_plots.R"))
source(GetScriptLoc("pooling_plots_laxphilips.R"))
source(GetScriptLoc("bootstrap_plot.R"))
source(GetScriptLoc("weights_plot.R"))
source(GetScriptLoc("example_plots.R"))
