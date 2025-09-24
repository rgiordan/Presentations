


clean_regressors <- function(reg) {
  df <- data.frame(reg=reg) %>%
    mutate(reg_clean=reg) %>%
    mutate(reg_clean=str_replace(reg_clean, "decade_married_rk", "married ")) %>%
    mutate(reg_clean=str_replace(reg_clean, "age_group_rk", "age ")) %>%
    mutate(reg_clean=str_replace(reg_clean, "region_rk", "")) %>%
    mutate(reg_clean=str_replace(reg_clean, "educ_group", "")) %>%
    mutate(reg_clean=str_replace(reg_clean, ":", " x "))
  return(df$reg_clean)
}

clean_method <- function(method) {
  case_when(
    method == "mrp_x" ~ "MRP",
    method == "raking_x" ~ "Raking",
    method == "poststrat_x" ~ "Target",
    TRUE ~ "UNKNOWN"
  )
}

# Alexander data

alex <- list()

alex$col_pert_clean <- "decade_married_rk2009educ_groupBA"
alex$sim_data <- LoadIntoEnvironment(
  file.path(data_path,
  sprintf("alexander_%s_simulations.Rdata", alex$col_pert_clean)))

alex$basic_data <- LoadIntoEnvironment(
    file.path(data_path, "alexander_basic_data.Rdata"))

alex$refit_data <- LoadIntoEnvironment(
  file.path(data_path, sprintf("alexander_%s_refit.Rdata", alex$col_pert_clean)))


alex$basic_metrics <- basic_data$basic_metrics
alex$basic_data$imb_plot_df <-
  basic_metrics$imb_df %>%
  select(reg, interaction, mrp_x, raking_x, poststrat_x) %>%
  pivot_longer(cols=c(mrp_x, raking_x, poststrat_x)) %>%
  mutate(reg_clean=clean_regressors(reg)) %>%
  mutate(name=clean_method(name))

alex$basic_data$pert_col_clean <- clean_regressors(sim_data$col_pert)


# Lax Philips data

lax <- list()

lax$col_pert_clean <- "region_rkwest"
lax$sim_data <- LoadIntoEnvironment(
  file.path(data_path,
  sprintf("laxphilips_%s_simulations.Rdata", lax$col_pert_clean)))

lax$basic_data <- LoadIntoEnvironment(
    file.path(data_path, "laxphilips_basic_data.Rdata"))

lax$refit_data <- LoadIntoEnvironment(
  file.path(data_path, sprintf("alexander_%s_refit.Rdata", col_pert_clean)))


lax$basic_metrics <- basic_data$basic_metrics
lax$basic_data$imb_plot_df <-
  basic_metrics$imb_df %>%
  select(reg, interaction, mrp_x, raking_x, poststrat_x) %>%
  pivot_longer(cols=c(mrp_x, raking_x, poststrat_x)) %>%
  mutate(reg_clean=clean_regressors(reg)) %>%
  mutate(name=clean_method(name))

lax$basic_data$pert_col_clean <- clean_regressors(sim_data$col_pert)

