# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/Austin_mrplew_20250926"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))

source("figures_knitr/predictions_plot.R")
source("figures_knitr/imbalance_plots.R")

#source(file.path(paper_directory, "figures_knitr/weights_plot.R"), print.eval=TRUE)

# source("figures_knitr/predictions_plot.R")
# alex_refit_plots$base_plot
# alex_refit_plots$plt1


# ggplot(boot_env$samples_df) +
#   geom_point(aes(x=diff, y=diff_pred)) +
#   facet_grid(~ analysis) +
#   geom_abline()

#source(file.path(paper_directory, "figures_knitr/bootstrap_comparison.R"), print.eval=TRUE)


# source("figures_knitr/imbalance_plots.R")
# alex_imb_plots$interaction_plt
# lax_imb_plots$interaction_plt
# 
# 
# lax_imb_plots$primary_plt


analysis_list <- alex

make_imbalance_plots <- function(analysis_list) {
  basic_data  <- analysis_list$basic_data
  sim_data <- analysis_list$sim_data
  
  imb_plot_df <-
    basic_data$imb_plot_df %>%
    pivot_wider(id_cols=c(reg, interaction, reg_clean), 
                names_from=name, values_from=value) %>%
    mutate(MRP=MRP - Target, Raking=Raking - Target) %>%
    pivot_longer(c(MRP, Target, Raking)) %>%
    mutate(value_pct=100 * value / basic_data$basic_metrics$mrp) %>%
    mutate(reg_clean=case_when(
      reg_clean == basic_data$pert_col_clean ~ paste0(" ---> ", reg_clean),
      TRUE ~ reg_clean))
  
  
  # Jitter so the zeros are visible
  jitter_amount <- max(abs(imb_plot_df$value_pct)) * 5e-4
  primary_plt <-
    imb_plot_df %>%
    filter(!interaction, name != "Target") %>%
    ggplot() +
    geom_bar(
      aes(fill=name, y=value_pct + jitter_amount, x=reg_clean),
      position="dodge", stat="identity") +
    coord_flip() + 
    xlab(NULL) + 
    labs(fill="Method") + 
    ylab("Imbalance (% of MrP estimate)")
  
  #primary_plt
  
  bad_regs <- basic_data$basic_metrics$imb_df %>%
    mutate(mrp_imbalance=mrp_x - poststrat_x) %>%
    filter(interaction) %>%
    arrange(abs(mrp_imbalance)) %>%
    tail(10) %>%
    pull(reg)
  
  #bad_regs <- union(bad_regs)
  
  interaction_imb_df <-
    imb_plot_df %>%
    filter(interaction, name != "Target") %>%
    filter(reg %in% bad_regs)
  
  
  interaction_plt <-
    interaction_imb_df %>%
    ggplot() +
    geom_bar(
      aes(fill=name, y=value_pct, x=reg_clean),
      position="dodge", stat="identity") +
    coord_flip() + 
    xlab(NULL) + 
    labs(fill="Method") + 
    ylab("Imbalance (% of MrP estimate)")
  
  #interaction_plt
  
  return(list(interaction_plt=interaction_plt, primary_plt=primary_plt))
}


alex_imb_plots <- make_imbalance_plots(alex)
lax_imb_plots <- make_imbalance_plots(lax)

lax_imb_plots$primary_plt
alex_imb_plots$primary_plt


alex_imb_plots$interaction_plt
lax_imb_plots$interaction_plt
