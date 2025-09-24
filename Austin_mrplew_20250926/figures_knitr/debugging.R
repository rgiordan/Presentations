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

source(file.path(paper_directory, "figures_knitr/bootstrap_comparison.R"), print.eval=TRUE)


# source("figures_knitr/imbalance_plots.R")
# alex_imb_plots$interaction_plt
# lax_imb_plots$interaction_plt
# 
# 
# lax_imb_plots$primary_plt


clean_sd_method <- function(method) {
  case_when(
    method == "ij_sd" ~ "MrPlew estimate",
    method == "sd" ~ "MrP parametric bootstrap",
    method == "raking_sd" ~ "Raking",
    TRUE ~ "UNKNOWN"
  )
}

# Both ij_sd and sd are of the MrP estimator
method_colors <- c(
  "MrPlew estimate" = "#F8766D",
  "Raking" = "#00BA38",
  "MrP parametric bootstrap" = "#F876B0"
)



boot_df <-
  boot_env$mrp_df %>%
  filter(analysis %in% c("alexander", "laxphilips")) %>%
  left_join(data.frame(
    analysis=c("alexander", "laxphilips"),
    raking_sd=c(
      sqrt(attr(alex$basic_metrics$raking, "var")),
      sqrt(attr(lax$basic_metrics$raking, "var"))
    )), by="analysis") %>%
  pivot_longer(cols=-analysis) %>%
  filter(name %in% c("sd", "ij_sd", "raking_sd")) %>%
  mutate(method_label=clean_sd_method(name))



# Create the grouped bar chart
plt <- boot_df %>%
  ggplot(aes(x=analysis, y=value, fill=method_label)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=method_colors) +
  xlab(NULL) + 
  labs(fill="") +
  ylab("Frequentist standard error")

print(plt)
