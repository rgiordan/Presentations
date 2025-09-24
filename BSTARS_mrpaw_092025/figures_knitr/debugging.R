# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/BSTARS_mrpaw_092025"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))
source(file.path(paper_directory, "figures_knitr/predictions_plot.R"), print.eval=TRUE)

#source(file.path(paper_directory, "figures_knitr/imbalance_primary.R"), print.eval=TRUE)
#source(file.path(paper_directory, "figures_knitr/imbalance_interaction.R"), print.eval=TRUE)
#source(file.path(paper_directory, "figures_knitr/weights_plot.R"), print.eval=TRUE)


boot_env
ggplot(boot_list$mrp_df) +
  geom_point(aes(x=diff, y=diff_pred)) +
  facet_grid(~ analysis) +
  geom_abline()

# Create the grouped bar chart
mrp_df %>%
  pivot_longer(-analysis) %>%
  filter(name %in% c("sd", "sd_pred", "ij_sd")) %>%
  ggplot(aes(x=analysis, y=value, fill=name)) +
  geom_bar(position = "dodge", stat = "identity")