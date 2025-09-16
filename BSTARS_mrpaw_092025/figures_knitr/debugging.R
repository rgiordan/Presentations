# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/BSTARS_mrpaw_092025"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))
#source(file.path(paper_directory, "figures_knitr/predictions_plot.R"), print.eval=TRUE)
#source(file.path(paper_directory, "figures_knitr/imbalance_primary.R"), print.eval=TRUE)
#source(file.path(paper_directory, "figures_knitr/imbalance_interaction.R"), print.eval=TRUE)

source(file.path(paper_directory, "figures_knitr/predictions_plot.R"), print.eval=TRUE)




# This defines but does not print three refit plots

clean_sim_method <- function(method) {
  case_when(
    method == "mrp" ~ "MRP",
    method == "raking" ~ "Raking",
    method == "Truth" ~ "Target",
    TRUE ~ "UNKNOWN"
  )
}

lwd_val <- 1.2

method_levels <- c("MRP", "Raking", "Target")
prediction_constants_df <-
  sim_data$prediction_constants_df %>%
  mutate(method=clean_sim_method(method)) %>%
  mutate(method=factor(method, levels=method_levels))


predictions_band_df <-
  sim_data$predictions_band_df %>%
  mutate(method=clean_sim_method(method)) %>%
  mutate(method=factor(method, levels=method_levels))

predictions_band_df$method

method_colors <- c(
  "MRP" = "#F8766D",
  "Raking" = "#00BA38",
  "Target" = "#619CFF"
)


base_plot <-
  predictions_band_df %>%
  ggplot(aes(x=delta)) +
  xlab(TeX("$\\delta$")) +
  ylab(sprintf(
    "Change in target population mean\n(Recall MrP = %0.2f)",
    basic_data$basic_metrics$mrp)) +
  scale_color_manual(values = method_colors) +
  scale_fill_manual(values = method_colors) +
  guides(fill="none") + labs(color="Method") +
  ylim(0, 0.062)


method_linetypes <- c(
  "Mean pred." = "solid",
  "Binary pred." = "dashed",
  "Refit" = "dotted"
)

pred_plot_3 +
  scale_linetype_manual(values=method_linetypes) +
  guides(colour = guide_legend(order = 1),
         linteype = guide_legend(order = 2)) 
