# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/Stanford_mrplew_20251021"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))

source(file.path(paper_directory, "figures_knitr/simulation_fig.R"))


#source(file.path(paper_directory, "figures_knitr/weights_plot.R"))

df <- data.frame(
  x_start=1,
  x_end=2,
  y_start=1,
  y_end=2
)
ggplot(df, aes(x= x_start, y = y_start, xend = x_end, yend = y_end)) +
  geom_point(aes(x = x_start, y = y_start), color = "blue", size = 3) + # Start points
  geom_point(aes(x = x_end, y = y_end), color = "blue", size = 3) +     # End points
  geom_segment(arrow = arrow(length = unit(0.5, "cm")), color="blue") +             # Draw arrows
  labs(title = "Arrows between points", x =TeX("$y_1$"), y =TeX("$y_2")) +
  geom_label(aes(x=x_start, y=y_start, label=TeX("$Y_S$"))) +
  theme_minimal()  +
  xlim(0.5, 2.5) + ylim(0.5, 2.5)

