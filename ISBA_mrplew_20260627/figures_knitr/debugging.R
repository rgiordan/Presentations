# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/ISBA_mrplew_20260627"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))

map_df <- laxphilips_CA_pooling$map_df
make_us_plot(map_df, map_df$region == "california") +
  annotate("text", x = -99, y = 32,
           label = "?", color = "red", size = 8) +
  theme(legend.position  = "none")
