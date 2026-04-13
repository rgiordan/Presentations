# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/Stanford_mrplew_20251021"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))



source(file.path(paper_directory, "figures_knitr/partial_pooling.R"))


pp_df <- pp_env$pp_df

print(
  ggplot(pp_df) +
    geom_point(aes(x=region_pop, y=region_sur, size=infl_norm)) +
    #scale_fill_gradient(low="black", high = "white", limits = c(0, 1)) +
    labs(size = "Proportion of total weight") +
    xlab("Population region") +
    scale_size_area(limits = c(0, 1), max_size=20) +
    ylab("Survey region")
)

