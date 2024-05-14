git_repo_loc <- "/home/rgiordan/Documents/git_repos/InfinitesimalJackknifeWorkbench/"
paper_directory <- file.path(git_repo_loc, "writing/bcomp_2023")
setwd(paper_directory)

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE # Set to true to cache knitr output for this analysis.
r_script_dir <- "R_scripts/"
source(file.path(r_script_dir, "initialize.R"), echo=FALSE)
source(file.path(r_script_dir, "load_data.R"), echo=FALSE)


