# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/DADVI/fd-advi-paper"

paper_directory <- file.path(base_dir)

knitr_debug <- FALSE # Set to true to see error output
example_cache <- FALSE

setwd(paper_directory)
source(file.path(paper_directory, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))


source(file.path(paper_directory, "figures_knitr/define_macros.R"))

source(file.path(paper_directory, "figures_knitr/initialize.R"))

source("figures_knitr/coverage_histogram.R", echo=knitr_debug, print.eval=TRUE)



post_env$posterior_comp_df %>% head()
post_env$posterior_comp_df %>%
    filter(method_1 == "LR", model == "occ_det") %>%
    pull(param)

cg_count_df <-
    all_env$posteriors_df %>%
    filter(method == "LRVB_CG") %>%
    group_by(model) %>%
    summarize(n=n())

all_env$posteriors_df %>%
    filter(method == "LRVB_CG") %>%
    group_by(model, param) %>%
    summarize(n=n())


filter(cg_count_df, model == "occ_det")$n
