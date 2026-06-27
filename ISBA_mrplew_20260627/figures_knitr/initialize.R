# Initialize R for knitr.

library(tidyverse)
library(knitr)
library(kableExtra)
library(xtable)
library(gridExtra)
library(latex2exp)
library(ggforce)

library(maps)
library(mapproj) # Ensure this is installed

# This must be run from within the git repo, obviously.
git_repo_loc <- system("git rev-parse --show-toplevel", intern=TRUE)
paper_directory <- file.path(git_repo_loc, "aapor")
data_path <- file.path(paper_directory, "applications_data")
script_directory <- file.path(paper_directory, "figures_knitr")

GetScriptLoc <- function(script_name) {
    file.path(script_directory, script_name)
}

# Set some figure defaults.
# opts_chunk$set(fig.width=4.9, fig.height=3)
opts_chunk$set(fig.pos='!h', fig.align='center', dev='png', dpi=300)
opts_chunk$set(echo=knitr_debug, message=knitr_debug, warning=knitr_debug)

# Set the default ggplot theme.
theme_set(theme_bw())

source(file.path(script_directory, "utils_lib.R"))

