# Initialize R for knitr.

library(tidyverse)
library(knitr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggforce)
library(gginnards)

library(xtable)

library(gridExtra)
library(patchwork)

library(latex2exp)


# This must be run from within the git repo, obviously.
git_repo_loc <- system("git rev-parse --show-toplevel", intern=TRUE)

paper_directory <- file.path(git_repo_loc, "writing/journal_paper")
data_path <- file.path(git_repo_loc, "src/bayes/simple_simulations/")
r_script_path <- file.path(git_repo_loc, "writing/bcomp_2023")

# opts_chunk$set(fig.width=4.9, fig.height=3)
opts_chunk$set(fig.pos='!h', fig.align='center', dev='png', dpi=300)
opts_chunk$set(echo=knitr_debug, message=knitr_debug, warning=knitr_debug)

# Set the default ggplot theme
theme_set(theme_bw())

# Load into an environment rather than the global space
LoadIntoEnvironment <- function(filename) {
  my_env <- environment()
  load(filename, envir=my_env)
  return(my_env)
}

# Define LaTeX macros that will let us automatically refer
# to simulation and model parameters.
DefineMacro <- function(macro_name, value, digits=3) {
  #sprintf_code <- paste("%0.", digits, "f", sep="")
  value_string <- format(value, big.mark=",", digits=digits, scientific=FALSE)
  cat("\\newcommand{\\", macro_name, "}{", value_string, "}\n", sep="")
}

# Aspect ratio is height / width
base_aspect_ratio <- 5 / (5 * 2)  # This one was working 7/7/21
base_image_width <- 6

SetImageSize <- function(aspect_ratio=base_aspect_ratio,
                         image_width=1.0) {

  ow <- sprintf("%0.3f\\linewidth", image_width * 0.98)
  oh <- sprintf("%0.3f\\linewidth", aspect_ratio * image_width * 0.98)

  fw <- base_image_width * image_width
  fh <- fw * aspect_ratio

  opts_chunk$set(out.width=ow,
                 out.height=oh,
                 fig.width=fw,
                 fig.height=fh)
}

SetImageSize()
