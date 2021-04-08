# Initialize R for knitr.  After setting `knitr_debug`, source this file
# in a chunk at the top of your knitr file.

library(tidyverse)
library(knitr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggforce)
library(xtable)

library(gridExtra)
library(patchwork)

library(latex2exp)

# Set variables containing absolute paths to the files.
# This must be run from within the git repo, obviously.
git_repo_loc <- system("git rev-parse --show-toplevel", intern=TRUE)
paper_directory <- file.path(git_repo_loc, "TamaraGroupKnitr_20210408/")
data_path <- file.path(paper_directory, "R_scripts/data/")

# Set default chunk settings for all figures.
opts_chunk$set(fig.pos='!h', fig.align='center', dev='png', dpi=300)

# Set global options.  Before sourcing this script, you must set the
# vairable `knitr_debug` in a chunk.  If `knitr_debug` is TRUE, then
# lots of helpful R output will be in your tex.
opts_chunk$set(echo=knitr_debug, message=knitr_debug, warning=knitr_debug)


##############################################################
# Helper functions for knitr

# Load an Rdata file into an environment rather than the global space
LoadIntoEnvironment <- function(filename) {
  my_env <- environment()
  load(filename, envir=my_env)
  return(my_env)
}

# Define LaTeX macros that will let us automatically refer
# to simulation and model parameters.
#
# For example, if you run DefineMacro("foo", 10.3) in knitr chunk, you
# can then refer to $\foo$ in LaTeX, and the output will read 10.3 in the pdf.
DefineMacro <- function(macro_name, value, digits=3) {
  value_string <- format(value, big.mark=",", digits=digits, scientific=FALSE)
  cat("\\newcommand{\\", macro_name, "}{", value_string, "}\n", sep="")
}

##############################################################
# ggplot options, defaults, and helper functions

theme_set(theme_bw())

# Set ggplot fontsizes
axis_ticksize = 4
axis_title_size = 7
title_size = 7

GetFontsizes <- function(scaling = 1){
  axis_ticksize = axis_ticksize * scaling
  axis_title_size = axis_title_size * scaling
  title_size = title_size * scaling

  fontsize_theme <- theme(
    axis.text.x = element_text(size=axis_ticksize),
    axis.text.y = element_text(size=axis_ticksize),
    axis.title.x = element_text(size=axis_title_size),
    axis.title.y = element_text(size=axis_title_size),
    legend.text = element_text(size=axis_title_size),
    plot.title = element_text(size=title_size),
    axis.ticks.length = unit(0.05, "cm"),
    strip.text = element_text(size=axis_title_size,
                              margin=margin(.05, 0, .05, 0, "cm")),
    legend.margin=margin(-10,-10,-10,-10))

  return(fontsize_theme)
}


# A convenient function for extracting only the legend from a ggplot.  This
# is useful when you have multiple side-by-side images and you don't want
# to repeat a legend.
#
# Taken from
# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
GetLegend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


##############################################################
# Image size wrangling

# knitr has two different image size parameters:
# - The size on the page (out.width, out.height)
# - The size of the R graphic window (fig.width, fig.height).
# If these quantities are not in the same ratio, the image will be distorted.
#
# Here, I define a function that sets a default relationship between the two
# and allows the user to specify an image size only through its aspect
# ratio (the ratio of the height of the image to the width of the image).

# The default aspect ratio
base_aspect_ratio <- 8 / (5 * 2)

# The default R figure width.  If this is larger, the R window will be larger
# for a given figures size on the page, and the ggplot's text will appear smaller.
base_figure_width <- 4.

# Set the chunk options for a figure with a given
# - aspect ratio (height / width)
# - image_width (as a proportion of the printed page)
SetImageSize <- function(aspect_ratio=base_aspect_ratio, image_width=1.0) {

  ow <- sprintf("%0.3f\\linewidth", image_width * 0.98)
  oh <- sprintf("%0.3f\\linewidth", aspect_ratio * image_width * 0.98)

  fw <- base_figure_width * image_width
  fh <- fw * aspect_ratio

  opts_chunk$set(out.width=ow,
                 out.height=oh,
                 fig.width=fw,
                 fig.height=fh)
}

# Use the defaults by default.
SetImageSize()

# SetFullImageSize <- function() SetImageSize(
#     aspect_ratio=base_aspect_ratio, image_width=base_image_width)
#
# # Default to a full image.
# SetFullImageSize()
