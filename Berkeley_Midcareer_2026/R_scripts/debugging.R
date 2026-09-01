# Use this script to debug and edit the knit graphs without re-compiling in latex.

git_repo_loc <- system("git rev-parse --show-toplevel", intern=TRUE)
base_dir <- file.path(git_repo_loc, "Berkeley_Midcareer_2026")

knitr_debug <- FALSE # Set to true to see error output
simple_cache <- FALSE # Set to true to cache knitr output for this analysis.
single_column <- FALSE
setwd(base_dir)
source(file.path(base_dir, "R_scripts/initialize.R"))


source(file.path(base_dir, "R_scripts/logistic_otto_sim/load_data.R"))
source(file.path(base_dir, "R_scripts/logistic_otto_sim/generate_histogram_plots.R"))
source(file.path(base_dir, "R_scripts/logistic_otto_sim/generate_shift_plots.R"))



#####################################
# Simulated data for exposition

set.seed(42)
n_obs <- 50
df <- 
  data.frame(x1 = rnorm(n_obs)) %>%
  mutate(x2 = 0.5 * x1 + 0.5 * rnorm(n_obs)) %>%
  mutate(y = as.character(x1 + x2 + 2 * rnorm(n_obs) > 0.6))

data_plot <- ggplot(df) +
  geom_point(aes(x=x1, y=x2, shape=y), size=4) +
  scale_shape_manual(values = c(1, 4)) +
  theme(legend.position="none")

x1_thresh <- 0.5
x1_zone <-
  annotate("rect", xmin = x1_thresh, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "purple", alpha = 0.2)

data_plot +
  x1_zone +
  geom_point(aes(x=x1, y=x2, shape=y), stroke=2, size=5,
             data=filter(df, x1 > x1_thresh))
  



#####################################
# Simulated data for flows

set.seed(42)
n_obs <- 200
df <- 
  data.frame(t1 = rnorm(n_obs) + 0.3) %>%
  mutate(t2 = 0.5 * t1 + 0.6 * rnorm(n_obs)) 

data_plot <- ggplot(df) +
  geom_point(aes(x=t1, y=t2))  +
  theme(legend.position="none")
data_plot

# a few "anchor" points in the upper-right quadrant to define the blob's extent
blob_pts <- data.frame(
  x = c(1.5, 2.2, 1.8, 2.6, 1.9, 1.3, 2.4, 2.0, 1.6, 2.8, 1.4, 2.1),
  y = c(1.0, 1.4, 1.8, 1.1, 1.6, 1.3, 0.9, 2.0, 0.7, 1.5, 1.9, 1.2)
)

ggplot(df) +
  geom_mark_hull(data = blob_pts, aes(x = x, y = y),
                 fill = "purple", color = NA, alpha = 0.3,
                 concavity = 1, expand = unit(4, "mm")) +
  geom_point(aes(x = t1, y = t2)) +
  theme(legend.position = "none")

