

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
blob_pts$x <- blob_pts$x - mean(blob_pts$x)
blob_pts$y <- blob_pts$y - mean(blob_pts$y)

x0 <- 1.5
y0 <- 0

plot_blob <- function(scale) {
  geom_mark_hull(data = blob_pts, aes(x = scale * x + x0, y = scale * y + y0),
                 fill = "purple", color = NA, alpha = 0.1,
                 concavity = 5, expand = unit(4, "mm"))  
}

arrow_len <- 0.15  # fixed arrow length so field is uniform, not magnitude-scaled

vec_df <- df %>%
  mutate(
    dx = x0 - t1,
    dy = y0 - t2,
    dist = sqrt(dx^2 + dy^2),
    dist_mod = case_when(
      dist > 0.8 ~ 0.8,
      dist < 0.3 ~ 0.3,
      TRUE ~ dist
    ),
    xend = t1 + arrow_len * dx * dist_mod,
    yend = t2 + arrow_len * dy * dist_mod,
    w=1 / (dist_mod + 1.0)
  )


posterior_plots <- list()

posterior_plots$base_plot <- ggplot(vec_df) +
  geom_point(aes(x = t1, y = t2)) +
  theme(legend.position = "none") +
  xlab(TeX("$\\theta_1$")) + ylab(TeX("$\\theta_2$"))


posterior_plots$h_plot <-
  posterior_plots$base_plot +
  plot_blob(1) +
  plot_blob(1.5) +
  plot_blob(2)

posterior_plots$flow_plot <-
  posterior_plots$h_plot +
  geom_segment(aes(x = t1, y = t2, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.08, "cm")),
               color = "gray40", alpha = 0.6)

posterior_plots$w_plot <-
  posterior_plots$h_plot +
  geom_point(aes(x = t1, y = t2, size=w))


