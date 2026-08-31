

#####################################
# Simulated data for exposition

set.seed(42)
n_obs <- 50
df <- 
  data.frame(x1 = rnorm(n_obs)) %>%
  mutate(x2 = 0.5 * x1 + 0.5 * rnorm(n_obs)) %>%
  mutate(y = as.character(x1 + x2 + 2 * rnorm(n_obs) > 0.6))

shift_base_plot <- ggplot(df) +
  geom_point(aes(x=x1, y=x2, shape=y), size=4) +
  scale_shape_manual(values = c(4, 1)) +
  theme(legend.position="none")

x1_thresh <- 1.0
x1_zone <-
  annotate("rect", xmin = x1_thresh, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "purple", alpha = 0.2)

shift_plot1 <- 
  shift_base_plot +
  x1_zone

shift_plot2 <- 
  shift_plot1 +  
  geom_point(aes(x=x1, y=x2, shape=y), stroke=2, size=4,
             data=filter(df, x1 > x1_thresh))
  
