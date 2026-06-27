example_df <-
  data.frame(y1=c(1, 2), y2=c(1, 3))

ypath_plt <- ggplot(example_df) +
  geom_point(aes(x=y1, y=y2)) +
  geom_line(aes(x=y1, y=y2), linetype="dotted") +
  expand_limits(x=c(0.5, 2.5), y=c(0, 3.5)) +
  annotate("text", x = 0.8, y = 1, 
           label = TeX("$Y_S$"),  size = 5) +
  annotate("text", x = 2.2, y = 3, 
           label = TeX("$\\tilde{Y}_S$"),  size = 5) +
  annotate("text", x = 1.5, y = 2.3, 
           label = TeX("$t$"),  size = 5) +
  geom_segment(aes(x = 1.6, y = 2.5, 
                   xend = 1.6 + 0.2, yend = 2.5 + 0.2 * 2),
               arrow = arrow()) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("") +
  theme(panel.grid = element_blank()) +
  ggtitle("Path through response space")


ypath_region_plt <-
    ypath_plt +
    geom_ellipse(aes(x0=1, y0=1, a=0.4, b=0.8, angle=0), 
                alpha=0.1, fill="purple", color="transparent",
                data=data.frame(foo=1))





# Mrp plot

t_grid <- seq(0, 1, length.out=100)
mrp1 <- 0.3 + 0.04 * t_grid
mrp2 <- 0.28 + 0.06 * t_grid + 0.02 * t_grid^2

example_mrp_df <-
  data.frame(t=t_grid,
             mrp1=mrp1,
             mrp2=mrp2)
ggplot(example_mrp_df, aes(x=t)) +
  geom_line(aes(y=mrp1, color="Calibration weighting")) +
  geom_line(aes(y=mrp2, color="Hierarchical / logistic MrP")) +
  labs(color = "Method") +
  ylab(NULL)

all_mrp <- c(example_mrp_df$mrp1, example_mrp_df$mrp2)
ylim <- c(min(all_mrp), max(all_mrp))

cw_plt <- ggplot(example_mrp_df, aes(x=t)) +
  geom_line(aes(y=mrp1), color="blue") +
  ylab(TeX("$\\hat{\\mu}^{\\textrm{WGT}}$")) +
  ggtitle("Calibration weighting") +
  expand_limits(y=ylim) +
  annotate("text", x = 0.50, y = 0.305, 
           label = TeX("Slope = $w / N_S$"),  size = 5)


mrp_plt <- ggplot(example_mrp_df, aes(x=t)) +
  geom_line(aes(y=mrp2), color="red") +
  ylab(TeX("$\\hat{\\mu}^{\\textrm{MrP}}$")) +
  ggtitle("Hierarchical / logistic MrP") +
  expand_limits(y=ylim)


mrplew_plt <-
  mrp_plt +
  geom_abline(aes(slope=0.06, intercept=0.28), color="purple") +
  annotate("text", x = 0.50, y = 0.285, 
           label = TeX("Slope = $w^{MrP} / N_S$"),  size = 5)

