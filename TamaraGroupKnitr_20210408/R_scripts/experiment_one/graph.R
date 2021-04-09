ggplot() +
  geom_point(aes(x=e1_data$x, y=e1_data$eps)) +
  xlab("x") + ylab(TeX("$\\epsilon$")) +
  ggtitle("Some garbage")
