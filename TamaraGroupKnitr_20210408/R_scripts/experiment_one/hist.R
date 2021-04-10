PlotHist <- function(x) {
  return(
    ggplot() + geom_histogram(aes(x=x), bins=50)
  )
}

grid.arrange(
  PlotHist(e1_data$x) + ggtitle("X histogram") + xlab("x"),
  PlotHist(e1_data$eps) +
    ggtitle("Epsilon histogram") +
    xlab(TeX("$\\epsilon$")),
  ncol=2
)
