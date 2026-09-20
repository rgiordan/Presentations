
grid.arrange(
  weight_plt_alexander,
  PlotBalanceForEnv(alexander, interactions=TRUE, threshold=3, drop_names=FALSE), #+
    #ggtitle("Name Change\nlove plot"),
  ncol=2,
  widths=c(0.4, 0.6)
)


