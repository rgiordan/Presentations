table_df <- e2_data$table_df
cat("\\begin{center}")
print(xtable(table_df, digits=rep(4, ncol(table_df) + 1)),
      include.rownames=FALSE, floating=FALSE)
cat("\\end{center}")
