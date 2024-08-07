
df <- arm_env$combined_df_wide_labeled

cat("\\begin{center}\n")

print(xtable(table(df[c("family_label", "is_glmm_label")]),
             align="|l|c|c|"),
      hline.after=-1:2, floating=FALSE
    )
cat("\n\nModels\n\n")

cat("\\vspace{0.1in}")
print(xtable(table(df[c("has_sigma_label", "is_cov_label")]),
             align="|l|c|c|"),
      hline.after=-1:2, floating=FALSE
    )
cat("\n\nCovariances to estimate\n\n")

cat("\\end{center}\n")
