

extra_levels <- c(mrp="MrP", truth="Target Difference")
GenerateRefitPlot <- function(refit_df, mrp_base) {
  ggplot(refit_df, aes(x=delta)) +
    geom_line(aes(y=true_mrp_diff, 
                  color=extra_levels["truth"], 
                  linetype="base")) +
    geom_line(aes(y=raking_diff,
                  color="Raking", 
                  linetype="base")) +
    geom_line(aes(y=mrp_pred - mrp_base, 
                  color=extra_levels["mrp"], 
                  linetype="pred")) +
    geom_ribbon(aes(ymin=mrp_pred_q10 - mrp_base, 
                    ymax=mrp_pred_q90 - mrp_base, 
                    fill="MrPlew", 
                    color=NULL), alpha=0.1) +
    geom_line(aes(y=mrp_refit - mrp_base, 
                  color=extra_levels["mrp"], 
                  linetype="rerun")) +
    geom_point(aes(y=mrp_refit - mrp_base, 
                  color=extra_levels["mrp"])) +
    MethodColorScale(extra_levels=extra_levels["truth"], 
                     mrplew_label="MrP") +
    MethodColorScale(extra_levels=extra_levels["truth"],
                     aesthetic="fill", guide="none") +
    # scale_fill_discrete(guide="none") +
    # scale_color_discrete(name="Method") +
    scale_linetype_manual(name="",
                            values = c("base" = "solid", 
                                      "pred" = "dashed", 
                                      "rerun" = "dotted"),
                            breaks=c("pred", "rerun"),
                            labels=c("MrPlew prediction", "MCMC rerun")) +
    ylab(TeX("Change in $\\mu$")) +
    xlab("Step size in the direction of imbalance")
}


alexander_refit_plot <- GenerateRefitPlot(
  alexander_refit$refit_df, alexander$save_list$mrp)

laxphilips_refit_plot <- GenerateRefitPlot(
  laxphilips_refit$refit_df, laxphilips$save_list$mrp)