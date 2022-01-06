# Use this script to debug and edit the knit graphs without re-compiling in latex.

#base_dir <- "/home/rgiordan/Documents/git_repos/AdversarialInfluenceWorkbench"
base_dir <- "/home/rgiordan/Documents/git_repos/Presentations"

paper_directory <- file.path(base_dir, "JobTalk_2021")

knitr_debug <- FALSE # Set to true to see error output
cash_cache <- FALSE
ohie_cache <- FALSE
microcredit_cache <- FALSE

setwd(paper_directory)
source(file.path(paper_directory, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))



plot_line_df <- 
    microcredit_refit_env$line_df %>% 
    filter(site == "Mexico")
ymax <- max(c(plot_line_df$pred, plot_line_df$refit))
ymin <- min(c(0, plot_line_df$pred, plot_line_df$refit))

analysis_df <- microcredit_refit_env$analysis_df

orig_betahat <-
    analysis_df %>%
    filter(site == "Mexico") %>%
    pull(betahat)

reg_se <-
    analysis_df %>%
    filter(site == "Mexico") %>%
    pull(reg_se)


plot1 <- plot_line_df %>%
    ggplot(aes(group=site, x=prop_drop)) +
    geom_line(aes(y=pred, color="Prediction")) + 
    theme(legend.title = element_blank()) +
    ylim(ymin, ymax) +
    ylab(TeX("Change in $\\phi$")) +
    geom_hline(aes(yintercept=0)) +
    geom_ribbon(
        aes(ymin=ymin), ymax=reg_se * 1.96, alpha=0.1) +
    xlab("Proportion of points dropped")

plot2 <- plot1 +
    geom_line(aes(y=refit, color="Refit"))

grid.arrange(plot1, plot2, ncol=2)



source(file.path(paper_directory,
                 "figures_knitr/microcredit/refit_line.R"),
       echo=knitr_debug, print.eval=TRUE)



# source(file.path(paper_directory,
#                  "figures_knitr/microcredit/microcredit_profit_results_table.R"),
#        echo=knitr_debug, print.eval=TRUE)
# 
# 
# source(file.path(paper_directory,
#                  "figures_knitr/cash_transfers/cash_transfers_results_table.R"),
#        echo=knitr_debug, print.eval=TRUE)
# 
# 
# source(file.path(paper_directory,
#                  "figures_knitr/OHIE/OHIE-table_results.R"),
#        echo=knitr_debug, print.eval=TRUE)
