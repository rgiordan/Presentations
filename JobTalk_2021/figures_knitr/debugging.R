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

RenderSimpleLatexTable  <- function(table_df, fontsize="\\tiny",
                                    study_case_label="Study case",
                                    caption=NULL, label=NULL) {
    cat("\n\n")
    cat("\\begin{table}")
    cat(fontsize, "\n")
    cat("\\begin{tabular}{|cccc|}\n")
    cat("\\toprule\n")
    cat(study_case_label,
        " & Original estimate (SE)",
        " & Refit estimate ",
        " & Observations dropped ",
        "\\\\\n", sep="")
    cat("\\midrule\n")
    cat("\\midrule\n")
    
    for (case in levels(table_df$study_case)) {
        this_table_df <-
            filter(table_df,
                   study_case == case,
                   target_signal == "both")
        
        # There should be one rerun
        stopifnot(nrow(this_table_df) == 1)
        
        # All the estimate strings for the original should be the same
        stopifnot(length(unique(this_table_df$est_string_base)) == 1)
        
        i <- 1
        cat(case, " & ", this_table_df$est_string_base[1])
        # cat(" & ", this_table_df[["change_string"]][i] %>% as.character())
        cat(" & ", this_table_df[["est_string_rerun"]][i] %>% as.character())
        cat(" & ", this_table_df[["num_string"]][i] %>% as.character())
        cat("\\\\\n")
        cat("\\midrule\n")
    }
    
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    if (!is.null(caption)) {
        cat("\\caption{", caption, "}\n")
    }
    if (!is.null(label)) {
        cat("\\label{table:", label, "}\n", sep="")
    }
    cat("\\end{table}")
    
    cat("\n\n")
}

results_df <-
    microcredit_env$results_df  %>%
    filter(analysis=="rerun") %>%
    filter(site == "Mexico") %>%
    mutate(study_case=ordered(site))

base_df <-
    microcredit_env$base_df %>%
    filter(site == "Mexico") %>%
    mutate(study_case=ordered(site))

table_df <- FormatRefitTable(results_df, base_df)

RenderSimpleLatexTable(
    table_df,
    label="mc_profit_results",
    caption="Microcredit Mexico results \\citep{angelucci2015microcredit}.")


source(file.path(paper_directory,
                 "figures_knitr/microcredit/microcredit_profit_results_table.R"),
       echo=knitr_debug, print.eval=TRUE)



source(file.path(paper_directory,
                 "figures_knitr/cash_transfers/cash_transfers_results_table.R"),
       echo=knitr_debug, print.eval=TRUE)


source(file.path(paper_directory,
                 "figures_knitr/OHIE/OHIE-table_results.R"),
       echo=knitr_debug, print.eval=TRUE)
