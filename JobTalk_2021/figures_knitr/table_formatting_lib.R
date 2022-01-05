

RefitAchieved <- function(beta0, beta_mzse0, beta_pzse0,
                          beta1, beta_mzse1, beta_pzse1, change) {
  sign_diff <- sign(beta0) != sign(beta1)
  sig0 <- sign(beta_mzse0) == sign(beta_pzse0)
  sig1 <- sign(beta_mzse1) == sign(beta_pzse1)
  return(case_when(
    change == "sign" ~ sign_diff,
    change == "significance" ~ sig0 != sig1,
    change == "sign and significance" ~ sign_diff & sig1,
    TRUE ~ as.logical(NA)
  ))
}



FormatValues <- function(results_df) {
    results_df %>%
        pivot_wider(id_cols=c(-metric),
                    names_from=metric,
                    values_from=value) %>%
        mutate(
            sig=(sign(param_pzse) == sign(param_mzse)),
            sig_string=ifelse(sig, "*", ""),
            est_string=sprintf(
                "%0.3f (%0.3f)%s", param, se, sig_string)) %>%
        return()
}


FormatRefitTable <- function(results_df, base_df) {
    # This should accept the output of the refactored zaminfluence
    results_wide_df <-
        inner_join(
            results_df %>% FormatValues(),
            base_df %>% FormatValues(),
            by=c("param_name", "study_case"),
            suffix=c("_rerun", "_base"))

    names(results_wide_df)

    results_formatted_df <-
        results_wide_df %>%
        mutate(success=RefitAchieved(
            beta0=param_base, beta_mzse0=param_mzse_base, beta_pzse0=param_pzse_base,
            beta1=param_rerun, beta_mzse1=param_mzse_rerun, beta_pzse1=param_pzse_rerun,
            change=description)) %>%
        mutate(
            num_string=sprintf(
                "%d = %0.2f\\%%", n_drop, 100 * prop_drop),
            # est_string_rerun=ifelse(
            #     !is.na(success) & success, sprintf(
            #     "\\textbf{%s}", est_string_rerun), est_string_rerun),
            change_string=ordered(
                description,
                levels=c("sign", "significance", "sign and significance"),
                labels=c("Sign change", "Significance change",
                         "Significant sign change")))

    return(results_formatted_df)
}




RenderLatexTable  <- function(table_df, fontsize="\\tiny",
                              study_case_label="Study case",
                              caption=NULL, label=NULL) {
    cat("\n\n")
    cat("\\begin{table}")
    cat(fontsize, "\n")
    cat("\\begin{tabular}{|ccccc|}\n")
    cat("\\toprule\n")
    cat(study_case_label,
        " & Original estimate (SE)",
        " & Target change ",
        " & Refit estimate ",
        " & Observations dropped ",
        "\\\\\n", sep="")
    cat("\\midrule\n")
    cat("\\midrule\n")

    for (case in levels(table_df$study_case)) {
        this_table_df <- filter(table_df, study_case == case)

        # There should be three reruns
        stopifnot(nrow(this_table_df) == 3)

        # All the estimate strings for the original should be the same
        stopifnot(length(unique(this_table_df$est_string_base)) == 1)

        for (i in 1:3) {
            if (i == 2) {
                cat(case, " & ", this_table_df$est_string_base[1])
            } else {
                cat("&")
            }
            cat(" & ", this_table_df[["change_string"]][i] %>% as.character())
            cat(" & ", this_table_df[["est_string_rerun"]][i] %>% as.character())
            cat(" & ", this_table_df[["num_string"]][i] %>% as.character())
            cat("\\\\\n")
        }
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


# Render a table showing only the change sign and significance result.
RenderSimpleLatexTable  <- function(table_df, fontsize="\\small",
                                    study_case_label="Study case",
                                    caption=NULL, label=NULL) {
    cat("\n\n")
    cat("\\begin{table}")
    cat(fontsize, "\n")
    cat("\\begin{tabular}{|ccc|}\n")
    cat("\\toprule\n")
    cat(" Original estimate (SE)",
        " & Refit estimate (SE) ",
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
        cat(this_table_df$est_string_base[1])
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






GetTableCaptionBoilerplate <- function() {
    paste0(
      "The ``Refit estimate'' column shows the result of re-fitting ",
      "the model removing ",
      "the Approximate Most Influential Set. ",
      "Stars indicate significance at the 5\\% level.  ",
      "Refits that achieved the desired change are bolded."
    )
}
