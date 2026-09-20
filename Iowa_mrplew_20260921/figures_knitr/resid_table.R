
MakeResidTable <- function(refit_env, col_pert) {
    resid_df <- refit_env$resid_df
    table_resid_df <- 
        resid_df %>%
        group_by(x_pert) %>%
        summarize(ybar=mean(y), residbar=mean(resid))

    names(table_resid_df) <-
        c(col_pert, 
        "$\\overline{y}$", 
        "$\\overline{y - \\hat{y}}$")

    table_resid_df %>%
        kable(format="latex", escape=FALSE, digits=3, align=c("|r", "|l", "|l|"),
            caption="Mean response and residuals by interaction value for Name Change") %>%
        kable_styling(latex_options=c("hold_position"), position="center")
}

MakeResidTable(alexander_refit, "\\AlexanderColpert{}")
