CleanNames <- function(balance_df, analysis_name) {
  if (analysis_name == "alexander") {
    balance_df <-
      balance_df %>%
      mutate(reg=reg %>%
        str_replace("educ_group", "educ") %>%
        str_replace("decade_married_rk", "married") %>%
        str_replace("age_group_rk", "age") %>%
        str_replace("region_rk", "") 
        )
  }
  if (analysis_name == "stories") {
    balance_df <-
      balance_df %>%
      mutate(reg=reg %>%
        str_replace("region_rk", "") %>%
        str_replace("gender", "") %>%
        str_replace("education_level", "educ") %>%
        str_replace("age_group", "age")
        )
  }
  if (analysis_name == "laxphilips") {
    balance_df <-
      balance_df %>%
      mutate(reg=reg %>%
        str_replace("race.wbh", "race") %>%
        str_replace("race.w", "race") %>%
        str_replace("edu.cat", "educ") %>%
        str_replace("age.cat", "age") %>%
        str_replace("edu.low", "educ")
        )
  }
  return(balance_df)
}


# Set the order of the factors so that mrp and raking have
# roughly consistent colors with the rest of the graphs.
# ... in fact this doesn't appear to work.
AssignMethodNames <- function(balance_df) {
  balance_df %>%
    mutate(method_name=factor(
    method_names[method], 
    levels=c(method_names["mrp"], 
             method_names["raking"],
             method_names["uncorrected"])))
}


PlotBalance <- function(balance_df) {
  method_names <- c("mrp"="MrPlew", "raking"="Raking", "uncorrected"="Uncorrected")

  # "love plot"
  plt <-
    balance_df %>%
    AssignMethodNames() %>%
    mutate(abs_diff=abs(difference)) %>%
    mutate(method_name=method_names[method]) %>%
    ggplot() +
      geom_point(aes(x=reg, y=100 * abs_diff, color=method_name, 
                    shape=method_name), size=2) +
      geom_hline(aes(yintercept=0)) +
      ylab("Absolute mean differences (percentage points)") + xlab(NULL) +
      labs(color="Method", shape="Method") +
      coord_flip() +
      MethodColorScale(extra_levels=method_names["uncorrected"])
  return(plt)
}


PlotBalanceForEnv <- function(analysisenv, interactions, threshold=5, drop_names=FALSE) {
  balance_df <- CleanNames(analysisenv$balance_df, analysisenv$simple_name) 
  if (!interactions) {
    balance_df <- filter(balance_df, raking_reg)
    plot_title <- sprintf("%s\n(main margins)", analysisenv$name)
  } else {
    balance_df <- filter(balance_df, !raking_reg & enough_data)
    plot_title <- sprintf("%s\n(select interactions)", analysisenv$name)

    # Only keep interactions with at least one imbalance over the threshold
    keep_regs <- balance_df %>%
        select(reg, method, pct_diff) %>%
        pivot_wider(id_cols=reg, names_from=method, values_from=pct_diff) %>%
        filter(abs(mrp) > threshold | abs(raking) > threshold) %>%
        pull(reg)
    balance_df <-
        balance_df %>%
        filter(reg %in% keep_regs)

    if (drop_names) {
      balance_df <-
        balance_df %>%
          mutate(reg_orig=reg) %>%
          mutate(reg=paste0("Reg", as.numeric(factor(reg_orig))))
    }

  }
  
  balance_plt <-
    PlotBalance(balance_df) +
    ggtitle(plot_title)

   return(balance_plt)
}