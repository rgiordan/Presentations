


diag_df <-
  logistic_env$diag_df %>%
  filter(sample_type == "parametric") %>%
  filter(regs == logistic_env$plot_reg)

base_df <- diag_df %>%
  filter(boot_num == 1) %>%
  filter(sample_type == "parametric")
mrp_change_0 <- base_df %>% filter(spec == "spec0") %>% pull(mrp_change_orig)
mrp_change_1 <- base_df %>% filter(spec == "spec1") %>% pull(mrp_change_orig)
stopifnot(length(mrp_change_0) == 1)
stopifnot(length(mrp_change_1) == 1)


#################
# plots

get_base_plot <- function(data, vline_col) {
  spec_labels <- c(
    "spec0"="Correct specification",
    "spec1"="Misspecification"
  )
  ggplot(data) +
    geom_vline(aes(xintercept=.data[[vline_col]])) +
    OttoMethodColorScale(aesthetic="color") +
    OttoMethodColorScale(aesthetic="fill") +
    guides(fill = "none") +
    facet_grid(spec ~ ., 
               labeller=labeller(spec=spec_labels))
}


append_result_panel <- function(plt, col, method, hist=FALSE) {
  plt <- plt +
    geom_density(aes(x=.data[[col]],
                     y=..density.., 
                     color=OTTO_METHOD_LABELS[method]))
  if (hist) {
    plt <- plt +
      geom_histogram(aes(x=.data[[col]],
                         y=..density.., 
                         fill=OTTO_METHOD_LABELS[method]), alpha=0.2)
  }
  return(plt)      
}


# Make the sequence of intro plots
intro_plots <- list()
intro_plots$plot0 <- 
  get_base_plot(diag_df, "mrp_change_orig") +
  geom_vline(aes(xintercept=0, 
                 linetype="Null value"), color="dark gray") +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 10), limits=c(0, 40)) +
  xlim(xmin=min(diag_df$mrp_change_ij),
       xmax=max(diag_df$mrp_change_true)) +
  scale_linetype(name="") +
  xlab("Covariate shift diagnostic") +
  guides(fill = "none") +
  ylab("")


intro_plots$plot1 <-
  append_result_panel(intro_plots$plot0, "mrp_change_true", "boot", hist=FALSE)
intro_plots$plot2 <-
  append_result_panel(intro_plots$plot1, "mrp_change_ij", "ij", hist=TRUE)
intro_plots$plot3 <-
  append_result_panel(intro_plots$plot2, "mrp_change_otto", "otto", hist=TRUE)



##############################
# Make the sequence of IJ plots 
mrp_df <- logistic_env$mrp_df
#glimpse(mrp_df)

ij_plots <- list()

# For some reason piping doesn't work
base_plot <- 
  get_base_plot(mrp_df, "mrp_orig") +
  xlab("MrP point estimate") +
  ylab("")
ij_plots$mrp <- base_plot %>%
  append_result_panel("mrp_pert", "boot", hist=FALSE) %>%
  append_result_panel("mrp_ij", "ij", hist=TRUE)
ij_plots$mrp


# For some reason piping doesn't work
base_plot <- 
  get_base_plot(mrp_df, "mrp_var_orig") +
  xlab("MrP variance estimate") +
  ylab("")
ij_plots$mrp_var <- base_plot %>%
  append_result_panel("mrp_var_pert", "boot", hist=FALSE) %>%
  append_result_panel("mrp_var_ij", "ij", hist=TRUE)
ij_plots$mrp_var


base_plot <- 
  get_base_plot(diag_df, "mrp_change_orig") +
  xlab("Covariate shift diagnostic") +
  ylab("")
ij_plots$diag1 <- base_plot %>%
  append_result_panel("mrp_change_true", "boot", hist=FALSE) %>%
  append_result_panel("mrp_change_ij", "ij", hist=TRUE)

ij_plots$diag2 <-
  append_result_panel(ij_plots$diag1, "mrp_change_otto", "otto", hist=TRUE)



  


