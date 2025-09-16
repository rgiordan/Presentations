# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/BSTARS_mrpaw_092025"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))
#source(file.path(paper_directory, "figures_knitr/predictions_plot.R"), print.eval=TRUE)

sim_data

basic_metrics <- basic_data$basic_metrics

clean_regressors <- function(reg) {
  df <- data.frame(reg=reg) %>%
    mutate(reg_clean=reg) %>%
    mutate(reg_clean=str_replace(reg_clean, "decade_married_rk", "married ")) %>%
    mutate(reg_clean=str_replace(reg_clean, "age_group_rk", "age ")) %>%
    mutate(reg_clean=str_replace(reg_clean, "region_rk", "")) %>%
    mutate(reg_clean=str_replace(reg_clean, "educ_group", "")) %>%
    mutate(reg_clean=str_replace(reg_clean, ":", " x "))
  return(df$reg_clean)
}

clean_method <- function(method) {
  case_when(
    method == "mrp_x" ~ "MRP",
    method == "raking_x" ~ "Raking",
    method == "poststrat_x" ~ "Target",
    TRUE ~ "UNKNOWN"
  )
}

imb_df <-
  basic_metrics$imb_df %>%
  select(reg, interaction, mrp_x, raking_x, poststrat_x) %>%
  pivot_longer(cols=c(mrp_x, raking_x, poststrat_x)) %>%
  mutate(reg_clean=clean_regressors(reg)) %>%
  mutate(name=clean_method(name))

pert_col <- clean_regressors(sim_data$col_pert)

imb_df %>%
  filter(!interaction) %>%
  ggplot() + 
  geom_bar(
    aes(fill=name, y=value, x=reg_clean),
    position="dodge", stat="identity") +
  # theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
  #       axis.ticks.x=element_blank()) +
  coord_flip() + xlab(NULL) + ylab(NULL) + labs(fill="Method")


imb_df %>%
  mutate(reg_clean=case_when(reg_clean == pert_col ~ paste0(" ---> ", reg_clean),
                             TRUE ~ reg_clean)) %>%
  filter(interaction) %>%
  ggplot() + 
  geom_bar(
    aes(fill=name, y=value, x=reg_clean),
    position="dodge", stat="identity") +
  # theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
  #       axis.ticks.x=element_blank()) +
  coord_flip() + xlab(NULL) + ylab(NULL) + labs(fill="Method")
