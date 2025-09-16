# Use this script to debug and edit the knit graphs without re-compiling in latex.

base_dir <- "/home/rgiordan/Documents/git_repos/Presentations/BSTARS_mrpaw_092025"

knitr_debug <- FALSE # Set to true to see error output
cache <- FALSE

setwd(base_dir)
source(file.path(base_dir, "figures_knitr/initialize.R"))
source(file.path(paper_directory, "figures_knitr/load_data.R"))
source(file.path(paper_directory, "figures_knitr/define_macros.R"))
#source(file.path(paper_directory, "figures_knitr/predictions_plot.R"), print.eval=TRUE)
#source(file.path(paper_directory, "figures_knitr/imbalance_primary.R"), print.eval=TRUE)
source(file.path(paper_directory, "figures_knitr/imbalance_interaction.R"), print.eval=TRUE)



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
