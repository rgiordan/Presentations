# Use this script to debug and edit the knit graphs without re-compiling in latex.

git_repo_loc <- system("git rev-parse --show-toplevel", intern=TRUE)
base_dir <- file.path(git_repo_loc, "Berkeley_Midcareer_2026")

knitr_debug <- FALSE # Set to true to see error output
simple_cache <- FALSE # Set to true to cache knitr output for this analysis.
single_column <- FALSE
setwd(base_dir)
source(file.path(base_dir, "R_scripts/initialize.R"))




OTTO_METHOD_LABELS <-
  c(boot="Parametric bootstrap",
    ij="IJ approximation",
    otto="Otto-grad approximation")


# Return a scale_color_manual or scale_fill_manual layer with consistent colors
# for the two core method levels.  The core levels occupy positions 1 and 3 of
# a 4-color evenly-spaced HCL wheel (180° apart), so up to two auto-selected
# extras land at positions 2 and 4 and remain maximally distinct from the cores.
#
# Arguments:
#   extra_levels  Character vector of additional level names (length <= 2).
#   extra_colors  Optional character vector of hex colors for extra_levels.
#                 When omitted, colors are chosen automatically.
#   aesthetic     Either "color" (default) or "fill".
#   ...           Additional arguments forwarded to scale_*_manual()
#                 (e.g. name=, breaks=, labels=).
#
# To change a core level name, edit the `core_levels` vector below.
MethodColorScale <- function(extra_levels = NULL, extra_colors = NULL,
                             aesthetic = c("color", "fill"),
                             ...) {
  aesthetic <- match.arg(aesthetic)
  
  # Four evenly-spaced hues (90° apart); core levels use positions 1, 3, and 5
  all_hues <- seq(15, 375, length.out = 6)[1:6]
  
  core_levels <- OTTO_METHOD_LABELS
  core_colors <- hcl(h = all_hues[c(1, 3, 5)], c = 100, l = 65)
  names(core_colors) <- core_levels
  
  if (!is.null(extra_levels)) {
    n_extra <- length(extra_levels)
    stopifnot(n_extra <= 2)
    if (is.null(extra_colors)) {
      extra_colors <- hcl(h = all_hues[c(2, 4)][seq_len(n_extra)],
                          c = 100, l = 65)
    }
    stopifnot(length(extra_colors) == n_extra)
    names(extra_colors) <- extra_levels
    all_colors <- c(core_colors, extra_colors)
  } else {
    all_colors <- core_colors
  }
  
  if (aesthetic == "fill") {
    scale_fill_manual(values = all_colors, name="Method", ...)
  } else {
    scale_color_manual(values = all_colors, name="Method", ...)
  }
}



logistic_env <- LoadIntoEnvironment(file.path(data_path, "logistic_sim.Rdata"))
reg <- "X2_rk(0.878, Inf]"
diag_df <-
  logistic_env$diag_df %>%
  filter(sample_type == "parametric") %>%
  filter(regs == !!reg)

base_df <- diag_df %>%
  filter(boot_num == 1) %>%
  filter(sample_type == "parametric")
mrp_change_0 <- base_df %>% filter(spec == "spec0") %>% pull(mrp_change_orig)
mrp_change_1 <- base_df %>% filter(spec == "spec1") %>% pull(mrp_change_orig)
stopifnot(length(mrp_change_0) == 1)
stopifnot(length(mrp_change_1) == 1)

plot0 <- 
  ggplot(diag_df) +
    geom_vline(aes(xintercept=mrp_change_0,
                   linetype="Correct specification"), lwd=2) +
    geom_vline(aes(xintercept=mrp_change_1, 
                   linetype="Misspecified"), lwd=2) +
    geom_vline(aes(xintercept=0), color="gray") +
  scale_linetype(name="Specification") +
  MethodColorScale(aesthetic="color")


plt_alpha <- 0.5

get_result_panel <- function(col, method) {
  geom_density(aes(x=.data[[col]],
                     y=..density.., 
                     color=OTTO_METHOD_LABELS[method]),
                 alpha=0.5)
}

plot1 <- plot0 + get_result_panel("mrp_change_true", "boot")
plot2 <- plot1 + get_result_panel("mrp_change_ij", "ij")
plot3 <- plot2 + get_result_panel("mrp_change_otto", "otto")

grid.arrange(
  plot0,
  plot1,
  plot2,
  plot3,
  ncol=4  
)
