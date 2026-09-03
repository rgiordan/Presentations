
logistic_env <- LoadIntoEnvironment(file.path(data_path, "logistic_sim.Rdata"))
logistic_env$plot_reg <- "X2_rk(0.878, Inf]"


OTTO_METHOD_LABELS <-
  c(boot="Parametric bootstrap",
    ij="IJ approximation",
    otto="Local flow approximation")


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
OttoMethodColorScale <- function(extra_levels = NULL, extra_colors = NULL,
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