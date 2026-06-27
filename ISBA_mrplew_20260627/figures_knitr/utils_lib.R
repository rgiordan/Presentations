
# Load into an environment rather than the global space.
LoadIntoEnvironment <- function(filename) {
  my_env <- environment()
  load(filename, envir=my_env)
  return(my_env)
}




# Width in rendered units.  High numbers give smaller fonts on the images.
# Setting base_image_width to the actual width of a page of paper (8.5)
# lets ggplot pick what it thinks is best.

# Aspect ratio is height / width
base_aspect_ratio <- 0.45
base_image_width <- 6.5

SetImageSize <- function(aspect_ratio, image_width=base_image_width) {
  # Set the size on the page
  ow <- "0.98\\linewidth"
  oh <- sprintf("%0.3f\\linewidth", aspect_ratio * 0.98)

  # Set the size in rendering
  fw <- image_width
  fh <- image_width * aspect_ratio

  opts_chunk$set(out.width=ow,
                 out.height=oh,
                 fig.width=fw,
                 fig.height=fh)
}


SetFullImageSize <- function() SetImageSize(
    aspect_ratio=base_aspect_ratio, image_width=base_image_width)

# Default to a full image.
SetFullImageSize()



# A convenient funciton for extracting only the legend from a ggplot.
# Taken from
# https://tinyurl.com/y8c742p6
GetLegend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Define common colors.
GGColorHue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


GetGraphColors <- function(legend_breaks) {
  stopifnot(length(legend_breaks) <= 4)
  graph_colors <- GGColorHue(4)[1:length(legend_breaks)]
  names(graph_colors) <- legend_breaks
  return(graph_colors)
}



# Define common colors.
GGColorHue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}


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
#   mrplew_label  Alternative legend label for the MrPlew color.  When
#                 supplied, the MrPlew color is mapped to this string instead
#                 of "MrPlew", so the data column must contain this string.
#   ...           Additional arguments forwarded to scale_*_manual()
#                 (e.g. name=, breaks=, labels=).
#
# To change a core level name, edit the `core_levels` vector below.
MethodColorScale <- function(extra_levels = NULL, extra_colors = NULL,
                             aesthetic = c("color", "fill"),
                             mrplew_label = NULL, ...) {
    aesthetic <- match.arg(aesthetic)

    # Four evenly-spaced hues (90° apart); core levels use positions 1 and 3.
    all_hues <- seq(15, 375, length.out = 5)[1:4]

    core_levels <- c("MrPlew", "Raking")
    core_colors <- hcl(h = all_hues[c(1, 3)], c = 100, l = 65)
    if (!is.null(mrplew_label)) {
        core_levels[1] <- mrplew_label
    }
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
        scale_fill_manual(values = all_colors, ...)
    } else {
        scale_color_manual(values = all_colors, name="Method", ...)
    }
}