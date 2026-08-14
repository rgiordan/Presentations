

# A convenient funciton for extracting only the legend from a ggplot.
# Taken from
# https://tinyurl.com/y8c742p6
GetLegend <- function(myggplot){
  warning("Use ggpubr::get_legend instead")
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


GetMethodLabels <- function() {
    method_breaks <- c("ij", "boot", "bayes", "sim")
    method_labels <- c(
        "IJ",
        "Boot",
        "Bayes",
        "Sim")
    names(method_labels) <- method_breaks
    return(list(breaks=method_breaks, labels=method_labels))
}


GetMethodScale <- function(type="color") {
    valid_types <- c("color")
    if (!(type %in% valid_types)) {
        stop(sprintf("%s is not a valid legend type for GetMethodScale", type))
    }
    method_labels <- GetMethodLabels()

    if (type == "color") {
        colors <- GGColorHue(length(method_labels$breaks))
        color_scale <-
            scale_color_manual(
                name="Method",
                values=colors,
                labels=method_labels$labels,
                aesthetics="color",
                drop=TRUE,
                limits = force)
        return(color_scale)
    } else {
        stop("This should never happen")
    }
}



VarianceCompPlot <- function(df, var, var_se) {
    method_labels <- GetMethodLabels()
    df %>%
        ggplot(aes(x=method, color=method)) +
        geom_point(aes(y={{var}})) +
        geom_errorbar(aes(
            ymin={{var}} - 2 * {{var_se}},
            ymax={{var}} + 2 * {{var_se}}), width=0.2) +
        GetMethodScale() +
        theme(axis.title.x = element_blank()) +
        scale_x_discrete(breaks=method_labels$breaks,
                         labels=method_labels$labels)
}
