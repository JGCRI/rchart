#' plot_class_waterfall
#'
#' generate chart plot for absolute difference and percent difference
#' @param data_diff Default = NULL.
#' @param data_agg Default = NULL.
#' @param scenRef Default = NULL.
#' @param scenDiff Default = NULL.
#' @param theme Default = NULL
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param diff_text Default = NULL. Text to remove from diff scenario names.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param diff_type Default = "bar". Choose between "bar" or "line"
#' @param size Default = 1.5. Line size
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @param include_points Default = FALSE. Add data points to all line charts.
#' @param summary_line Default = FALSE. Add parameter summary line to all bar charts.
#' @param wf_x Default = NULL. Year (or x value) for which to make the waterfall plot.
#' @param rect_width Default = 0.7. Width of the rectangles in the waterfall plot.
#' @param aspect_ratio Default = 0.6. Aspect ratio of the plot. Default is lower (more horizontal) than other rchart default
#' @param horizontal_lines Default = TRUE. Whether to include horizontal lines between rectangles
#' @param lty Default = 2. Line type for the horizontal lines between rectangles
#' @param fill_colors Default = NULL. Vector of colors for rectangles. If not specified, uses jgcricolors corresponding to classes
#' @param totals_fill_color Default = "gray90". Color of the param total bars for the ref and diff scenarios
#' @param palette Default = NULL. Named vector with custom palette colors (can include classes, regions, and/or scenarios; class colors will be used if provided)
#' @importFrom magrittr %>%
#' @export

plot_class_waterfall <- function(data_diff = NULL,
                                  data_agg = NULL,
                                  scenRef = NULL,
                                  scenDiff = NULL,
                                  theme = NULL,
                                  theme_default = ggplot2::theme_bw(),
                                  diff_text = NULL,
                                  scales = "free",
                                  diff_type = "bar",
                                  size = 1.5,
                                  break_interval = NULL,
                                  include_points = FALSE,
                                  summary_line = FALSE,
                                  wf_x = NULL,
                                  rect_width = 0.7,
                                  aspect_ratio = 0.6,
                                  horizontal_lines = TRUE,
                                  lty = 2,
                                  fill_colors = NULL,
                                  totals_fill_color = "gray90",
                                  palette = NULL){

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario->param

  # subset diff data to only the year for which we're making the waterfall plot
  data_diff_subset <- data_diff %>%
    dplyr::filter(x == wf_x)


  #...........................
  # Plot
  #...........................

  plist <- list()

  count <- 1

  for(i in 1:length(unique(data_diff$param))){

    # Check Color Palettes ....................................
    palCustom <- palette
    # remove scenarios from custom palette
    palCustom <- palette[!names(palette) %in% c(scenRef, scenDiff)]
    # remove custom palette names from jgcricolors
    jgcricolors_subset <- jgcricolors::jgcricol()$pal_all[!names(jgcricolors::jgcricol()$pal_all) %in% names(palCustom)]
    # get classes not in the custom palette
    missNamesCustom <- unique(data_diff_subset$class)[!unique(data_diff_subset$class) %in% names(palCustom)]
    # get classes not in the custom palette or in jgcricolors
    missNames <- missNamesCustom[!missNamesCustom %in% names(jgcricolors::jgcricol()$pal_all)]
    # get extra colors to use for nonspecified classes
    palAdd <- rep(jgcricolors::jgcricol()$pal_16,1000)


    if (length(missNames) > 0) {
      # assign extra colors to nonspecified classes
      palAdd <- palAdd[1:length(missNames)]
      names(palAdd) <- missNames
      palCharts <- c(palCustom, jgcricolors_subset, palAdd)
    } else{
      palCharts <- c(palCustom, jgcricolors_subset)
    }

    # add colors for ref and diff scenario totals
    palTotal <- rep(totals_fill_color, length(scenDiff) + 1)
    names(palTotal) <- c(scenRef, scenDiff)
    palCharts <- c(palCharts, palTotal)

    for(j in 1:length(scenDiff)){
      # subset data to the current parameter
      data_diff_plot <- data_diff_subset %>%
        dplyr::filter(param == unique(data_diff$param)[i])
      data_agg_plot <- data_agg %>%
        dplyr::filter(param == unique(data_diff$param)[i])

      # get the leftmost rectangle (total of reference case)
      refTotal <- dplyr::filter(data_agg_plot, x == wf_x,
                                scenario == scenRef)[["value"]]

      # get values for the rest of the rectangle (diffs)
      diffRects <- data_diff_plot %>%
        dplyr::filter(scenario == paste0(scenDiff[j], "_", diff_text, "_", scenRef))

      # get all classes and values in order (excluding the rightmost rectangle)
      classes <- factor(c(scenRef, diffRects$class, scenDiff[j]),
                        levels = c(scenRef, diffRects$class, scenDiff[j]))
      values <- c(0, diffRects$value)

      # calculate the cumulative sum of the values (these correspond to the
      # tops and bottoms of the diff rectangles)
      currentVal <- c(refTotal + cumsum(values), 0)
      prevVal <- c(0, refTotal + cumsum(values))


      # set fill colors
      if(is.null(fill_colors)){
        fill_colors <- palCharts
      }

      # initiate plot
      p <- ggplot2::ggplot(data.frame(x = classes,
                                      y = seq(from = min(c(currentVal, prevVal)),
                                              to = max(c(currentVal, prevVal)),
                                              length.out = length(classes))),
                           ggplot2::aes_string(x = "x", y = "y")) +
        ggplot2::geom_blank() +
        ggplot2::geom_hline(yintercept = 0) +
        theme_default

      for(k in 1:length(classes)){
        # add rectangles
        p <- p + ggplot2::annotate("rect",
                                   xmin = k - rect_width/2,
                                   xmax = k + rect_width/2,
                                   ymin = min(currentVal[k], prevVal[k]),
                                   ymax = max(currentVal[k], prevVal[k]),
                                   fill = fill_colors[as.character(classes[k])],
                                   color = "black")

        # add lines connecting rectangles
        if(horizontal_lines && k > 1){
          p <- p + ggplot2::annotate("segment",
                                     x = k - rect_width/2,
                                     xend = k - 1 + rect_width/2,
                                     y = prevVal[k],
                                     yend = prevVal[k],
                                     lty = lty)
        }
      }

      p <- p + ggplot2::ylab(paste0(unique(data_diff$param)[i], "_", wf_x)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(aspect.ratio = aspect_ratio)

      if(!is.null(theme)){p <- p + theme}

      plist[[count]] <- p
      count <- count + 1
    } # end scenDiff
  } # end param

  # return just the single plot if only one parameter and diff scenario
  if(length(unique(data_diff$param)) == 1 & length(scenDiff) == 1){
    invisible(p)
  } else{
    # otherwise, return grid of parameters (cannot be modified later)
    plot_out <- cowplot::plot_grid(plotlist=plist, ncol = length(scenDiff), align = "hv", axis = "lr")
    invisible(plot_out)
  }

}








