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
#' @param ylim Default = NULL. Y-axis limits
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
                                 palette = NULL,
                                 vertical_dim = NULL,
                                 ylim = NULL){

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario->param

  # subset diff data to only the year for which we're making the waterfall plot
  data_diff_subset <- data_diff %>%
    dplyr::filter(x == wf_x)

  # add a dummy column if there's no vertical dimension
  if(is.null(vertical_dim)){
    data_diff_subset <- data_diff_subset %>%
      dplyr::mutate(vDim = "vDim")
    data_agg <- data_agg %>%
      dplyr::mutate(vDim = "vDim")
  } else{
    data_diff_subset <- data_diff_subset %>%
      dplyr::rename(vDim = tidyselect::all_of(vertical_dim))
    data_agg <- data_agg %>%
      dplyr::rename(vDim = tidyselect::all_of(vertical_dim))
  }



  #...........................
  # Plot
  #...........................

  plist <- list()

  count <- 1

  for(i in 1:length(unique(data_diff$param))){
    # filter to single parameter
    data_diff_param <- data_diff_subset %>%
      dplyr::filter(param == unique(data_diff$param)[i])
    data_agg_param <- data_agg %>%
      dplyr::filter(param == unique(data_diff$param)[i])

    # give value of zero to class and vDim combinations that don't exist
    all_classes <- unique(data_diff_param$class)
    data_diff_param <- data_diff_param %>%
      tidyr::pivot_wider(names_from = "class", values_from = "value") %>%
      replace(is.na(.), 0) %>%
      tidyr::pivot_longer(all_of(all_classes), names_to = "class", values_to = "value")


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
      # initiate lists of refTotals, diffRects, diffTotals
      refTotals = c()
      #diffRects = list()
      diffTotals = c()
      values = matrix(nrow = length(unique(data_diff_param$vDim)),
                      ncol = length(unique(data_diff_param$class))+1)

      for(v in 1:length(unique(data_diff_param$vDim))){
        # subset data to the current parameter and vertical dimension
        data_diff_plot <- data_diff_param %>%
          dplyr::filter(vDim == unique(data_diff_param$vDim)[v]) %>%
          dplyr::arrange(class)
        data_agg_plot <- data_agg_param %>%
          dplyr::filter(vDim == unique(data_diff_param$vDim)[v]) %>%
          dplyr::arrange(class)

        # get the leftmost rectangle (total of reference case)
        refTotals[v] <- dplyr::filter(data_agg_plot, x == wf_x,
                                  scenario == scenRef)[["value"]]
        diffTotals[v] <- dplyr::filter(data_agg_plot, x == wf_x,
                                        scenario == scenDiff[j])[["value"]]

        # get values for the rest of the rectangle (diffs)
        diffRects <- data_diff_plot %>%
          dplyr::filter(scenario == paste0(scenDiff[j], "_", diff_text, "_", scenRef))

        values[v,] <- c(0, diffRects$value)

      }

      # get all classes and values in order (excluding the rightmost rectangle)
      classes <- factor(c(scenRef, sort(unique(data_diff_param$class)), scenDiff[j]),
                        levels = c(scenRef, sort(unique(data_diff_param$class)), scenDiff[j]))

      # calculate the cumulative sum of the values (these correspond to the
      # tops and bottoms of the diff rectangles)
      currentValTotal <- c(sum(refTotals) + cumsum(colSums(values)), 0)
      prevValTotal <- c(0, sum(refTotals) + cumsum(colSums(values)))
      # calculate the heights of the stacked rectangles
      midVals <- matrix(ncol = length(unique(data_diff_param$class))+2,
                        nrow = length(unique(data_diff_param$vDim)))
      midVals[,1] <- cumsum(refTotals)
      for(c in 2:(length(unique(data_diff_param$class))+1)){
        midVals[,c] <- prevValTotal[c] + cumsum(values[,c])
      }
      midVals[,ncol(midVals)] <- sum(diffTotals) -cumsum(diffTotals)

      currentVal <- midVals
      prevVal <- rbind(prevValTotal, midVals[1:(nrow(midVals)-1),])

      # set fill colors
      if(is.null(fill_colors)){
        fill_colors <- palCharts
      }

      # get y limits
      if(!is.null(ylim)){
        ymax <- ylim[2]
        yvals <- seq(from = ylim[1], to = ylim[2],
                     length.out = length(classes))
      }
      else{
        ymax = max(c(currentVal, prevVal))
        yvals <- seq(from = min(c(currentVal, prevVal)),
                        to = max(c(currentVal, prevVal)),
                        length.out = length(classes))
      }

      # initiate plot
      p <- ggplot2::ggplot(data.frame(x = classes,
                                      y = yvals),
                           ggplot2::aes_string(x = "x", y = "y")) +
        ggplot2::geom_blank() +
        ggplot2::geom_hline(yintercept = 0) +
        theme_default

      # amounts to darken each class color for each vertical dim
      darken <- seq(from = 0, to = 0.4, length.out = length(unique(data_diff_param$vDim)))

      for(k in 1:length(classes)){
        for(v in 1:length(unique(data_diff_param$vDim))){
          # add rectangles
          p <- p + ggplot2::annotate("rect",
                                     xmin = k - rect_width/2,
                                     xmax = k + rect_width/2,
                                     ymin = min(prevVal[v,k], currentVal[v,k]),
                                     ymax = max(prevVal[v,k], currentVal[v,k]),
                                     fill = colorspace::darken(
                                       fill_colors[as.character(classes[k])],
                                       amount = darken[v]),
                                     color = "black")
        }


        # add lines connecting rectangles
        if(horizontal_lines && k > 1){
          p <- p + ggplot2::annotate("segment",
                                     x = k - rect_width/2,
                                     xend = k - 1 + rect_width/2,
                                     y = prevValTotal[k],
                                     yend = prevValTotal[k],
                                     lty = lty)
        }
      }


      p <- p + ggplot2::ylab(paste0(unique(data_diff$param)[i], "_", wf_x)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        #ggplot2::theme(aspect.ratio = aspect_ratio) +
        # angle x axis labels
        #ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
        ggplot2::theme(axis.text.x=ggplot2::element_blank()) +
        ggplot2::coord_fixed(ratio = 1/ymax*3)

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








