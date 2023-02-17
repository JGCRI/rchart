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
#' @param vertical_dim Default = NULL. Column to use for vertical stacking within classes
#' @param single_chart Default = F. If there are multiple diff scenarios, include them all in the same chart?
#' @param scen_order Default = F. Order of scenarios to chart if single chart option is selected
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
                                 ylim = NULL,
                                 single_chart = F,
                                 scen_order = NULL){

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario->param->.->vDim-> include_class


  # reorder the diff scens if needed
  if(!is.null(scen_order)){
    if(scenRef %in% scen_order){
      stop("scenRef is included in scen_order. Please only include difference scenarios.")
    } else if(any(!scen_order %in% scenDiff)){
      stop("One or more scenarios included in waterfall_scen_order is not present in the data.")
    }
    scenDiff <- scen_order
  }


  # add a dummy column if there's no vertical dimension
  if(is.null(vertical_dim)){
    data_diff_full <- data_diff %>%
      dplyr::mutate(vDim = "vDim")
    data_agg_full <- data_agg %>%
      dplyr::mutate(vDim = "vDim")
  } else{
    data_diff_full <- data_diff %>%
      dplyr::rename(vDim = tidyselect::all_of(vertical_dim))
    data_agg_full <- data_agg %>%
      dplyr::rename(vDim = tidyselect::all_of(vertical_dim))
  }



  #...........................
  # Plot
  #...........................

  plist <- list()

  count <- 1

  origScenRef <- scenRef

  for(i in 1:length(unique(data_diff$param))){
    # filter to single parameter
    data_diff_param_all <- data_diff_full %>%
      dplyr::filter(param == unique(data_diff_full$param)[i])
    data_agg_param_all <- data_agg_full %>%
      dplyr::filter(param == unique(data_diff_full$param)[i])

    # set the year for the waterfall chart
    if(!is.null(wf_x)){
      if(!wf_x %in% unique(data_diff_param_all$x)){
        print(paste0("waterfall_x value provided is not present in data for param ",
                     unique(data_diff$param)[i], ". Using ",
                     max(data_diff_param_all$x), " instead."))
        wf_x <- max(data_diff_param_all$x)
      }
    } else{wf_x <- max(data_diff_param_all$x)}

    # filter data to only include selected year
    data_diff_param <- data_diff_param_all %>%
      dplyr::filter(x == wf_x)
    data_agg_param <- data_agg_param_all %>%
      dplyr::filter(x == wf_x)

    # # give value of zero to class-scenario-vDim combinations that don't exist
    all_classes <- unique(data_diff_param$class)
    data_diff_param <- data_diff_param %>%
      tidyr::pivot_wider(names_from = "class", values_from = "value") %>%
      replace(is.na(.), 0) %>%
      tidyr::pivot_longer(tidyselect::all_of(all_classes), names_to = "class", values_to = "value")


    # Check Color Palettes ....................................
    palCustom <- palette
    # remove scenarios from custom palette
    palCustom <- palette[!names(palette) %in% c(scenRef, scenDiff)]
    # remove custom palette names from jgcricolors
    jgcricolors_subset <- jgcricolors::jgcricol()$pal_all[!names(jgcricolors::jgcricol()$pal_all) %in% names(palCustom)]
    # get classes not in the custom palette
    missNamesCustom <- unique(data_diff_full$class)[!unique(data_diff_full$class) %in% names(palCustom)]
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

    # initiate plotting data if scenarios will be combined in one plot
    if(single_chart){
      allClasses <- list()
      allYVals <- list()
      allPrevVals <- list()
      allCurrentVals <- list()
      allPrevValTotals <- list()
    }

    # start with the original scenRef
    scenRef <- origScenRef
    for(j in 1:length(scenDiff)){
      # subset data to only include classes with nonzero value for at least one vDim
      data_diff_param_subset <- data_diff_param %>%
        dplyr::filter(scenario %in% c(scenRef, scenDiff[j], paste0(scenDiff[j], "_", diff_text, "_", scenRef))) %>%
        dplyr::group_by(dplyr::across(-c(value, vDim, scenario))) %>%
        dplyr::mutate(include_class = any(value !=0)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(include_class) %>%
        dplyr::select(-include_class)


      # initiate lists of refTotals, diffRects, diffTotals
      # refTotals are the heights of the ref scenario bars for each vdim
      refTotals = c()
      # diffTotals are the heights of the diff scenario bars for each vdim
      diffTotals = c()
      # values are the heights of each diff rectangle
      values = matrix(nrow = length(unique(data_diff_param_subset$vDim)),
                      ncol = length(unique(data_diff_param_subset$class))+1)


      for(v in 1:length(unique(data_diff_param_subset$vDim))){
        # subset data to the current vertical dimension
        data_diff_plot <- data_diff_param_subset %>%
          dplyr::filter(vDim == unique(data_diff_param_subset$vDim)[v]) %>%
          dplyr::arrange(class)
        data_agg_plot <- data_agg_param %>%
          dplyr::filter(vDim == unique(data_diff_param_subset$vDim)[v]) %>%
          dplyr::arrange(class)

        # get the leftmost rectangle (total of reference case)
        refTotals[v] <- dplyr::filter(data_agg_plot,
                                      scenario == scenRef)[["value"]]
        diffTotals[v] <- dplyr::filter(data_agg_plot,
                                        scenario == scenDiff[j])[["value"]]

        # get values for the rest of the rectangle (diffs)
        diffRects <- data_diff_plot %>%
          dplyr::filter(scenario == paste0(scenDiff[j], "_", diff_text, "_", scenRef))

        values[v,] <- c(0, diffRects$value)

      }



      # get all classes and values in order (excluding the rightmost rectangle)
      classes <- factor(c(scenRef, sort(unique(data_diff_param_subset$class)), scenDiff[j]),
                        levels = c(scenRef, sort(unique(data_diff_param_subset$class)), scenDiff[j]))

      # calculate the cumulative sum of the values (these correspond to the
      # tops and bottoms of the diff rectangles)
      currentValTotal <- c(sum(refTotals) + cumsum(colSums(values)), 0)
      prevValTotal <- c(0, sum(refTotals) + cumsum(colSums(values)))
      # calculate the heights of the stacked rectangles
      midVals <- matrix(ncol = length(unique(data_diff_param_subset$class))+2,
                        nrow = length(unique(data_diff_param_subset$vDim)))
      midVals[,1] <- cumsum(refTotals)
      for(c in 2:(length(unique(data_diff_param_subset$class))+1)){
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
      }else{
        ymax = max(c(currentVal, prevVal))
        yvals <- seq(from = min(c(currentVal, prevVal)),
                        to = max(c(currentVal, prevVal)),
                        length.out = length(classes))
      }

      if(!single_chart){

        # initiate plot
        p <- ggplot2::ggplot(data.frame(x = classes,
                                        y = yvals),
                             ggplot2::aes_string(x = "x", y = "y")) +
          ggplot2::geom_blank() +
          ggplot2::geom_hline(yintercept = 0) +
          theme_default

        # amounts to darken each class color for each vertical dim
        darken <- seq(from = 0, to = 0.4, length.out = length(unique(data_diff_param_subset$vDim)))

        for(k in 1:length(classes)){
          for(v in 1:length(unique(data_diff_param_subset$vDim))){
            # if there are only 2 vdim classes, use stripes for one of them
            if(length(unique(data_diff_param_subset$vDim)) == 2){
              if(v == 1){
                # striped portion
                p <- p + ggplot2::annotate("rect",
                                           xmin = k - rect_width/2,
                                           xmax = k + rect_width/2,
                                           ymin = min(prevVal[v,k], currentVal[v,k]),
                                           ymax = max(prevVal[v,k], currentVal[v,k]),
                                           fill = fill_colors[as.character(classes[k])],
                                           color = "black")
                #TODO: make stripes diagonal
                p <- p + ggplot2::annotate("segment",
                                           x = seq(k - rect_width/2, k + rect_width/2, length.out = 6),
                                           xend = seq(k - rect_width/2, k + rect_width/2, length.out = 6),
                                           y = rep(min(prevVal[v,k], currentVal[v,k]), 6),
                                           yend = rep(max(prevVal[v,k], currentVal[v,k]),6),
                                           color = "black")

              } else{
                # non-striped portion
                p <- p + ggplot2::annotate("rect",
                                           xmin = k - rect_width/2,
                                           xmax = k + rect_width/2,
                                           ymin = min(prevVal[v,k], currentVal[v,k]),
                                           ymax = max(prevVal[v,k], currentVal[v,k]),
                                           fill = fill_colors[as.character(classes[k])],
                                           color = "black")
              }
            } else{
              # if more than 2 vdim classes, use various shades of the original color
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
        }


        p <- p + ggplot2::ylab(paste0(unique(data_diff$param)[i], "_", wf_x)) +
          ggplot2::xlab("") +
          ggplot2::theme_bw() +
          ggplot2::theme(aspect.ratio = aspect_ratio) +
          # angle x axis labels
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
          ggplot2::coord_fixed(ratio = 1/ymax*3)

        if(!is.null(theme)){p <- p + theme}

        plist[[count]] <- p
        count <- count + 1
      } else{
        # if single chart, keep track of all data needed for the diff scenario
        allClasses[[j]] <- classes
        allYVals[[j]] <- yvals
        allCurrentVals[[j]] <- currentVal
        allPrevVals[[j]] <- prevVal
        allPrevValTotals[[j]] <- prevValTotal

        # for next round, need current class as reference scenario
        # e.g., need scen3_diffAbs_scen2 which is equal to
        # (scen3_diffAbs_scen1) - (scen2_diffAbs_scen1)
        if(j < length(scenDiff)){
          new_diff_scenario <- paste0(scenDiff[j+1], "_", diff_text, "_", scenDiff[j])
          data_diff_param <- data_diff_param %>%
            dplyr::filter(grepl(diff_text, scenario)) %>%
            tidyr::pivot_wider(names_from = scenario, values_from = value) %>%
            dplyr::mutate(!!new_diff_scenario := get(paste0(scenDiff[j+1], "_", diff_text, "_", scenRef)) - get(paste0(scenDiff[j], "_", diff_text, "_", scenRef))) %>%
            tidyr::pivot_longer(cols = tidyr::contains(diff_text), names_to = "scenario", values_to = "value")

          scenRef <- scenDiff[j]
        }
      }
    } # end scenDiff

    # if single chart option is selected, aggregate data into one chart
    if(single_chart){

      # combine the classes from each scenario
      classes_combined <- unlist(allClasses)

      # but only keep one of each "in between" scenario;
      # for any scenario bar other than the first and last, remove the subsequent (repeated) bar
      classes_keep <- 1:length(classes_combined)
      c <- 2
      while(c < length(classes_keep)){
        if(classes_combined[classes_keep[c]] %in% scenDiff){
          classes_keep <- classes_keep[-(c+1)]
        }
        c <- c+1
      }

      # classes that will be included in the chart
      classes_combined_chart <- classes_combined[classes_keep]

      # unique identifiers for all x axis labels
      # (this is needed to allow repeated labels)
      classes_id <- paste0(classes_combined, "_", 1:length(classes_combined))[classes_keep]

      # combine all other relevant plotting data from the multiple diff scenarios
      yvals_combined <- unlist(allYVals)
      prevVals_combined <- do.call(cbind, allPrevVals)
      currVals_combined <- do.call(cbind, allCurrentVals)
      prevValTotals_combined <- unlist(allPrevValTotals)

      # initiate plot
      p <- ggplot2::ggplot(data.frame(x = classes_combined_chart,
                                      y = yvals_combined[classes_keep]),
                           ggplot2::aes_string(x = "x", y = "y")) +
        ggplot2::geom_blank() +
        ggplot2::geom_hline(yintercept = 0) +
        theme_default

      # amounts to darken each class color for each vertical dim
      darken <- seq(from = 0, to = 0.4, length.out = length(unique(data_diff_param$vDim)))

      # keep track of the x position of each bar
      pos_x <- 1
      # add bars and line connectors one by one
      for(k in classes_keep){
        for(v in 1:length(unique(data_diff_param$vDim))){
          if(length(unique(data_diff_param$vDim)) == 2){
            if(v == 1){
              # striped portion
              p <- p + ggplot2::annotate("rect",
                                         xmin = pos_x - rect_width/2,
                                         xmax = pos_x + rect_width/2,
                                         ymin = min(prevVals_combined[v,k], currVals_combined[v,k]),
                                         ymax = max(prevVals_combined[v,k], currVals_combined[v,k]),
                                         fill = fill_colors[as.character(classes_combined[k])],
                                         color = "black")
              #TODO: make stripes diagonal
              p <- p + ggplot2::annotate("segment",
                                         x = seq(pos_x - rect_width/2, pos_x + rect_width/2, length.out = 6),
                                         xend = seq(pos_x - rect_width/2, pos_x + rect_width/2, length.out = 6),
                                         y = rep(min(prevVals_combined[v,k], currVals_combined[v,k]), 6),
                                         yend = rep(max(prevVals_combined[v,k], currVals_combined[v,k]),6),
                                         color = "black")
            } else{
              p <- p + ggplot2::annotate("rect",
                                         xmin = pos_x - rect_width/2,
                                         xmax = pos_x + rect_width/2,
                                         ymin = min(prevVals_combined[v,k], currVals_combined[v,k]),
                                         ymax = max(prevVals_combined[v,k], currVals_combined[v,k]),
                                         fill = fill_colors[as.character(classes_combined[k])],
                                         color = "black")
            }
          } else{
            # add rectangles
            p <- p + ggplot2::annotate("rect",
                                       xmin = pos_x - rect_width/2,
                                       xmax = pos_x + rect_width/2,
                                       ymin = min(prevVals_combined[v,k], currVals_combined[v,k]),
                                       ymax = max(prevVals_combined[v,k], currVals_combined[v,k]),
                                       fill = colorspace::darken(
                                         fill_colors[as.character(classes_combined[k])],
                                         amount = darken[v]),
                                       color = "black")
            }
          }


        # add lines connecting rectangles
        if(horizontal_lines && k > 1){
          p <- p + ggplot2::annotate("segment",
                                     x = pos_x - rect_width/2,
                                     xend = pos_x - 1 + rect_width/2,
                                     y = prevValTotals_combined[k],
                                     yend = prevValTotals_combined[k],
                                     lty = lty)
        }
        # move to the next x position
        pos_x <- pos_x + 1
      }


      p <- p + ggplot2::ylab(paste0(unique(data_diff$param)[i], "_", wf_x)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(aspect.ratio = aspect_ratio) +
        # need special x scale to make sure repeated x labels are included
        ggplot2::scale_x_discrete(limits = classes_id, labels = function(x) classes_combined[classes_keep][match(x, classes_id)]) +
        # angle the x axis labels 45 degrees
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
        ggplot2::coord_fixed(ratio = 1/ymax*3)

      if(!is.null(theme)){p <- p + theme}

      plist[[count]] <- p
      count <- count + 1

    }

  } # end param

  # return just the single plot if only one parameter and diff scenario,
  # or one parameter and single_chart option
  if(length(unique(data_diff$param)) == 1 & (length(scenDiff) == 1 | single_chart)){
    invisible(p)
  } else{
    # otherwise, return grid of parameters (cannot be modified later)
    plot_out <- cowplot::plot_grid(plotlist=plist, nrow = length(unique(data_diff$param)), align = "hv", axis = "lr")
    invisible(plot_out)
  }

}








