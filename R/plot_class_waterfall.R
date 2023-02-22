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

  # ............................................................................
  # Initialize data ------------------------------------------------------------
  # ............................................................................

  NULL->x->value->scenario->param->.->vDim-> include_class

  # reorder the diff scenarios if a new order is given
  if(!is.null(scen_order)){
    if(scenRef %in% scen_order){
      # make sure the reference scenario isn't included in the new order
      stop("scenRef is included in scen_order. Please only include difference scenarios.")
    } else if(any(!scen_order %in% scenDiff)){
      # make sure all of the difference scenarios in the new order exist
      stop("One or more scenarios included in waterfall_scen_order is not present in the data.")
    }
    scenDiff <- scen_order
  }


  # add a placeholder vertical dimension column if vertical dimension isn't given
  if(is.null(vertical_dim)){
    data_diff_full <- data_diff %>%
      dplyr::mutate(vDim = "vDim")
    data_agg_full <- data_agg %>%
      dplyr::mutate(vDim = "vDim")
  }
  # if the vertical dimension is given, rename the column to "vDim"
  else{
    data_diff_full <- data_diff %>%
      dplyr::rename(vDim = tidyselect::all_of(vertical_dim))
    data_agg_full <- data_agg %>%
      dplyr::rename(vDim = tidyselect::all_of(vertical_dim))
  }


  # keep track of the reference scenario since "scenRef" could change
  # if creating a single chart with 3+ scenarios
  origScenRef <- scenRef



  # ............................................................................
  # Initialize plot list -------------------------------------------------------
  # ............................................................................

  # start a list of plots
  plist <- list()
  # initialize the plot list index
  count <- 1



  # ............................................................................
  # Loop through parameters ----------------------------------------------------
  # ............................................................................

  for(i in 1:length(unique(data_diff$param))){

    # filter the data to the current parameter
    data_diff_param_all <- data_diff_full %>%
      dplyr::filter(param == unique(data_diff_full$param)[i])

    data_agg_param_all <- data_agg_full %>%
      dplyr::filter(param == unique(data_diff_full$param)[i])


    # set the year for the waterfall chart
    if(!is.null(wf_x)){
      # if the value is provided but doesn't exist in the data, use the largest
      # (latest) value in the data instead
      if(!wf_x %in% unique(data_diff_param_all$x)){
        print(paste0("waterfall_x value provided is not present in data for param ",
                     unique(data_diff$param)[i], ". Using ",
                     max(data_diff_param_all$x), " instead."))
        wf_x <- max(data_diff_param_all$x)
      }
      # if the value is not provided, automatically use the largest (latest)
      # value in the data
    } else{wf_x <- max(data_diff_param_all$x)}


    # filter data to only include selected year
    data_diff_param <- data_diff_param_all %>%
      dplyr::filter(x == wf_x)

    data_agg_param <- data_agg_param_all %>%
      dplyr::filter(x == wf_x)


    # give value of zero to class-scenario-vDim combinations that don't exist
    all_classes <- unique(data_diff_param$class)
    data_diff_param <- data_diff_param %>%
      tidyr::pivot_wider(names_from = "class", values_from = "value") %>%
      replace(is.na(.), 0) %>%
      tidyr::pivot_longer(tidyselect::all_of(all_classes), names_to = "class", values_to = "value")

    # ..........................................................................
    ## Set the color palette ===================================================
    # ..........................................................................

    palCustom <- palette

    # remove scenarios from custom palette
    palCustom <- palette[!names(palette) %in% c(scenRef, scenDiff)]

    # remove custom palette names from jgcricolors
    jgcricolors_subset <- jgcricolors::jgcricol()$pal_all[!names(jgcricolors::jgcricol()$pal_all) %in% names(palCustom)]

    # get classes not in the custom palette (if no palette is given, this will include all classes)
    missNamesCustom <- unique(data_diff_full$class)[!unique(data_diff_full$class) %in% names(palCustom)]

    # get classes not in the custom palette or in jgcricolors
    missNames <- missNamesCustom[!missNamesCustom %in% names(jgcricolors::jgcricol()$pal_all)]

    # get extra colors to use for nonspecified classes
    palAdd <- rep(jgcricolors::jgcricol()$pal_16,1000)


    if (length(missNames) > 0) {
      # assign extra colors to nonspecified classes
      palAdd <- palAdd[1:length(missNames)]
      names(palAdd) <- missNames
      # use custom colors first, then jgcricolors, then extra colors
      palCharts <- c(palCustom, jgcricolors_subset, palAdd)
    } else{
      # use custom colors first, then jgcricolors (no extra colors needed)
      palCharts <- c(palCustom, jgcricolors_subset)
    }

    # add colors for ref and diff scenario totals
    palTotal <- rep(totals_fill_color, length(scenDiff) + 1)
    names(palTotal) <- c(scenRef, scenDiff)
    palCharts <- c(palCharts, palTotal)



    # ..........................................................................
    ## Initiate plotting data lists if single chart ============================
    # ..........................................................................

    if(single_chart){
      allClasses <- list()
      allYVals <- list()
      allPrevVals <- list()
      allCurrentVals <- list()
      allPrevValTotals <- list()
    }



    # ..........................................................................
    ## Loop through scenarios ==================================================
    # ..........................................................................

    # for each new parameter, make sure we're starting with the original scenRef
    scenRef <- origScenRef
    for(j in 1:length(scenDiff)){

      #.........................................................................
      ### Subset the data ######################################################
      #.........................................................................

      data_diff_param_subset <- data_diff_param %>%
        # subset data to only include current scenario
        dplyr::filter(scenario %in% c(scenRef, scenDiff[j], paste0(scenDiff[j], "_", diff_text, "_", scenRef))) %>%
        dplyr::group_by(dplyr::across(-c(value, vDim, scenario))) %>%
        # subset data to only include classes that exist in this scenario
        # (classes with at least one nonzero vDim)
        dplyr::mutate(include_class = any(value !=0)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(include_class) %>%
        dplyr::select(-include_class)


      #.........................................................................
      ### Get chart data #######################################################
      #.........................................................................

      #.........................................................................
      ### ***rectangle heights #################################################
      #.........................................................................

      # initiate lists of refTotals, diffRects, diffTotals

      # refTotals are the heights of the ref scenario bars for each vdim
      refTotals = c()

      # diffTotals are the heights of the diff scenario bars for each vdim
      diffTotals = c()

      # values are the heights of each class rectangle
      values = matrix(nrow = length(unique(data_diff_param_subset$vDim)),
                      ncol = length(unique(data_diff_param_subset$class))+1)


      # get the heights of each rectangle for each vDim

      for(v in 1:length(unique(data_diff_param_subset$vDim))){
        # subset data to the current vertical dimension
        data_diff_plot <- data_diff_param_subset %>%
          dplyr::filter(vDim == sort(unique(data_diff_param_subset$vDim))[v]) %>%
          dplyr::arrange(class)

        data_agg_plot <- data_agg_param %>%
          dplyr::filter(vDim == sort(unique(data_diff_param_subset$vDim))[v]) %>%
          dplyr::arrange(class)

        # get the height of the reference scenario bar
        refTotals[v] <- dplyr::filter(data_agg_plot,
                                      scenario == scenRef)[["value"]]

        # get the height of the difference scenario bar
        diffTotals[v] <- dplyr::filter(data_agg_plot,
                                        scenario == scenDiff[j])[["value"]]

        # get the heights of each class rectangle
        diffRects <- data_diff_plot %>%
          dplyr::filter(scenario == paste0(scenDiff[j], "_", diff_text, "_", scenRef))

        values[v,] <- c(0, diffRects$value)

      } #end vDim


      #.........................................................................
      ### ***rectangle positions ###############################################
      #.........................................................................

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

      # ........................................................................
      ### Plot if not single chart #############################################
      # ........................................................................

      if(!single_chart){

        # get ymin and ymax of each rectangle
        ymin <- vector()
        ymax <- vector()
        box_count <- 1
        for(k in 1:length(classes)){
          for(v in 1:length(unique(data_diff_param_subset$vDim))){
            ymin[box_count] <- min(prevVal[v,k], currentVal[v,k])
            ymax[box_count] <- max(prevVal[v,k], currentVal[v,k])
            box_count <- box_count + 1
          }
        }


        # initiate plot and add boxes
        p <- ggplot2::ggplot(
          # initialize the data. Need one x value and one y value for each
          # individual box (including vertically stacked ones)
          data.frame(# in each x position, there will be one box for each level of the vertical dimension
                     xmin = rep(1:length(classes) - rect_width/2, each = length(unique(data_diff_param_subset$vDim))),
                     xmax = rep(1:length(classes) + rect_width/2, each = length(unique(data_diff_param_subset$vDim))),
                     # use the y positions calculated earlier
                     ymin = ymin, ymax = ymax,
                     # the fills correspond with the classes (x positions)
                     fill = rep(classes, each = length(unique(data_diff_param_subset$vDim))),
                     # patterns correspond to vertical dimension (one of each pattern per class/ x position)
                     pattern = as.factor(rep(1:length(unique(data_diff_param_subset$vDim)),
                                             length(classes)))
                     )) +
          # add the boxes with their patterns and fills
          ggpattern::geom_rect_pattern(
            ggplot2::aes(
              xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, pattern = pattern
            ),
            # pattern appears in black over the fill color
            color = "black", pattern_fill = "black", pattern_spacing = 0.02, pattern_angle = 45) +
          # assign the fill colors based on the pre-determined palette for classes/ scenarios
          ggplot2::scale_fill_manual(values = rep(fill_colors[as.character(classes)],
                                                  each = length(unique(data_diff_param_subset$vDim)))) +
          # add a line at y = 0
          ggplot2::geom_hline(yintercept = 0) +
          # add the default theme
          theme_default

        # assign the patterns to boxes according to vertical dimensions
        # if there are two vertical dimensions, default to striped and plain for the patterns
        if(length(unique(data_diff_param_subset$vDim)) == 2){
          p <- p + ggpattern::scale_pattern_manual(values = c("none", "stripe"))
          # if there is only one vertical dimension, don't add any patterns
        } else if(length(unique(data_diff_param_subset$vDim)) == 1){
          p <- p + ggpattern::scale_pattern_manual(values = c("none"))
          # if there are more than 2 vertical dimensions, use ggpattern's default patterns
        } else{
          p <- p + ggpattern::scale_pattern_discrete()
        }


        # add horizonal lines connecting boxes
        for(k in 1:length(classes)){
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
          ggplot2::theme(aspect.ratio = aspect_ratio,
                         legend.position = "none") +
          # add x labels
          ggplot2::scale_x_discrete(limits = classes, labels = classes) +
          # angle x axis labels
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
          ggplot2::coord_fixed(ratio = 1/ymax*3)

        if(!is.null(theme)){p <- p + theme}

        plist[[count]] <- p
        count <- count + 1
      } else{

        # ......................................................................
        ### Prep for next scenario if single chart #############################
        # ......................................................................

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



    # ..........................................................................
    ## chart aggregate data if single chart ====================================
    # ..........................................................................

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

      # get ymin and ymax of each rectangle
      ymin <- vector()
      ymax <- vector()
      box_fill <- vector()
      box_count <- 1
      for(k in classes_keep){
        for(v in 1:length(unique(data_diff_param$vDim))){
          ymin[box_count] <- min(prevVals_combined[v,k], currVals_combined[v,k])
          ymax[box_count] <- max(prevVals_combined[v,k], currVals_combined[v,k])
          box_fill[box_count] <- v
          box_count <- box_count + 1
        }
      }


      # initiate plot and add boxes
      p <- ggplot2::ggplot(
          # initialize the data. Need one x value and one y value for each
          # individual box (including vertically stacked ones)
          data.frame(# in each x position, there will be one box for each level of the vertical dimension
                     xmin = rep(1:length(classes_id) - rect_width/2, each = length(unique(data_diff_param$vDim))),
                     xmax = rep(1:length(classes_id) + rect_width/2, each = length(unique(data_diff_param$vDim))),
                     # use the y positions calculated earlier
                     ymin = ymin, ymax = ymax,
                     # the fills correspond with the classes (x positions)
                     fill = rep(classes_combined_chart, each = length(unique(data_diff_param$vDim))),
                     # patterns correspond to vertical dimension (one of each pattern per class/ x position)
                     pattern = as.factor(rep(1:length(unique(data_diff_param$vDim)),
                                             length(classes_keep))))) +
        # add the boxes with their patterns and fills
        ggpattern::geom_rect_pattern(
          ggplot2::aes(
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, pattern = pattern
            ),
          # pattern appears in black over the fill color
          color = "black", pattern_fill = "black", pattern_spacing = 0.02, pattern_angle = 45) +
        # assign the fill colors based on the pre-determined palette for classes/ scenarios
        ggplot2::scale_fill_manual(values = rep(fill_colors[as.character(classes_combined_chart)],
                                                each = length(unique(data_diff_param$vDim)))) +
        # add a line at y = 0
        ggplot2::geom_hline(yintercept = 0) +
        # add the default theme
        theme_default

      # assign the patterns to boxes according to vertical dimensions
      # if there are two vertical dimensions, default to striped and plain for the patterns
      if(length(unique(data_diff_param$vDim)) == 2){
        p <- p + ggpattern::scale_pattern_manual(values = c("none", "stripe"))
        # if there is only one vertical dimension, don't add any patterns
      } else if(length(unique(data_diff_param$vDim)) == 1){
        p <- p + ggpattern::scale_pattern_manual(values = c("none"))
        # if there are more than 2 vertical dimensions, use ggpattern's default patterns
      } else{
        p <- p + ggpattern::scale_pattern_continuous()
      }

      # add horizontal line connectors between boxes
      pos_x <- 1
      for(k in classes_keep){
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


      # finish formatting the chart
      p <- p + ggplot2::ylab(paste0(unique(data_diff$param)[i], "_", wf_x)) +
        # remove the x label
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        # fix the aspect ratio
        ggplot2::theme(aspect.ratio = aspect_ratio) +
        # add a special x scale to make sure repeated x labels are included
        ggplot2::scale_x_discrete(limits = classes_id, labels = function(x) classes_combined[classes_keep][match(x, classes_id)]) +
        # angle the x axis labels 45 degrees
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                       legend.position = "none") +
        # fix the coordinates for aspect ratio
        ggplot2::coord_fixed(ratio = 1/ymax*3)

      if(!is.null(theme)){p <- p + theme}

      plist[[count]] <- p
      count <- count + 1

    }

  } # end param



  # ............................................................................
  # Combine and return charts --------------------------------------------------
  # ............................................................................

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








