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

  NULL->x->value->scenario->param->.->vDim-> include_class -> region -> aggregate ->
    xmax -> fill -> pattern -> xmin


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
    vertical_dim <- "vDim"
    data_diff_full <- data_diff %>%
      dplyr::mutate(vDim = "vDim")
    data_agg_full <- data_agg %>%
      dplyr::mutate(vDim = "vDim")
  }

  # if the vertical dimension is given, duplicate the column and call it "vDim"
  else{
    data_diff_full <- data_diff
    data_diff_full[["vDim"]] <- data_diff_full[[vertical_dim]]

    data_agg_full <- data_agg
    data_agg_full[["vDim"]] <- data_agg_full[[vertical_dim]]
  }

  # summarise in case there are extra columns
  data_diff_full <- data_diff_full %>%
    dplyr::group_by(scenario, region, param, class, x, aggregate, vDim) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  data_agg_full <- data_agg_full %>%
    dplyr::group_by(scenario, region, param, class, x, aggregate, vDim) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()


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

    # ..........................................................................
    ## Subset the data =========================================================
    # ..........................................................................


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


    # fill in implicitly missing class-scenario-vDim combinations with 0
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
      combined_classes <- list()
      combined_yvals <- list()
      combined_all_ends <- list()
      combined_all_starts <- list()
      combined_class_starts <- list()
      combined_diffTotals <- list()
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
                      ncol = length(unique(data_diff_param_subset$class))+2)


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

        values[v,] <- c(refTotals[v], diffRects$value, diffTotals[v])

      } #end vDim


      #.........................................................................
      ### ***rectangle positions ###############################################
      #.........................................................................

      # get all classes and values in order (excluding the rightmost rectangle)
      classes <- factor(c(scenRef, sort(unique(data_diff_param_subset$class)), scenDiff[j]),
                        levels = c(scenRef, sort(unique(data_diff_param_subset$class)), scenDiff[j]))

      # calculate the starts and ends of the whole class rectangles (sum of all vDim)
      class_starts <- c(0, cumsum(colSums(values[,1:(ncol(values)-2), drop = F])), 0)

      # initiate matrix of starts of all rectangles, including scenario totals
      # (columns are scenarios/ classes and rows are vDim)
      all_starts <- matrix(ncol = length(unique(data_diff_param_subset$class))+2,
                           nrow = length(unique(data_diff_param_subset$vDim)))


      # each rectangle starts at the end of the most recently added
      # rectangle that goes in the same direction (positive or negative value),
      # or starts at the class start if it's the first of its direction/ sign
      for(k in 1:(length(unique(data_diff_param_subset$class))+2)){
        class_values <- values[,k]
        for(v in 1:length(unique(data_diff_param_subset$vDim))){
          # get indices of class_values that share the direction of the current value
          direction_match <- which(class_values*class_values[v] > 0)
          # only consider class_values whose starts have already been set
          prev_direction_match <- direction_match[direction_match < v]

          # if there are none, then start at the class start
          if(length(prev_direction_match) == 0){
            all_starts[v,k] = class_starts[k]
          }
          # if there is at least one, choose the most recent one
          else{
            q <- max(prev_direction_match)
            all_starts[v,k] = all_starts[q,k] + values[q,k]
          }
        }
      }

      # collapse into a single vector
      all_starts_vec <- c(all_starts)


      # calculate ends of all rectangles (starts + values)
      all_ends <- all_starts + values
      # collapse into a single vector
      all_ends_vec <- c(all_ends)


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
        ymax = max(c(all_starts_vec, all_ends_vec))
        yvals <- seq(from = min(c(all_starts_vec, all_ends_vec)),
                        to = max(c(all_starts_vec, all_ends_vec)),
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
            ymin[box_count] <- min(all_starts[v,k], all_ends[v,k])
            ymax[box_count] <- max(all_starts[v,k], all_ends[v,k])
            box_count <- box_count + 1
          }
        }


        # initiate plot and add boxes
        p <- ggplot2::ggplot(
          # initialize the data
          data.frame(# in each class (x position), there will be one box for each level of the vertical dimension
                     xmin = rep(1:length(classes) - rect_width/2, each = length(unique(data_diff_param_subset$vDim))),
                     xmax = rep(1:length(classes) + rect_width/2, each = length(unique(data_diff_param_subset$vDim))),
                     # use the y positions calculated earlier
                     ymin = ymin, ymax = ymax,
                     # the fills correspond with the classes (x positions)
                     fill = rep(classes, each = length(unique(data_diff_param_subset$vDim))),
                     # patterns correspond to vertical dimension (one of each pattern per class/ x position)
                     pattern = as.factor(rep(unique(data_diff_param_subset$vDim),
                                             length(classes)))
                     )) +
          # add the boxes with their patterns and fills from the data created above
          ggpattern::geom_rect_pattern(
            ggplot2::aes(
              xmin = xmin, xmax = xmax,
              ymin = ymin, ymax = ymax,
              fill = fill, pattern = pattern
            ),
            # set the color and spacing of the pattern
            color = "black", pattern_fill = "black", pattern_spacing = 0.02, pattern_angle = 45) +
          # assign the fill colors based on the pre-determined palette for classes/ scenarios
          ggplot2::scale_fill_manual(values = rep(fill_colors[as.character(classes)],
                                                  each = length(unique(data_diff_param_subset$vDim))),
                                     # don't include fill legend since classes are labeled on x-axis
                                     guide = "none") +
          # add a line at y = 0
          ggplot2::geom_hline(yintercept = 0) +
          # add the default theme
          theme_default

        # assign the patterns to boxes according to vertical dimensions
        # if there are two vertical dimensions, default to striped and plain for the patterns
        if(length(unique(data_diff_param_subset$vDim)) == 2){
          p <- p + ggpattern::scale_pattern_manual(values = c("none", "stripe"),
                                                              # don't include a fill color in the pattern legend
                                                              guide = ggplot2::guide_legend(override.aes = list(fill = NA)),
                                                              name = "")
          # if there is only one vertical dimension, don't add any patterns
        } else if(length(unique(data_diff_param_subset$vDim)) == 1){
          p <- p + ggpattern::scale_pattern_manual(values = c("none"),
                                                   guide = "none")
          # if there are more than 2 vertical dimensions, use ggpattern's default patterns
        } else{
          p <- p + ggpattern::scale_pattern_discrete(guide = ggplot2::guide_legend(override.aes = list(fill = NA)),
                                                     name = "")
        }


        # add horizontal lines connecting boxes if desired

        if(horizontal_lines){
          # for each class after the refScen and before the diffScen,
          # add a line from the starting height and left edge of the rectangle
          # to the right edge of the previous rectangle
          for (k in 2:(length(classes) - 1)){
            p <- p + ggplot2::annotate("segment",
                                       x = k - rect_width/2,
                                       xend = k - 1 + rect_width/2,
                                       y = class_starts[k],
                                       yend = class_starts[k],
                                       lty = lty)
          }
          # also need a line at the height of the diffScen rectangle, from the
          # left edge of the diffScen to the right edge of the previous rectangle
          k <- length(classes)
          p <- p + ggplot2::annotate("segment",
                                     x = k - rect_width/2,
                                     xend = k - 1 + rect_width/2,
                                     y = sum(diffTotals),
                                     yend = sum(diffTotals),
                                     lty = lty)
        } # end horizontal lines



        # add axis labels and other theme elements
        p <- p + ggplot2::ylab(paste0(unique(data_diff$param)[i], "_", wf_x)) +
          ggplot2::xlab("") +
          ggplot2::theme_bw() +
          # add x labels
          ggplot2::scale_x_discrete(limits = classes, labels = classes) +
          # angle x axis labels
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                         legend.position = "right")

        # add custom theme if given
        if(!is.null(theme)){p <- p + theme}

        # add the finished plot to the output plot list
        plist[[count]] <- p
        count <- count + 1

      } # end plot non-single chart

      else{

        # ......................................................................
        ### Prep for next scenario if single chart #############################
        # ......................................................................

        # if single chart, keep track of all data needed for the diff scenario
        combined_classes[[j]] <- classes
        combined_yvals[[j]] <- yvals
        combined_all_starts[[j]] <- all_starts
        combined_all_ends[[j]] <- all_ends
        combined_class_starts[[j]] <- class_starts
        combined_diffTotals[[j]] <- sum(diffTotals)

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
      classes_combined_vec <- unlist(combined_classes)

      # but only keep one of each "in between" scenario;
      # for any scenario bar other than the first and last, remove the subsequent (repeated) bar
      classes_keep <- 1:length(classes_combined_vec)
      classes_mid <- vector()
      c <- 2
      while(c < length(classes_keep)){
        if(classes_combined_vec[classes_keep[c]] %in% scenDiff){
          classes_keep <- classes_keep[-(c+1)]
          classes_mid <- append(classes_mid, c)
        }
        c <- c+1
      }

      # classes that will be included in the chart
      classes_combined_chart <- classes_combined_vec[classes_keep]

      # unique identifiers for all x axis labels
      # (this is needed to allow repeated labels)
      classes_id <- paste0(classes_combined_vec, "_", 1:length(classes_combined_vec))[classes_keep]

      # combine all other relevant plotting data from the multiple diff scenarios
      yvals_combined_vec <- unlist(combined_yvals)
      prevVals_combined_mat <- do.call(cbind, combined_all_ends)
      currVals_combined_mat <- do.call(cbind, combined_all_starts)
      prevValTotals_combined_vec <- unlist(combined_class_starts)
      scenDiffs_combined_vec <- unlist(combined_diffTotals)


      # get ymin and ymax of each rectangle
      ymin <- vector()
      ymax <- vector()
      box_fill <- vector()
      box_count <- 1
      for(k in classes_keep){
        for(v in 1:length(unique(data_diff_param$vDim))){
          ymin[box_count] <- min(prevVals_combined_mat[v,k], currVals_combined_mat[v,k])
          ymax[box_count] <- max(prevVals_combined_mat[v,k], currVals_combined_mat[v,k])
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
                     pattern = as.factor(rep(unique(data_diff_param$vDim),
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
                                                each = length(unique(data_diff_param$vDim))),
                                   # don't include fill legend since classes are labeled on x-axis
                                   guide = "none") +
        # add a line at y = 0
        ggplot2::geom_hline(yintercept = 0) +
        # add the default theme
        theme_default

      # assign the patterns to boxes according to vertical dimensions
      # if there are two vertical dimensions, default to striped and plain for the patterns
      if(length(unique(data_diff_param$vDim)) == 2){
        p <- p + ggpattern::scale_pattern_manual(values = c("none", "stripe"),
                                                 # don't include a fill color in the pattern legend
                                                 guide = ggplot2::guide_legend(override.aes = list(fill = NA)),
                                                 name = "")
        # if there is only one vertical dimension, don't add any patterns
      } else if(length(unique(data_diff_param$vDim)) == 1){
        p <- p + ggpattern::scale_pattern_manual(values = c("none"),
                                                 # don't include in legend
                                                 guide = "none")
        # if there are more than 2 vertical dimensions, use ggpattern's default patterns
      } else{
        p <- p + ggpattern::scale_pattern_continuous(guide = ggplot2::guide_legend(override.aes = list(fill = NA)),
                                                     name = "")
      }

      # add horizontal line connectors between boxes
      pos_x <- 1
      for(k in classes_keep[-length(classes_keep)]){
        # add lines connecting rectangles
        if(horizontal_lines && k > 1){
          p <- p + ggplot2::annotate("segment",
                                     x = pos_x - rect_width/2,
                                     xend = pos_x - 1 + rect_width/2,
                                     y = prevValTotals_combined_vec[k],
                                     yend = prevValTotals_combined_vec[k],
                                     lty = lty)
        }
        # move to the next x position
        pos_x <- pos_x + 1
      }



      if(horizontal_lines){
        # need to add a line from the end of each diff scen to the left
        diffScen_heights <- c(prevValTotals_combined_vec[classes_mid + 2], prevVals_combined_mat[length(prevVals_combined_mat)])

        p <- p + ggplot2::annotate("segment",
                                   x = c(classes_mid, length(classes_keep)) - rect_width/2,
                                   xend = c(classes_mid, length(classes_keep)) - 1 + rect_width/2,
                                   y = scenDiffs_combined_vec,
                                   yend = scenDiffs_combined_vec,
                                   lty = lty)
      }


      # finish formatting the chart
      p <- p + ggplot2::ylab(paste0(unique(data_diff$param)[i], "_", wf_x)) +
        # remove the x label
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        # angle the x axis labels 45 degrees
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                       legend.position = "right") +
        # add a special x scale to make sure repeated x labels are included
        ggplot2::scale_x_discrete(limits = classes_id, labels = function(x) classes_combined_vec[classes_keep][match(x, classes_id)]) +


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








