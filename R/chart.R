#' chart
#'
#' Used to add missing data to input files and customize format
#' @param data Default = NULL. Dataframe to test and convert.
#' @param chart_type Default = "all". Choices one or more of "all", "param_absolute", "class_absolute",
#' "param_diff_absolute", "class_diff_absolute", "param_diff_percent", "class_diff_percent", "region_absolute"
#' @param col_agg Default = "class". Column to remove and then aggregate by.
#' @param aspect_ratio Default = 0.75. aspect ratio
#' @param size Default = 1.5. line size
#' @param size_text Default = 15. Text size
#' @param theme Default = NULL.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param save Default = TRUE. Save plots.
#' @param show Default = FALSE. Whether to show figure outputs in console.
#' @param title Default = TRUE. Used for region or subRegions or to add title to all plots.
#' @param folder Default = getwd().
#' @param scenRef Default = NULL. Reference Scenario
#' @param scenDiff Default = NULL. Difference Scenarios
#' @param xRef Default = NULL. Reference x
#' @param xDiff Default = NULL. Difference x
#' @param diff_type Default = "both". One of "absolute", "percent", "both".
#' @param diff_type_x Default = "both". One of "absolute", "percent", "both".
#' @param diff_type_x Default = "both". One of "absolute", "percent", "both".
#' @param diff_text_percent Default = "_diffPrcnt"
#' @param diff_text_absolute Default = "_diffAbs"
#' @param diff_text_percent_x Default = "_xdiffPrcnt"
#' @param diff_text_absolute_x Default = "_xdiffAbs"
#' @param width Default = NULL
#' @param height Default = NULL
#' @param append Default = ""
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @param include_points Default = FALSE. Add data points to all line charts.
#' @param summary_line Default = FALSE. Add parameter summary line to all bar charts.
#' @param waterfall_x Default = NULL. Year (or x value) for which to make waterfall plot. If NULL, latest year will be used
#' @param interaction_col_lty Default = NULL. Column to use for interaction plot linetype.
#' @param interaction_col_color Default = NULL. Column to use for interaction plot color.
#' @param palette Default = NULL. Named vector with custom palette colors (can include classes, regions, and/or scenarios)
#' @param linetype Default = NULL. Named vector with custom linetypes (solid lines will be used if not provided)
#' @param ylim Default = NULL. Y-axis limits
#' @param waterfall_single_chart Default = FALSE. If there are multiple diff scenarios, show them on a single chart?
#' @param waterfall_scen_order Default = NULL. If waterfall_single_chart option is selected, order of scenarios in chart
#' @param waterfall_vertical_dim Default = NULL. Column defining vertical stacking for waterfall charts
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

chart <- function(data = NULL,
                  col_agg = "class",
                  chart_type = "all",
                  aspect_ratio = 0.75,
                  size = 0.75,
                  size_text = 15,
                  theme = NULL,
                  ncol = NULL,
                  scales = "free_y",
                  save = TRUE,
                  show = FALSE,
                  title = TRUE,
                  folder = getwd(),
                  scenRef = NULL,
                  scenDiff = NULL,
                  xRef = NULL,
                  xDiff = NULL,
                  diff_type = "both",
                  diff_type_x = "both",
                  diff_text_percent = "diffPrcnt",
                  diff_text_absolute = "diffAbs",
                  diff_text_percent_x = "xdiffPrcnt",
                  diff_text_absolute_x = "xdiffAbs",
                  width = NULL,
                  height = NULL,
                  append = "",
                  break_interval = NULL,
                  include_points = FALSE,
                  summary_line = FALSE,
                  waterfall_x = NULL,
                  palette = NULL,
                  linetype = NULL,
                  ylim = NULL,
                  waterfall_single_chart = F,
                  waterfall_scen_order = NULL,
                  waterfall_vertical_dim = NULL,
                  interaction_col_lty = NULL,
                  interaction_col_color = NULL){

  print("Starting chart...")

  # # Check
  # col_agg = "class"
  # chart_type = "all"
  # aspect_ratio = 0.75
  # size = 0.75
  # size_text = 10
  # theme = NULL
  # ncol = NULL
  # scales = "free_y"
  # save = TRUE
  # show = TRUE
  # title = TRUE
  # folder = getwd()
  # scenRef = NULL
  # scenDiff = NULL
  # xRef = NULL
  # xDiff = NULL
  # diff_type = "both"
  # diff_type_x = "both"
  # diff_text_percent = "diffPrcnt"
  # diff_text_absolute = "diffAbs"
  # diff_text_percent_x = "xdiffPrcnt"
  # diff_text_absolute_x = "xdiffAbs"
  # width = NULL
  # height = NULL
  # append= ""
  # break_interval = NULL
  # include_points = FALSE

  #...........................
  # Initialize -----------------------------------------------------------------
  #...........................

  NULL -> region -> subRegion -> region_subRegion_check -> param -> scenario

  charts_out <- list()
  count <- 1

  if(save){
    if(!dir.exists(folder)){
      dir.create(folder)
      folder <- normalizePath(folder)
      }
  }

  theme_default <- ggplot2::theme(
    strip.background = ggplot2::element_rect(color = "black", fill = "gray30"),
    strip.text = ggplot2::element_text(color = "white"),
    aspect.ratio = aspect_ratio,
    text = ggplot2::element_text(size = size_text),
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.title = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
    legend.margin = ggplot2::margin(t = 2.5, r = 2.5, b = 2.5, l =2.5, "pt"),
    plot.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, "pt"),
    panel.spacing = ggplot2::unit(5, "pt")
  )

  #.................................
  # Prepare Data ---------------------------------------------------------------
  #.................................

  data_full <- rchart::add_missing(data,
                                   interaction_col_lty = interaction_col_lty,
                                   interaction_col_color = interaction_col_color)
  data_full <- data_full %>%
    dplyr::mutate(class = as.character(class),
                  class = dplyr::if_else(grepl("^class1$|^class$",class),param,class))
  data_agg <- rchart::aggregate_data(data = data_full, col_agg = col_agg)
  if(!is.null(scenRef)){
  data_full_diff <- rchart::calculate_diff(data = data_full,
                                           scenRef = scenRef,
                                           scenDiff = scenDiff,
                                           xRef = xRef,
                                           xDiff = xDiff,
                                           diff_type = diff_type,
                                           diff_type_x = diff_type_x,
                                           diff_text_percent = diff_text_percent,
                                           diff_text_absolute = diff_text_absolute,
                                           diff_text_percent_x = diff_text_percent_x,
                                           diff_text_absolute_x = diff_text_absolute_x)

  data_agg_diff <- rchart::calculate_diff(data = data_agg,
                                           scenRef = scenRef,
                                           scenDiff = scenDiff,
                                           xRef = xRef,
                                           xDiff = xDiff,
                                           diff_type = diff_type,
                                           diff_type_x = diff_type_x,
                                           diff_text_percent = diff_text_percent,
                                           diff_text_absolute = diff_text_absolute,
                                           diff_text_percent_x = diff_text_percent_x,
                                           diff_text_absolute_x = diff_text_absolute_x)
  } else {
    data_full_diff <- tibble::tibble()
    data_agg_diff <- tibble::tibble()
  }

  # Find difference scenarios to plot
  if(is.null(scenDiff) & !is.null(scenRef)){
    scenDiff_plot <- unique(c(unique(data_agg_diff$scenario),unique(data_full_diff$scenario)))
    scenDiff_plot <- scenDiff_plot[grepl(paste0(diff_text_percent,"|",
                                         diff_text_absolute,"|",
                                         diff_text_percent_x, "|",
                                         diff_text_absolute_x),scenDiff_plot)]
  }

  n_param = length(unique(data_agg$param))
  n_scenario = length(unique(data_full$scenario))

  #.................................
  # Prepare region subRegion append --------------------------------------------
  #.................................

  mapping_region_subRegion <- data_full %>%
    dplyr::select(region,subRegion) %>%
    unique() %>%
    dplyr::mutate(region_subRegion_check = region==subRegion); mapping_region_subRegion

  #.................................
  # Single-Region Charts -------------------------------------------------------
  #.................................

  for(row_i in 1:nrow(mapping_region_subRegion)){

    region_i <- mapping_region_subRegion[row_i,]$region; region_i
    subRegion_i <- mapping_region_subRegion[row_i,]$subRegion; subRegion_i

      # Prepare data for region_subRegion loops
      data_full_i <- data_full
      data_agg_i <- data_agg
      data_full_diff_i <- data_full_diff
      data_agg_diff_i <- data_agg_diff

      # Filter for region if more than 1 region
      if(length(unique(data_full$region))==1){region_i = ""} else {
        if(nrow(data_full_i)>0){ data_full_i<- data_full %>% dplyr::filter(region == region_i)}
        if(nrow(data_agg_i)>0){ data_agg_i <- data_agg %>% dplyr::filter(region == region_i)}
        if(nrow(data_full_diff_i)>0){ data_full_diff_i <- data_full_diff %>% dplyr::filter(region == region_i)}
        if(nrow(data_agg_diff_i)>0){ data_agg_diff_i <- data_agg_diff %>% dplyr::filter(region == region_i)}
      }
      # Filter for subRegions if more than 1 subRegion
      if(length(unique(data_full$subRegion))==1){subRegion_i=""} else {
        if(nrow(data_full_i)>0){ data_full_i<- data_full %>% dplyr::filter(subRegion == subRegion_i)}
        if(nrow(data_agg_i)>0){ data_agg_i <- data_agg %>% dplyr::filter(subRegion == subRegion_i)}
        if(nrow(data_full_diff_i)>0){ data_full_diff_i <- data_full_diff %>% dplyr::filter(subRegion == subRegion_i)}
        if(nrow(data_agg_diff_i)>0){ data_agg_diff_i <- data_agg_diff %>% dplyr::filter(subRegion == subRegion_i)}
      }

      if(region_i == "" & subRegion_i == ""){
        region_subRegion = ""
      } else if (region_i == "" & subRegion_i != ""){
      region_subRegion = paste0(subRegion_i)
      }  else if (region_i != "" & subRegion_i == ""){
        region_subRegion = paste0(region_i)
      } else if (region_i != "" & subRegion_i != "" & region_i == subRegion_i){
        region_subRegion = paste0(region_i)
      } else {
        region_subRegion = paste0(region_i,"_",subRegion_i)
      }




  #.................................
  ## Params (Aggregated Classes) Line Plots ====================================
  #.................................

  ### Scenarios Absolute #######################################################

      if (nrow(data_agg_i) > 0 & any(grepl("^all|param_absolute",chart_type,ignore.case = T))) {
        chart_name_i <- "chart_param"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,append,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion,append,".png")
          chart_name_i <- paste0(chart_name_i, "_", region_subRegion)
        }

        charts_out[[count]] <-
          rchart::plot_param_absolute(
            data = data_agg_i,
            size = size,
            theme = theme,
            theme_default = theme_default,
            ncol = ncol,
            scales = scales,
            break_interval = break_interval,
            include_points = include_points,
            palette = palette,
            linetype = linetype,
            interaction_col_lty = interaction_col_lty,
            interaction_col_color = interaction_col_color
          )

        # Set title if provided or turn off
        if(title != F){
        if(is.character(title)){
          charts_out[[count]] <- charts_out[[count]] +
          ggplot2::ggtitle(paste0(title," ",region_subRegion))}else{
            charts_out[[count]] <- charts_out[[count]] +
              ggplot2::ggtitle(region_subRegion)
          }
        }

        names(charts_out)[count] <- chart_name_i

        if(show){ print(charts_out[[count]])}

        if (save) {

          if(is.null(ncol)){
            if((n_param %% 2) == 0){ncol = as.integer(ceiling(n_param^0.5))}
            if((n_param %% 2) != 0){ncol = as.integer(ceiling(n_param^0.5))+1}
          }
          width_i = 7*max(ncol^0.5,1)
          height_i = 5*max((n_param-ncol)^0.5,1)

          if(!is.null(width)){width_i = width}
          if(!is.null(height)){height_i = height}

          ggplot2::ggsave(
            filename = fname_i,
            plot = charts_out[[count]],
            width = width_i,
            height = height_i,
            units = "in"
          )
          print(paste0("Figure saved as: ", fname_i))
        }

        count = count + 1
      }

  ### Scenarios Difference Absolute ############################################

      if (nrow(data_agg_diff_i) > 0 & any(grepl("^all|param_diff_absolute",chart_type,ignore.case = T))) {

        scenDiff_plot_i <- scenDiff_plot[grepl(diff_text_absolute, scenDiff_plot)]

        chart_name_i <- "chart_param_diff_absolute"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,append,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion,append,".png")
          chart_name_i <- paste0(chart_name_i, "_", region_subRegion)
        }

        charts_out[[count]] <-
          rchart::plot_param_difference(
            data = data_agg_diff_i,
            scenRef = scenRef,
            scenDiff = scenDiff_plot_i,
            theme = theme,
            theme_default = theme_default,
            ncol = ncol,
            facet_label_diff = "Difference Absolute",
            size = size,
            diff_text = diff_text_absolute,
            break_interval = break_interval,
            include_points = include_points,
            palette = palette,
            scales = scales
          )


        # Set title if provided or turn off
        if(title != F){
          if(is.character(title)){
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(title," ",region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))
          }else{
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))

          }
        }

        names(charts_out)[count] <- chart_name_i

        if(show){ print(charts_out[[count]])}

        if (save) {

          width_i = 14
          height_i = 5*n_param

          if(!is.null(width)){width_i = width}
          if(!is.null(height)){height_i = height}

          ggplot2::ggsave(
            filename = fname_i,
            plot = charts_out[[count]],
            width = width_i,
            height = height_i,
            units = "in"
          )

          print(paste0("Figure saved as: ", fname_i))
        }

        count = count + 1
      }

  ### Scenarios Difference Percent ##############################################

      if (nrow(data_agg_diff_i) > 0 & any(grepl("^all|param_diff_percent",chart_type,ignore.case = T))) {

        scenDiff_plot_i <- scenDiff_plot[grepl(diff_text_percent, scenDiff_plot)]

        chart_name_i <- "chart_param_diff_percent"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,append,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion,append,".png")
          chart_name_i <- paste0(chart_name_i, "_", region_subRegion)
        }

        charts_out[[count]] <-
          rchart::plot_param_difference(
            data = data_agg_diff_i,
            scenRef = scenRef,
            scenDiff = scenDiff_plot_i,
            theme = theme,
            theme_default = theme_default,
            facet_label_diff = "Difference Percent",
            size = size,
            diff_text = diff_text_percent,
            break_interval = break_interval,
            include_points = include_points,
            palette = palette,
            scales = scales
          )

        # Set title if provided or turn off
        if(title != F){
          if(is.character(title)){
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(title," ",region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))
          }else{
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))

          }
        }

        names(charts_out)[count] <- chart_name_i

        if(show){ print(charts_out[[count]])}

        if (save) {

          width_i = 14
          height_i = 5*n_param

          if(!is.null(width)){width_i = width}
          if(!is.null(height)){height_i = height}

          ggplot2::ggsave(
            filename = fname_i,
            plot = charts_out[[count]],
            width = width_i,
            height = height_i,
            units = "in"
          )

          print(paste0("Figure saved as: ", fname_i))
        }

        count = count + 1
      }


  #.................................
  ## Params and Classes Bar Charts =============================================
  #.................................
  # if more than one class

  ### Scenarios Absolute #######################################################

  if(length(unique(data_full_i$class))>1){
      if (nrow(data_full_i) > 0 & any(grepl("^all|class_absolute",chart_type,ignore.case = T))) {
        chart_name_i <- "chart_class"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,append,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion,append,".png")
          chart_name_i <- paste0(chart_name_i, "_", region_subRegion)
        }

        charts_out[[count]] <-
          rchart::plot_class_absolute(
            data = data_full_i,
            theme = theme,
            theme_default = theme_default,
            ncol = ncol,
            scales = scales,
            size_text = size_text,
            break_interval = break_interval,
            summary_line = summary_line,
            data_agg = data_agg_i,
            palette = palette
          )

        # data = data_full_i
        # theme = theme
        # theme_default = theme_default
        # ncol = ncol
        # scales = scales

        # Set title if provided or turn off
        if(title != F){
          if(is.character(title)){
            charts_out[[count]] <- charts_out[[count]] +
              ggplot2::ggtitle(paste0(title," ",region_subRegion))}else{
                charts_out[[count]] <- charts_out[[count]] +
                  ggplot2::ggtitle(region_subRegion)
              }
        }

        names(charts_out)[count] <- chart_name_i

        if(show){ print(charts_out[[count]])}

        if (save) {

          width_i = 7*length(unique(data_full_i$scenario))
          height_i = 5*length(unique(data_full_i$param))

          if(!is.null(width)){width_i = width}
          if(!is.null(height)){height_i = height}

          ggplot2::ggsave(
            filename = fname_i,
            plot = charts_out[[count]],
            width = width_i,
            height = height_i,
            units = "in"
          )
          print(paste0("Figure saved as: ", fname_i))
        }

        count = count + 1
      }

  ### Scenarios Difference Absolute ############################################


      if (nrow(data_full_diff_i) > 0 & any(grepl("^all|class_diff_absolute",chart_type,ignore.case = T))) {

        scenDiff_plot_i <- scenDiff_plot[grepl(diff_text_absolute, scenDiff_plot)]

        chart_name_i <- "chart_class_diff_absolute"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,append,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion,append,".png")
          chart_name_i <- paste0(chart_name_i, "_", region_subRegion)
        }


        charts_out[[count]] <-
          rchart::plot_class_difference(
            data = data_full_diff_i,
            scenRef = scenRef,
            scenDiff = scenDiff_plot_i,
            theme = theme,
            theme_default = theme_default,
            ncol = ncol,
            diff_text = diff_text_absolute,
            break_interval = break_interval,
            include_points = include_points,
            summary_line = summary_line,
            data_agg_ref = data_agg_i,
            data_agg_diff = data_agg_diff_i,
            palette = palette,
            scales = scales
          )

        # data = data_full_diff_i
        # scenRef = scenRef
        # scenDiff = scenDiff_plot_i
        # theme = theme
        # theme_default = theme_default
        # diff_text = diff_text_absolute

        # Set title if provided or turn off
        if(title != F){
          if(is.character(title)){
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(title," ",region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))
          }else{
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))

          }
        }

        names(charts_out)[count] <- chart_name_i

        if(show){ print(charts_out[[count]])}

        if (save) {

          width_i = 7*n_scenario
          height_i = 5*n_param

          if(!is.null(width)){width_i = width}
          if(!is.null(height)){height_i = height}

          ggplot2::ggsave(
            filename = fname_i,
            plot = charts_out[[count]],
            width = width_i,
            height = height_i,
            units = "in"
          )

          print(paste0("Figure saved as: ", fname_i))
        }

        count = count + 1
      }

  ### Scenarios Difference Percent #############################################

      if (nrow(data_full_diff_i) > 0 & any(grepl("^all|class_diff_percent",chart_type,ignore.case = T))) {

        scenDiff_plot_i <- scenDiff_plot[grepl(diff_text_percent, scenDiff_plot)]

        chart_name_i <- "chart_class_diff_percent"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,append,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion,append,".png")
          chart_name_i <- paste0(chart_name_i, "_", region_subRegion)
        }

        charts_out[[count]] <-
          rchart::plot_class_difference(
            data = data_full_diff_i,
            scenRef = scenRef,
            scenDiff = scenDiff_plot_i,
            theme = theme,
            theme_default = theme_default,
            ncol = ncol,
            diff_text = diff_text_percent,
            diff_type="line",
            break_interval = break_interval,
            include_points = include_points,
            summary_line = summary_line,
            data_agg_ref = data_agg_i,
            data_agg_diff = data_agg_diff_i,
            palette = palette,
            scales = scales
          )

        # data = data_full_diff_i
        # scenRef = scenRef
        # scenDiff = scenDiff_plot_i
        # theme = theme
        # theme_default = theme_default
        # diff_text = diff_text_absolute

        # Set title if provided or turn off
        if(title != F){
          if(is.character(title)){
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(title," ",region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))
          }else{
            title_label <- cowplot::ggdraw() +
              cowplot::draw_label(paste0(region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
            charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))

          }
        }

        names(charts_out)[count] <- chart_name_i

        if(show){ print(charts_out[[count]])}

        if (save) {

          width_i = 7*n_scenario
          height_i = 5*n_param

          if(!is.null(width)){width_i = width}
          if(!is.null(height)){height_i = height}

          ggplot2::ggsave(
            filename = fname_i,
            plot = charts_out[[count]],
            width = width_i,
            height = height_i,
            units = "in"
          )

          print(paste0("Figure saved as: ", fname_i))
        }

        count = count + 1
      }

    ### Waterfall chart ########################################################
    if (nrow(data_full_diff_i) > 0 & any(grepl("^all|class_waterfall",chart_type,ignore.case = T))){
      # get all the scenDiff names
      scenDiff = unique(data_full$scenario)[!unique(data_full$scenario) %in% scenRef]

      chart_name_i <- "chart_class_waterfall"
      if(region_subRegion==""){
        fname_i <- paste0(folder, "/", chart_name_i, "_", append,".png")
      } else {
        fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion,append,".png")
        chart_name_i <- paste0(chart_name_i, "_", region_subRegion)
      }

      charts_out[[count]] <-
        rchart::plot_class_waterfall(
          data_diff = data_full_diff_i,
          data_agg = data_agg_diff_i,
          scenRef = scenRef,
          scenDiff = scenDiff,
          theme = theme,
          theme_default = theme_default,
          diff_text = diff_text_absolute,
          break_interval = break_interval,
          include_points = include_points,
          summary_line = summary_line,
          wf_x = waterfall_x,
          palette = palette,
          ylim = ylim,
          single_chart = waterfall_single_chart,
          scen_order = waterfall_scen_order,
          scales = scales
        )

      # Set title if provided or turn off
      if(title != F){
        if(is.character(title)){
          title_label <- cowplot::ggdraw() +
            cowplot::draw_label(paste0(title," ",region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
          charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))
        }else{
          title_label <- cowplot::ggdraw() +
            cowplot::draw_label(paste0(region_subRegion), fontface='bold', vjust=0, hjust=-0.25,x=0, y=0)
          charts_out[[count]] <- cowplot::plot_grid(title_label, charts_out[[count]], ncol=1, rel_heights=c(0.5,5*n_param))

        }
      }

      names(charts_out)[count] <- chart_name_i

      if(show){ print(charts_out[[count]])}

      if (save) {

        width_i = 7*n_scenario
        height_i = 5*n_param

        if(!is.null(width)){width_i = width}
        if(!is.null(height)){height_i = height}

        ggplot2::ggsave(
          filename = fname_i,
          plot = charts_out[[count]],
          width = width_i,
          height = height_i,
          units = "in"
        )

        print(paste0("Figure saved as: ", fname_i))
      }

      count = count + 1
      }


    } # if > 1 class


    } # Close for(subRegion_i in subRegions)

  #.................................
  ## Multi- Region plots =======================================================
  #.................................

  ### region-param class plots #################################################

  if((length(unique(data_agg$region))>1) | (length(unique(data_agg$subRegion))>1)){
    # save a separate plot for each scenario
    for(scen_i in 1:length(unique(data_full$scenario))){
      scen_name_i <- unique(data_full$scenario)[scen_i]
      data_full_i <- data_full %>% dplyr::filter(scenario == scen_name_i)
      if (nrow(data_full_i) > 0 & any(grepl("^all|class_absolute",chart_type,ignore.case = T))) {
        chart_name_i <- "chart_class"
        if(length(unique(data_full$scenario)) == 1){
          fname_i <- paste0(folder, "/", chart_name_i, "byRegion", append,".png")
          chart_name_i <- paste0(chart_name_i, "_byRegion")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", scen_name_i,append,".png")
          chart_name_i <- paste0(chart_name_i, "_", scen_name_i)
        }

        charts_out[[count]] <-
          rchart::plot_class_absolute(
            data = data_full_i,
            theme = theme,
            theme_default = theme_default,
            ncol = ncol,
            scales = scales,
            size_text = size_text,
            break_interval = break_interval,
            col_dim = "region",
            summary_line = summary_line,
            data_agg = data_agg_i,
            palette = palette
          )

        # data = data_full_i
        # theme = theme
        # theme_default = theme_default
        # ncol = ncol
        # scales = scales

        # Set title if provided or turn off
        if(title != F){
          if(is.character(title)){
            charts_out[[count]] <- charts_out[[count]] +
              ggplot2::ggtitle(paste0(title," ",scen_name_i))}else{
                charts_out[[count]] <- charts_out[[count]] +
                  ggplot2::ggtitle(scen_name_i)
              }
        }

        names(charts_out)[count] <- chart_name_i

        if(show){ print(charts_out[[count]])}

        if (save) {

          width_i = 7*length(unique(data_full_i$scenario))
          height_i = 5*length(unique(data_full_i$param))

          if(!is.null(width)){width_i = width}
          if(!is.null(height)){height_i = height}

          ggplot2::ggsave(
            filename = fname_i,
            plot = charts_out[[count]],
            width = width_i,
            height = height_i,
            units = "in"
          )
          print(paste0("Figure saved as: ", fname_i))
        }

        count = count + 1
    }
    }
  }

  ### param-scenario region agg plots ##########################################

  if((length(unique(data_agg$region))>1) | (length(unique(data_agg$subRegion))>1)){

    # Assign region_subRegion to class category
    if((length(unique(data_agg$region))>1) & (length(unique(data_agg$subRegion))>1)){
      data_agg_reg <- data_agg %>%
        dplyr::mutate(region_subRegion_check = region!=subRegion,
                      class = dplyr::if_else(region_subRegion_check,paste0(region,"_",subRegion),region)) %>%
        dplyr::select(-region_subRegion_check)
    }
    if(!(length(unique(data_agg$region))>1) & (length(unique(data_agg$subRegion))>1)){
      data_agg_reg <- data_agg %>%
        dplyr::mutate(class = paste0(subRegion))
    }
    if((length(unique(data_agg$region))>1) & !(length(unique(data_agg$subRegion))>1)){
      data_agg_reg <- data_agg %>%
        dplyr::mutate(class = paste0(region))
    }

    if (nrow(data_agg_reg) > 0 & any(grepl("^all|region_absolute",chart_type,ignore.case = T))) {

      chart_name_i <- "chart_region_absolute"
      fname_i <- paste0(folder, "/", chart_name_i,append,".png")

      charts_out[[count]] <-
        rchart::plot_reg_absolute(
          data = data_agg_reg,
          size = size,
          theme = theme,
          theme_default = theme_default,
          scales = scales,
          break_interval = break_interval,
          include_points = include_points,
          palette = palette
        )

      # Set title if provided or turn off
      if(title != F){
        if(is.character(title)){
          charts_out[[count]] <- charts_out[[count]] +
            ggplot2::ggtitle(paste0(title))}
      }

      names(charts_out)[count] <- chart_name_i

      if(show){ print(charts_out[[count]])}

      if (save) {

        if(is.null(ncol)){
          if((n_param %% 2) == 0){ncol = as.integer(ceiling(n_param^0.5))}
          if((n_param %% 2) != 0){ncol = as.integer(ceiling(n_param^0.5))+1}
        }
        width_i = 7*n_scenario
        height_i = 5*n_param

        if(!is.null(width)){width_i = width}
        if(!is.null(height)){height_i = height}

        ggplot2::ggsave(
          filename = fname_i,
          plot = charts_out[[count]],
          width = width_i,
          height = height_i,
          units = "in"
        )
        print(paste0("Figure saved as: ", fname_i))
      }

      count = count + 1
    }

  }

  ### region-split waterfall plots #############################################
  # if((length(unique(data_full_diff$region))>1) | (length(unique(data_full_diff$subRegion))>1)){
  #   # Assign region_subRegion to region category
  #   if((length(unique(data_full_diff$region))>1) & (length(unique(data_full_diff$subRegion))>1)){
  #     data_full_diff_reg <- data_full_diff %>%
  #       dplyr::mutate(region_subRegion_check = region!=subRegion,
  #                     region = dplyr::if_else(region_subRegion_check,paste0(region,"_",subRegion),region)) %>%
  #       dplyr::select(-region_subRegion_check, -subRegion)
  #     data_agg_diff_reg <- data_agg_diff %>%
  #       dplyr::mutate(region_subRegion_check = region!=subRegion,
  #                     region = dplyr::if_else(region_subRegion_check,paste0(region,"_",subRegion),region)) %>%
  #       dplyr::select(-region_subRegion_check, -subRegion)
  #   }
  #   if(!(length(unique(data_full_diff$region))>1) & (length(unique(data_full_diff$subRegion))>1)){
  #     data_full_diff_reg <- data_full_diff %>%
  #       dplyr::mutate(region = paste0(subRegion)) %>%
  #       dplyr::select(-subRegion)
  #     data_agg_diff_reg <- data_agg_diff %>%
  #       dplyr::mutate(region = paste0(subRegion)) %>%
  #       dplyr::select(-subRegion)
  #   }
  #   if((length(unique(data_full_diff$region))>1) & !(length(unique(data_full_diff$subRegion))>1)){
  #     data_full_diff_reg <- data_full_diff %>%
  #       dplyr::mutate(region = paste0(region)) %>%
  #       dplyr::select(-subRegion)
  #     data_agg_diff_reg <- data_agg_diff %>%
  #       dplyr::mutate(region = paste0(region)) %>%
  #       dplyr::select(-subRegion)
  #   }
  #
  #   # get all the scenDiff names
  #   scenDiff = unique(data_full$scenario)[!unique(data_full$scenario) %in% scenRef]
  #
  #   # one waterfall plot with stacked regions for each class
  #   if (any(grepl("^all|class_region_waterfall",chart_type,ignore.case = T))){
  #     chart_name_i <- "chart_class_region_waterfall"
  #     fname_i <- paste0(folder, "/", chart_name_i,append,".png")
  #
  #     charts_out[[count]] <-
  #       rchart::plot_class_waterfall(
  #         data_diff = data_full_diff_reg,
  #         data_agg = data_agg_diff_reg,
  #         scenRef = scenRef,
  #         scenDiff = scenDiff,
  #         theme = theme,
  #         theme_default = theme_default,
  #         diff_text = diff_text_absolute,
  #         break_interval = break_interval,
  #         include_points = include_points,
  #         summary_line = summary_line,
  #         wf_x = waterfall_x,
  #         palette = palette,
  #         vertical_dim = "region",
  #         single_chart = waterfall_single_chart,
  #         scen_order = waterfall_scen_order,
  #         scales = scales
  #       )
  #
  #     names(charts_out)[count] <- chart_name_i
  #
  #     if(show){ print(charts_out[[count]])}
  #
  #     if (save) {
  #
  #       if(is.null(ncol)){
  #         if((n_param %% 2) == 0){ncol = as.integer(ceiling(n_param^0.5))}
  #         if((n_param %% 2) != 0){ncol = as.integer(ceiling(n_param^0.5))+1}
  #       }
  #       width_i = 7*n_scenario
  #       height_i = 5*n_param
  #
  #       if(!is.null(width)){width_i = width}
  #       if(!is.null(height)){height_i = height}
  #
  #       ggplot2::ggsave(
  #         filename = fname_i,
  #         plot = charts_out[[count]],
  #         width = width_i,
  #         height = height_i,
  #         units = "in"
  #       )
  #       print(paste0("Figure saved as: ", fname_i))
  #     }
  #
  #     count = count + 1
  #   }
  #
  # }

  #...........................
  # Close-out
  #...........................

  print("Completed chart.")

  invisible(charts_out)

}
