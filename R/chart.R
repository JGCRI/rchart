#' chart
#'
#' Used to add missing data to input files and customize format
#' @param data Default = NULL. Dataframe to test and convert.
#' @param chart_type Default = "all". Choices one or more of "all", "lines", "bar", "lines_diff", "bar_diff"
#' @param col_agg Default = "class". Column to remove and then aggregate by.
#' @param aspect_ratio Default = 0.75. aspect ratio
#' @param size Default = 1.5. line size
#' @param size_text Default = 15. Text size
#' @param theme Default = NULL.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param save Default = TRUE. Save plots.
#' @param show Default = TRUE. Whether to show figure outputs in console.
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
#' @param width = NULL
#' @param height = NULL
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

chart <- function(data = NULL,
                  col_agg = "class",
                  chart_type = "all",
                  aspect_ratio = 0.75,
                  size = 1.5,
                  size_text = 15,
                  theme = NULL,
                  ncol = NULL,
                  scales = "free_y",
                  save = TRUE,
                  show = TRUE,
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
                  height = NULL){

  print("Starting chart...")

  # Check
  # col_agg = "class"
  # chart_type = "all"
  # aspect_ratio = 0.75
  # size = 1.5
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

  #...........................
  # Initialize
  #...........................

  NULL -> region -> subRegion

  charts_out <- list()
  count <- 1

  if(save){
    if(!dir.exists(folder)){dir.create(folder)}
  }

  theme_default <- ggplot2::theme(
    strip.background = ggplot2::element_rect(color = "black", fill = "gray30"),
    strip.text = ggplot2::element_text(color = "white"),
    aspect.ratio = aspect_ratio,
    text = ggplot2::element_text(size = size_text),
    axis.title.y = ggplot2::element_text(vjust = 5),
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.title = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(t = 2.5, r = 2.5, b = 2.5, l =2.5, "pt"),
    plot.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, "pt"),
    panel.spacing = ggplot2::unit(5, "pt")
  )

  #.................................
  # Prepare Data
  #.................................

  data_full <- rchart::add_missing(data)
  data_agg <- rchart::aggregate_data(data = data_full, col_agg = col_agg)
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

  # Find difference scenarios to plot
  if(is.null(scenDiff) & !is.null(scenRef)){
    scenDiff_plot <- unique(c(unique(data_agg_diff$scenario),unique(data_full_diff$scenario)))
    scenDiff_plot <- scenDiff_plot[grepl(paste0(diff_text_percent,"|",
                                         diff_text_absolute,"|",
                                         diff_text_percent_x, "|",
                                         diff_text_absolute_x),scenDiff_plot)]
  }

  n_param = length(unique(data_agg$param))

  #.................................
  # Prepare region subRegion append
  #.................................

  regions <- data_full$region %>% unique()
  subRegions <- data_full$subRegion %>% unique()

  for(region_i in regions){
    for(subRegion_i in subRegions){

      # Prepare data for region_subRegion loops
      data_full_i <- data_full
      data_agg_i <- data_agg
      data_full_diff_i <- data_full_diff
      data_agg_diff_i <- data_agg_diff

      # Filter for region if more than 1 region
      if(length(regions)==1){region_i = ""} else {
        if(nrow(data_full_i)>0){ data_full_i<- data_full %>% dplyr::filter(region == region_i)}
        if(nrow(data_agg_i)>0){ data_agg_i <- data_agg %>% dplyr::filter(region == region_i)}
        if(nrow(data_full_diff_i)>0){ data_full_diff_i <- data_full_diff %>% dplyr::filter(region == region_i)}
        if(nrow(data_agg_diff_i)>0){ data_agg_diff_i <- data_agg_diff %>% dplyr::filter(region == region_i)}
      }
      # Filter for subRegions if more than 1 subRegion
      if(length(subRegions)==1){subRegion_i=""} else {
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
      } else {
        region_subRegion = paste0(region_i,"_",subRegion_i)
      }


  #.................................
  # Summary Plot comparing scenarios absolute
  #.................................

      if (nrow(data_agg_i) > 0) {
        chart_name_i <- "chart_lines"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion, ".png")
        }

        charts_out[[count]] <-
          rchart::plot_line_absolute(
            data = data_agg_i,
            size = size,
            theme = theme,
            theme_default = theme_default,
            ncol = ncol,
            scales = scales
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

  #.................................
  # Summary Plot comparing scenarios diff absolute
  #.................................

      if (nrow(data_agg_diff_i) > 0) {

        scenDiff_plot_i <- scenDiff_plot[grepl(diff_text_absolute, scenDiff_plot)]

        chart_name_i <- "chart_lines_diff_absolute"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion, ".png")
        }

        charts_out[[count]] <-
          rchart::plot_param_difference(
            data = data_agg_diff_i,
            scenRef = scenRef,
            scenDiff = scenDiff_plot_i,
            theme = theme,
            theme_default = theme_default,
            facet_label_diff = "Difference Absolute",
            size = size,
            diff_text = diff_text_absolute
          )

        # data = data_agg_diff_i
        # scenRef = scenRef
        # scenDiff = scenDiff_plot_i
        # theme = theme
        # theme_default = theme_default
        # facet_label_diff = "Difference Absolute"
        # size = size
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
  # Summary Plot comparing scenarios diff percent
  #.................................

      if (nrow(data_agg_diff_i) > 0) {

        scenDiff_plot_i <- scenDiff_plot[grepl(diff_text_percent, scenDiff_plot)]

        chart_name_i <- "chart_lines_diff_percent"
        if(region_subRegion==""){
          fname_i <- paste0(folder, "/", chart_name_i,".png")
        } else {
          fname_i <- paste0(folder, "/", chart_name_i, "_", region_subRegion, ".png")
        }

        charts_out[[count]] <-
          rchart::plot_param_difference(
            data = data_agg_diff_i,
            scenRef = scenRef,
            scenDiff = scenDiff_plot_i,
            theme = theme,
            theme_default = theme_default,
            facet_label_diff = "Difference Percent",
            y_label_diff = "%",
            size = size,
            diff_text = diff_text_percent
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
  # Bar Chart comparing scenarios Absolute
  #.................................

  #.................................
  # Bar Chart comparing scenarios Diff Absolute
  #.................................

  #.................................
  # Bar Chart comparing scenarios Diff Percent
  #.................................


  #...........................
  # Close-out
  #...........................

    } # Close for(subRegion_i in subRegions){
  } # Close for(region_i in regions){


  print("Completed chart.")

  invisible(charts_out)

}
