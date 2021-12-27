#' chart
#'
#' Used to add missing data to input files and customize format
#' @param data Default = NULL. Dataframe to test and convert.
#' @param chart_type Default = "all". Choices one or more of "all", "lines", "bar", "lines_diff", "bar_diff"
#' @param col_agg Default = "class". Column to remove and then aggregate by.
#' @param aspect_ratio Default = 0.75. aspect ratio
#' @param size Default = 1.5. line size
#' @param size_text Default = NULL. Text size
#' @param theme Default = NULL.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param save Default = TRUE. Save plots.
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
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

chart <- function(data = NULL,
                  col_agg = "class",
                  chart_type = "all",
                  aspect_ratio = 0.75,
                  size = 1.5,
                  size_text = 10,
                  theme = NULL,
                  ncol = NULL,
                  scales = "free_y",
                  save = TRUE,
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
                  diff_text_absolute_x = "xdiffAbs"){

  print("Starting chart...")

  # Check
  # data = rchart::exampleMapDataClass
  # col_agg = "class"
  # chart_type = "all"
  # aspect_ratio = 0.75
  # size = 10
  # size_title = NULL
  # theme = NULL
  # ncol = 3
  # scales = "free"
  # save = TRUE
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

  charts_out <- list()
  count <- 1

  if(save){
    if(!dir.exists(folder)){dir.create(folder)}
  }

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
  if(is.null(ncol)){ncol = as.integer(ceiling(n_param^0.5))}
  width_i = 7*max(ncol^0.5,1)
  height_i = 5*max((n_param-ncol)^0.5,1)

  #.................................
  # Summary Plot comparing scenarios absolute
  #.................................

  chart_name_i <- "chart_lines"
  fname_i <- paste0(folder,"/",chart_name_i,".png")
  charts_out[[count]] <- rchart::plot_line_absolute(data = data_agg,
                                                    aspect_ratio = aspect_ratio,
                                                    size = size,
                                                    size_text = size_text,
                                                    theme = theme,
                                                    ncol = ncol,
                                                    scales = scales)
  names(charts_out)[count] <- chart_name_i

  if(save){
    ggplot2::ggsave(filename = fname_i,
                           plot = charts_out[[count]],
                           width = width_i,
                           height = height_i,
                           units = "in")
    print(paste0("Figure saved as: ",fname_i))
    }

  #.................................
  # Summary Plot comparing scenarios diff absolute
  #.................................

  chart_name_i <- "chart_lines_diff"
  fname_i <- paste0(folder,"/",chart_name_i,".png")

  charts_out[[count]] <- rchart::plot_difference(data = data_agg_diff,
                                                 scenRef = scenRef,
                                                 scenDiff = scenDiff_plot,
                                                 plot_type = "line",
                                                 theme = theme)
  names(charts_out)[count] <- chart_name_i

  if(save){ggplot2::ggsave(filename = fname_i,
                           plot = charts_out[[count]],
                           width = width_i,
                           height = height_i,
                           units = "in")

    print(paste0("Figure saved as: ",fname_i))
  }

  #.................................
  # Summary Plot comparing scenarios diff percent
  #.................................

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

  print("Completed chart.")

  invisible(charts_out)

}
