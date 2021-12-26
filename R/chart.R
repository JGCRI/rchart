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
                  folder = getwd()){

  print("Starting chart...")

  # Check
  # data = NULL
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
  data_agg = rchart::aggregate_data(data = data_full, col_agg = col_agg)
  n_param = length(unique(data_agg$param))
  if(is.null(ncol)){ncol = as.integer(ceiling(n_param^0.5))}
  width_i = 7*max(ncol^0.5,1)
  height_i = 5*max((n_param-ncol)^0.5,1)

  #.................................
  # Summary Plot comparing scenarios absolute
  #.................................

  chart_name_i <- "chart_summary"
  fname_i <- paste0(folder,"/",chart_name_i,".png")
  charts_out[[count]] <- rchart::plot_summary(data = data_agg,
                                            aspect_ratio = aspect_ratio,
                                            size = size,
                                            size_text = size_text,
                                            theme = theme,
                                            ncol = ncol,
                                            scales = scales)
  names(charts_out)[count] <- chart_name_i

  if(save){ggplot2::ggsave(filename = fname_i,
                           plot = charts_out[[count]],
                           width = width_i,
                           height = height_i,
                           units = "in")

    print(paste0("Figure saved as: ",fname_i))
    }

  #.................................
  # Summary Plot comparing scenarios diff absolute
  #.................................

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
