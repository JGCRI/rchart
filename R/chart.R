#' chart
#'
#' Used to add missing data to input files and customize format
#' @param data Default = NULL. Dataframe to test and convert.
#' @param chart_type Default = "all". Choices one or more of "all", "lines", "bar", "lines_diff", "bar_diff"
#' @param col_agg Default = "class". Column to remove and then aggregate by.
#' @param aspect_ratio Default = 0.75. aspect ratio
#' @param size Default = 10. text size
#' @param size_title Default = NULL. size_title text
#' @param theme Default = NULL.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

chart <- function(data = NULL,
                  col_agg = "class",
                  aspect_ratio = 0.75,
                  size = 10,
                  size_title = NULL,
                  theme = NULL){

  print("Starting chart...")

  #...........................
  # Initialize
  #...........................

  charts_out <- list()
  count <- 1

  #.................................
  # Prepare Data
  #.................................

  data_full <- rchart::add_missing(data)

  data_agg = rchart::aggregate_data(data = data_full, col_agg = col_agg)

  #.................................
  # Summary Plot comparing scenarios absolute
  #.................................

  chart_name_i <- "chart_summary"
  charts_out[count] <- rchart::plot_summary(data = data_agg,
                                            aspect_ratio = aspect_ratio,
                                            size = size,
                                            size_title = size_title,
                                            theme = theme)

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
  # Theme
  #...........................


  #...........................
  # Close-out
  #...........................

  print("Completed chart.")

  invisible(charts_out)

}
