#' aggregate_data
#'
#' Used to add missing data to input files and customize format
#' @param data dataframe to test and convert
#' @param col_agg Default = "class". Column to remove and then aggregate by.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

aggregate_data <- function(data = NULL,
                           col_agg = "class"){

  # Check if aggregate is in the column names
  if(!"aggregate" %in% names(data)){
    data <- rchart::add_missing(data)
  }

  # Check that data has columns value and aggregate
  if("value" %in% names(data)){

  agg_sums <- data %>%
    dplyr::filter(aggregate == "sum") %>%
    dplyr::select(-dplyr::all_of(col_agg)) %>%
    dplyr::group_by_at(dplyr::vars(-value)) %>%
    dplyr::summarize_at(c("value"), list( ~ sum(.)))
  agg_means <- data %>%
    dplyr::filter(aggregate == "mean") %>%
    dplyr::select(-dplyr::all_of(col_agg)) %>%
    dplyr::group_by_at(dplyr::vars(-value)) %>%
    dplyr::summarize_at(c("value"), list( ~ mean(.)))

  data_out <- dplyr::bind_rows(agg_sums, agg_means) %>% dplyr::ungroup()

  } else {
    print(paste0("Data must have column 'value'. Skipping aggregate."))
    data_out <- data
  }

  invisible(data_out)
}
