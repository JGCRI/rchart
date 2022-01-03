#' calculate_diff
#'
#' Calculate difference between scenarios or time periods
#' @param data Default = NULL.
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

calculate_diff <- function(data = NULL,
                           scenRef = NULL,
                           scenDiff = NULL,
                           xRef = NULL,
                           xDiff = NULL,
                           diff_type = "both",
                           diff_type_x = "both",
                           diff_text_percent = "diffPrcnt",
                           diff_text_absolute = "diffAbs",
                           diff_text_percent_x = "xdiffPrcnt",
                           diff_text_absolute_x = "xdiffAbs") {

  # data = data_full
  # scenRef = "GCAM_SSP3"
  # scenDiff = NULL
  # xRef = NULL
  # xDiff = NULL
  # diff_type = "both"
  # diff_type_x = "both"
  # diff_text_percent = "diffPrcnt"
  # diff_text_absolute = "diffAbs"
  # diff_text_percent_x = "xdiffPrcnt"
  # diff_text_absolute_x = "xdiffAbs"

  # print("Starting calculate_diff ...")

  #...........................
  # Initialize
  #...........................

  NULL -> scenario -> value -> x

  orig_cols <- colnames(data)
  data_full <- rchart::add_missing(data)

  data_out <- tibble::tibble()

  #...........................
  # Calculate Scenario Difference
  #...........................

  if(!is.null(scenRef)){
  if(scenRef %in% unique(data_full$scenario)){

  if(is.null(scenDiff) & !is.null(scenRef)){scenDiff = unique(data_full$scenario)[!unique(data_full$scenario) %in% scenRef]}

  for (scenDiff_i in scenDiff) {

    data_temp <- data_full %>%
      dplyr::filter(scenario %in% c(scenRef, scenDiff_i))  %>%
      dplyr::filter(!(is.na(class) & value==0))%>%
      dplyr::mutate(class=dplyr::if_else(is.na(class),"NA",class))

    data_temp <- data_temp %>%
      tidyr::spread(scenario, value)

    data_temp[is.na(data_temp)] <- 0

    # Absolute Difference
    if(any(grepl("both|absolute|abs",diff_type,ignore.case = T))){
      data_temp_absolute <- data_temp %>%
        dplyr::mutate(!!paste0(scenDiff_i, "_", diff_text_absolute, "_", scenRef) := get(scenDiff_i) - get(scenRef)) %>%
        dplyr::select(-dplyr::one_of(c(scenDiff_i, scenRef)))
      data_temp_absolute_long <- data_temp_absolute %>%
        tidyr::gather(key = scenario, value = value, -c(names(data_temp_absolute)[!names(data_temp_absolute) %in% paste0(scenDiff_i, "_", diff_text_absolute, "_", scenRef)]))
      data_out <- dplyr::bind_rows(data_out, data_temp_absolute_long)
      }

    # Percent Difference
    if(any(grepl("both|percentage|prcnt",diff_type,ignore.case = T))){
      data_temp_percent <- data_temp %>%
        dplyr::mutate(!!paste0(scenDiff_i, "_", diff_text_percent, "_", scenRef) := (get(scenDiff_i) - get(scenRef))*100/get(scenRef)) %>%
        dplyr::select(-dplyr::one_of(c(scenDiff_i, scenRef)))
      data_temp_percent_long <- data_temp_percent %>%
        tidyr::gather(key = scenario, value = value, -c(names(data_temp_percent)[!names(data_temp_percent) %in% paste0(scenDiff_i, "_", diff_text_percent, "_", scenRef)]))
      data_out <- dplyr::bind_rows(data_out, data_temp_percent_long)
      }

  }

  data_out <- dplyr::bind_rows(
    data_full,
    data_out %>%
      dplyr::mutate(scenario := factor(scenario)))

  } else {
    print("scenRef provided is not present in the data. Skipping calculate_diff.")
  }

  } else {
  }

  #...........................
  # Calculate X Difference
  #...........................

  if(!is.null(xRef)){

    if(nrow(data_out)==0){
      data_out<- data_full}

    scenarios <- data_out$scenario%>%unique()

    for(scenario_i in scenarios){

    if(xRef %in% unique(data_out$x)){

      if(is.null(xDiff) & !is.null(xRef)){xDiff = unique(data_out$x)[!unique(data_out$x) %in% xRef]}

      for (xDiff_i in xDiff) {

        data_temp_x <- data_out %>%
          dplyr::filter(scenario %in% scenario_i) %>%
          dplyr::filter(x %in% c(xRef, xDiff_i))  %>%
          dplyr::filter(!(is.na(class) & value==0)) %>%
          dplyr::mutate(class = dplyr::if_else(is.na(class),"NA",class))

        data_temp_x <- data_temp_x %>%
          tidyr::spread(x, value)

        data_temp_x[is.na(data_temp_x)] <- 0

        # Absolute Difference
        if(any(grepl("both|absolute|abs",diff_type_x,ignore.case = T))){
          data_temp_absolute_x <- data_temp_x %>%
            dplyr::mutate(!!paste0(scenario_i, "_", diff_text_absolute_x,"_", xRef) := get(xDiff_i) - get(xRef)) %>%
            dplyr::select(-dplyr::one_of(c(xDiff_i, xRef)))
          data_temp_absolute_long_x <- data_temp_absolute_x %>%
            tidyr::gather(key = scenario, value = value, -c(names(data_temp_absolute_x)[!names(data_temp_absolute_x) %in% paste0(scenario_i, "_", diff_text_absolute_x, "_", xRef)])) %>%
            dplyr::mutate(x=xDiff_i)
          data_out <- dplyr::bind_rows(data_out, data_temp_absolute_long_x)
        }

        # Percent Difference
        if(any(grepl("both|percentage|prcnt",diff_type_x,ignore.case = T))){
          data_temp_percent_x <- data_temp_x %>%
            dplyr::mutate(!!paste0(scenario_i, "_", diff_text_percent_x,"_",xRef) := (get(xDiff_i) - get(xRef))*100/get(xRef)) %>%
            dplyr::select(-dplyr::one_of(c(xDiff_i, xRef)))
          data_temp_percent_long_x <- data_temp_percent_x %>%
            tidyr::gather(key = scenario, value = value, -c(names(data_temp_percent_x)[!names(data_temp_percent_x) %in% paste0(scenario_i,"_", diff_text_percent_x,"_", xRef)]))%>%
            dplyr::mutate(x=xDiff_i)
          data_out <- dplyr::bind_rows(data_out, data_temp_percent_long_x)
        }

      }

      data_out <- data_out %>%
        dplyr::mutate(scenario := factor(scenario))

    } else {
      print("xRef provided is not present in the data. Skipping x difference.")
    }

    } # close for(scenario_i in data_out$scenario%>%unique()){

  } else { # Close if(!is.null(xRef)){
  }


  #...........................
  # Close out
  #...........................

  # print("Completed calculate_diff.")

  invisible(data_out)

}
