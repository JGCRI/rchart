#' add_missing
#'
#' Used to add missing data to input files and customize format
#' @param data dataframe to test and convert
#' @param interaction_col_lty Default = NULL. Column to use for interaction plot linetype.
#' @param interaction_col_color Default = NULL. Column to use for interaction plot color.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

add_missing <- function(data,
                        interaction_col_lty = NULL,
                        interaction_col_color = NULL){
  NULL -> year -> aggregate -> scenario -> subRegion -> param -> x -> value -> region

  if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario = as.character(scenario),
                               scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
  if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(scenario = as.character(scenario),
                               scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
  if(!any(grepl("\\<region\\>",names(data),ignore.case = T))){
    data<-data%>%dplyr::mutate(region="region")}else{
    data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<region\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(region = as.character(region),
                               region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
  if(!any(grepl("\\<subRegion\\>",names(data),ignore.case = T))){
    if(!any(grepl("\\<region\\>",names(data),ignore.case = T))){
      data<-data%>%dplyr::mutate(subRegion="subRegion")
    }else{data<-data%>%dplyr::mutate(subRegion="subRegion")}}else{
    data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subRegion\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(subRegion = as.character(subRegion),
                               subRegion=dplyr::case_when(is.na(subRegion)~"subRegion",TRUE~subRegion))}
  if(!any(grepl("\\<subRegions\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subRegions\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(subRegion = as.character(subRegion),
                               subRegion=dplyr::case_when(is.na(subRegion)~"subRegion",TRUE~subRegion))}
  if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
    data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(param = as.character(param),
                               param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
  if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(param = as.character(param),
                               param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
  if(!any(grepl("\\<value\\>",names(data),ignore.case = T))){stop("Data must have 'value' column.")}else{
    data <- data %>% dplyr::rename(!!"value" := (names(data)[grepl("\\<value\\>",names(data),ignore.case = T)])[1])
    data$value = as.numeric(data$value)
    data<-data%>%dplyr::mutate(value=dplyr::case_when(is.na(value)~0,TRUE~value))}
  if(!any(grepl("\\<values\\>",names(data),ignore.case = T))){}else{
    data <- data %>% dplyr::rename(!!"value" := (names(data)[grepl("\\<values\\>",names(data),ignore.case = T)])[1])
    data$value = as.numeric(data$value)
    data<-data%>%dplyr::mutate(value=dplyr::case_when(is.na(value)~0,TRUE~value))}
  if(!"x"%in%names(data)){
    if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}

  if(!any(grepl("\\<aggregate\\>",names(data),ignore.case = T))){
    if(is.null(aggregate)){data<-data%>%dplyr::mutate(aggregate="sum")}else{
      data<-data%>%dplyr::mutate(aggregate = as.character(aggregate),
                                 aggregate="sum")}
  }else{
    data <- data %>% dplyr::rename(!!"aggregate" := (names(data)[grepl("aggregate",names(data),ignore.case = T)])[1])
    data<-data%>%dplyr::mutate(aggregate = as.character(aggregate),
                               aggregate=dplyr::case_when(is.na(aggregate)~"sum",
                                                          TRUE~aggregate))}

  if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){
    if(!any(grepl("class",names(data),ignore.case = T))){
      data<-data%>%dplyr::mutate(class="class")} else {
        data <- data %>% dplyr::rename(!!"class" := (names(data)[grepl("class",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(class = as.character(class),
                                   class=dplyr::case_when(is.na(class)~"class",TRUE~class))}
  }

  if(!is.null(interaction_col_lty) & !is.null(interaction_col_color) & length((data$scenario)%>%unique())>1){
    if(any(interaction_col_lty %in% names(data)) & any(interaction_col_color %in% names(data))){
      data[[interaction_col_lty]] <- as.character(data[[interaction_col_lty]])
      data[[interaction_col_color]] <- as.character(data[[interaction_col_color]])
    }
  }

  data <- data %>%
    dplyr::select(scenario,region,subRegion,param,class,x,aggregate,value,interaction_col_lty, interaction_col_color)
  if(!is.numeric(data$x)){
    # convert x to factor so values won't be re-ordered alphabetically
    data$x <- factor(data$x, levels=unique(data$x))
  }
  return(data)
}
