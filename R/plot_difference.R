#' plot_difference
#'
#' generate chart plot for absolute difference and percent difference
#' @param data Default = NULL.
#' @param scenRef Default = NULL.
#' @param scenDiff Default = NULL.
#' @param plot_type Default = "bar". Choose one of "bar" or "line",
#' @param theme Default = NULL
#' @importFrom magrittr %>%
#' @export

plot_difference <- function(data = NULL,
                                scenRef = NULL,
                                scenDiff = NULL,
                                plot_type = "bar",
                                theme = NULL) {


  # data = data_agg_diff
  # scenRef = scenRef
  # scenDiff = scenDiff
  # plot_type = "line"
  # theme = NULL

  #...........................
  # Initialize
  #...........................

  NULL -> value -> param -> x -> scenario

  #...........................
  # Plots
  #...........................

  plist <- list()
  count = 1

  if(!scenRef %in% unique(data$scenario)){scenRef = NULL}
  if(is.null(scenDiff)){
  if(is.null(scenDiff) & !is.null(scenRef)){
    scenDiff = unique(data$scenario)[!unique(data$scenario) %in% scenRef]} else {
    scenDiff = NULL
  }}

  #...........................
  # Plot
  #...........................

  if(!is.null(scenRef) & !is.null(scenDiff)){

  for(i in 1:length(unique(data$param))){

    data <- data %>%
      dplyr::filter(!(is.na(class) & value==0))%>%
      dplyr::mutate(class=dplyr::if_else(is.na(class),"NA",class))

    # Check Color Palettes ....................................
    palAdd <- rep(c("firebrick3","dodgerblue3","forestgreen","black","darkgoldenrod3","darkorchid3","gray50", "darkturquoise"),1000)
    missNames <- unique(data$class)[!unique(data$class) %in%
                                               names(jgcricolors::jgcricol()$pal_all)]
    if (length(missNames) > 0) {
      palAdd <- palAdd[1:length(missNames)]
      names(palAdd) <- missNames
      palCharts <- c(jgcricolors::jgcricol()$pal_all, palAdd)
    } else{
      palCharts <- jgcricolors::jgcricol()$pal_all
    }

    # Prep Data Ref and Diff ....................................
    data_ref <- data %>%
      dplyr::filter(param==unique(data$param)[i], scenario == scenRef)

    data_diff <- data %>%
      dplyr::filter(param==unique(data$param)[i], scenario %in% scenDiff)%>%
      droplevels()

    palCharts <- palCharts[names(palCharts) %in% unique(c(unique(data_diff$class),unique(data_ref$class)))]

    # Plot data_ref ....................................
     p1 <-  ggplot2::ggplot(data_ref%>%
                            droplevels(),
                            ggplot2::aes(x=x,y=value,
                            group=class,
                            fill=class))+
        ggplot2::theme_bw() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(unique(data$param)[i])+
        ggplot2::scale_fill_manual(breaks=names(palCharts),values=palCharts) +
        ggplot2::scale_y_continuous(position = "left") +
        ggplot2::facet_grid(param~scenario, scales="free",switch="y",
                            labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))) +
        ggplot2::geom_bar(position="stack", stat="identity") +
        ggplot2::theme(legend.position="bottom",
            strip.text.y = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank(),
            legend.margin = ggplot2::margin(t =5, r = 0, b = 5, l =0, "pt"),
            legend.key.height = ggplot2::unit(0, "cm"),
            text = ggplot2::element_text(size = 15),
            plot.margin = ggplot2::margin(t = 20, r = 5, b = 0, l = 0, "pt"),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))

     if(!is.null(theme)){p1 <- p1 + theme}

    plist[[count]] <- p1

    # Plot data_diff ....................................
    p2 <-  ggplot2::ggplot(data %>%
                           dplyr::filter(param==unique(data$param)[i], scenario != scenRef)%>%
                           droplevels(),
                           ggplot2::aes(x=x,y=value,
                           group=class,
                           color=class)) +
      ggplot2::theme_bw() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::scale_color_manual(breaks=names(palCharts),values=palCharts) +
      ggplot2::scale_y_continuous(position = "left") +
      ggplot2::facet_grid(param~scenario, scales="free",switch="y",
                          labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))) +
      ggplot2::theme(legend.position="bottom",
            legend.title = ggplot2::element_blank(),
            strip.text.y = ggplot2::element_blank(),
            legend.margin = ggplot2::margin(t =5, r = 0, b = 5, l =0, "pt"),
            legend.key.height = ggplot2::unit(0, "cm"),
            text = ggplot2::element_text(size = 15),
            plot.margin = ggplot2::margin(t = 20, r = 5, b = 0, l = 0, "pt"),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))

    if(grepl("bar",plot_type,ignore.case = T)){
      p2 <- p2 + ggplot2::geom_bar(position="stack", stat="identity")
    }

    if(grepl("line",plot_type,ignore.case = T)){
      p2 <- p2 + ggplot2::geom_line(size=2)
    }

    if(!is.null(theme)){p2 <- p2 + theme}
    plist[[count + 1]] <- p2
    count =  count +2

    }

  plot_out <- cowplot::plot_grid(plotlist = plist, ncol=2, align="v", rel_widths = c(1, length(unique(data$scenario))-1))

  } else {

    print(paste0("scenRef and scenDiff provided do not exist. Skipping difference plot."))
    plot_out = NULL
  }

  invisible(plot_out)

  }
