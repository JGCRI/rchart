#' plot_diff
#'
#' generate chart plot for absolute difference and percent difference
#' @param data Default = NULL.
#' @param scenRef Default = NULL.
#' @param scenDiff Default = NULL.
#' @param theme Default = ggplot2::theme_bw()
#' @importFrom magrittr %>%
#' @export

plot_diff <- function(data = NULL,
                      scenRef = NULL,
                      scenDiff = NULL,
                      theme = ggplot2::theme_bw()) {

  #...........................
  # Initialize
  #...........................

  NULL -> filter -> param -> scenario -> input -> value -> plot_out

  plist <- list()
  count = 1

  if(!scenRef %in% unique(data$scenario)){scenRef = NULL}
  if(is.null(scenDiff) & !is.null(scenRef)){scenDiff = unique(data$scenario)[!unique(data$scenario) %in% scenRef]} else {
    scenDiff = NULL
  }

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
    plist[[count]] <-  ggplot2::ggplot(data_ref%>%
                                         droplevels(),
                                       aes(x=x,y=value,
                                           group=class,
                                           fill=class))+
      theme +
      xlab(NULL) +
      ylab(unique(data$param)[i])+
      scale_fill_manual(breaks=names(palCharts),values=palCharts) +
      scale_y_continuous(position = "left")+
      geom_bar(position="stack", stat="identity") +
      facet_grid(param~scenario, scales="free",switch="y")+
      theme(legend.position="bottom",
            strip.text.y = element_blank(),
            legend.title = element_blank(),
            legend.margin=margin(t =5, r = 0, b = 5, l =0, "pt"),
            legend.key.height=unit(0, "cm"),
            text = element_text(size = 15),
            plot.margin=margin(t = 20, r = 5, b = 0, l = 0, "pt"),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

    # Plot data_diff ....................................
    plist[[count + 1]] <-  ggplot2::ggplot(data %>%
                                       dplyr::filter(param==unique(data$param)[i], scenario != scenRef)%>%
                                       droplevels(),
                                     aes(x=x,y=value,
                                         group=class,
                                         color=class)) +
      theme +
      xlab(NULL) +
      ylab(NULL) +
      scale_color_manual(breaks=names(palCharts),values=palCharts) +
      scale_y_continuous(position = "left")+
      geom_line(size=2)+
      facet_grid(param~scenario, scales="free",switch="y") +
      theme(legend.position="bottom",
            legend.title = element_blank(),
            strip.text.y = element_blank(),
            legend.margin=margin(t =5, r = 0, b = 5, l =0, "pt"),
            legend.key.height=unit(0, "cm"),
            text = element_text(size = 15),
            plot.margin=margin(t = 20, r = 5, b = 0, l = 0, "pt"),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


    count =  count +2
  }

  plot_out <- cowplot::plot_grid(plotlist = plist, ncol=2, align="v", rel_widths = c(1, length(unique(data$scenario))-1))

  } else {

    print(paste0("scenRef and scenDiff provided do not exist. Skipping difference plot."))
    plot_out = NULL
  }

  return(plot_out)

  }
