#' plot_absolute
#'
#' generate chart plot for absolute value
#' @param data Default = NULL.
#' @param theme Default = NULL.
#' @importFrom magrittr %>%
#' @export
plot_absolute <- function(data = NULL,
                          theme = NULL){

  #...........................
  # Initialize
  #...........................

  NULL -> value -> param -> x -> scenario

  #...........................
  # Plots
  #...........................


  plist <- list()
  count_i = 1

  for(i in 1:length(unique(data$param))){

    data <- data %>%
      dplyr::filter(!(is.na(class) & value==0))%>%
      dplyr::mutate(class=dplyr::if_else(is.na(class),"NA",class))

    # Check Color Palettes
    palAdd <- rep(c("firebrick3","dodgerblue3","forestgreen","black","darkgoldenrod3","darkorchid3","gray50", "darkturquoise"),10000)

    missNames <- unique(data$class)[!unique(data$class) %in%
                                               names(jgcricolors::jgcricol()$pal_all)]
    if (length(missNames) > 0) {
      palAdd <- palAdd[1:length(missNames)]
      names(palAdd) <- missNames
      palCharts <- c(jgcricolors::jgcricol()$pal_all, palAdd)
    } else{
      palCharts <- jgcricolors::jgcricol()$pal_all
    }

    chartz <- data %>%
      dplyr::filter(param==unique(data$param)[i])

    palCharts <- palCharts[names(palCharts) %in% unique(chartz$class)]

    p1 <-  ggplot2::ggplot(chartz%>%
                           droplevels(),
                           ggplot2::aes(x=x,y=value,
                           group=scenario,
                           fill=class))+
      ggplot2::theme_bw() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(unique(data$param)[i])+
      ggplot2::scale_fill_manual(breaks=names(palCharts),values=palCharts) +
      ggplot2::scale_y_continuous(position = "left")+
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::facet_grid(param~scenario, scales="free",switch="y",
                          labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15)))+
      ggplot2::theme(legend.position="bottom",
            strip.text.y = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank(),
            legend.margin = ggplot2::margin(t =5, r = 0, b = 5, l =0, "pt"),
            legend.key.height = ggplot2::unit(0, "cm"),
            text = ggplot2::element_text(size = 15),
            plot.margin= ggplot2::margin(t = 20, r = 5, b = 0, l = 0, "pt"),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))

    if(!is.null(theme)){p1 <- p1 + theme}

    plist[[count_i]] <- p1
    count_i = count_i + 1

  }

  plot_out <- cowplot::plot_grid(plotlist = plist, ncol=1, align="v", rel_widths = c(1, length(unique(data$scenario))-1))

  invisible(plot_out)

}
