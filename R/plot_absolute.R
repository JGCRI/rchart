#' plot_absolute
#'
#' generate chart plot for absolute value
#' @param data Default = NULL.
#' @param theme ggplot theme to use
#' @importFrom magrittr %>%
#' @export
plot_absolute <- function(data = NULL,
                          theme = ggplot2::theme_bw()){

  NULL -> filter -> param -> scenario -> input -> value -> x

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

    plist[[count_i]] <-  ggplot2::ggplot(chartz%>%
                                           droplevels(),
                                         aes(x=x,y=value,
                                             group=scenario,
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

    count_i = count_i + 1

  }
  cowplot::plot_grid(plotlist = plist, ncol=1, align="v", rel_widths = c(1, length(unique(data$scenario))-1))
}
