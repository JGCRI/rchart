#' plot_class_absolute
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param theme Default = NULL.
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param size_text Default = 15. Text size
#' @importFrom magrittr %>%
#' @export

plot_class_absolute <- function(data = NULL,
                               theme = NULL,
                               theme_default = ggplot2::theme_bw(),
                               ncol = 3,
                               scales = "free_y",
                               size_text = 15) {

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario

  #...........................
  # Plot
  #...........................


  plist <- list()
  count = 1

  for(i in 1:length(unique(data$param))){

    # Check Color Palettes ....................................
    palAdd <- rep(jgcricolors::jgcricol()$pal_16,1000)
    missNames <- unique(data$class)

    if (length(missNames) > 0) {
      palAdd <- palAdd[1:length(missNames)]
      names(palAdd) <- missNames
      palCharts <- c(jgcricolors::jgcricol()$pal_all, palAdd)
    } else{
      palCharts <- jgcricolors::jgcricol()$pal_all
    }

    data_plot <- data %>%
      dplyr::filter(param==unique(data$param)[i])

   palCharts <- palCharts[names(palCharts) %in% unique(data_plot$class)]

  # Plot
  p1 <- ggplot2::ggplot(data_plot,
                        ggplot2::aes(x=x,y=value,
                                     group=class,
                                     fill=class)) +
    ggplot2::theme_bw() +
    theme_default +
    ggplot2::scale_fill_manual(breaks=names(palCharts),values=palCharts) +
    ggplot2::geom_bar(position="stack", stat="identity") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::facet_grid(
      param ~ scenario,
      scales = scales,
      labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15)),
      switch='y'
    ) +
    ggplot2::theme(legend.position="right",
                   strip.text.y = ggplot2::element_text(color="black",angle=270, vjust=1.5, size=size_text),
                   strip.background.y = ggplot2::element_blank(),
                   strip.placement = "outside");p1

  if(!is.null(theme)){p1 <- p1 + theme}

  plist[[count]] <- p1

  count <- count + 1

  }

  plot_out <- cowplot::plot_grid(plotlist=plist, ncol=1)

  invisible(plot_out)

}
