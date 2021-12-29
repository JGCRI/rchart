#' plot_reg_absolute
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param size Default = 1.5. Line size
#' @param theme Default = NULL.
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @importFrom magrittr %>%
#' @export

plot_reg_absolute <- function(data = NULL,
                               size = 1.5,
                               theme = NULL,
                               theme_default = ggplot2::theme_bw(),
                               scales = "free_y") {

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario

  #...........................
  # Plot
  #...........................

  # Check Color Palettes ....................................
  palAdd <- rep(jgcricolors::jgcricol()$pal_basic,1000)
  missNames <- unique(data$class)

  if (length(missNames) > 0) {
    palAdd <- palAdd[1:length(missNames)]
    names(palAdd) <- missNames
    palCharts <- c(jgcricolors::jgcricol()$pal_all, palAdd)
  } else{
    palCharts <- jgcricolors::jgcricol()$pal_all
  }

  palCharts <- palCharts[names(palCharts) %in% unique(data$class)]


  # Plot
  p1 <- ggplot2::ggplot(data,
                        ggplot2::aes(x=x,y=value,
                        group=class,
                        color=class)) +
    ggplot2::theme_bw() +
    theme_default +
    ggplot2::scale_color_manual(breaks=names(palCharts),values=palCharts) +
    ggplot2::geom_line(size=size) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::facet_grid(
          param ~ scenario,
          scales = scales,
          labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15)),
          switch='y'
        ) +
    ggplot2::theme(legend.position="bottom",
                   strip.text.y = ggplot2::element_text(color="black",angle=270, vjust=1.5, size=size_text),
                   strip.background.y = ggplot2::element_blank(),
                   strip.placement = "outside")

  if(!is.null(theme)){p1 <- p1 + theme}
  invisible(p1)

}
