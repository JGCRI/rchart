#' plot_param_absolute
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param size Default = 1.5. Line size
#' @param theme Default = NULL.
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param breaks_x Default = NULL. Number of breaks for x.
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @importFrom magrittr %>%
#' @export

plot_param_absolute <- function(data = NULL,
                               size = 1.5,
                               theme = NULL,
                               theme_default = ggplot2::theme_bw(),
                               ncol = 3,
                               scales = "free_y",
                               breaks_x = NULL,
                               break_interval = NULL) {

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario

  #...........................
  # Plot
  #...........................

  # Check Color Palettes ....................................
  palAdd <- rep(jgcricolors::jgcricol()$pal_basic,1000)
  missNames <- unique(data$scenario)

  if (length(missNames) > 0) {
    palAdd <- palAdd[1:length(missNames)]
    names(palAdd) <- missNames
    palCharts <- c(jgcricolors::jgcricol()$pal_all, palAdd)
  } else{
    palCharts <- jgcricolors::jgcricol()$pal_all
  }

  palCharts <- palCharts[names(palCharts) %in% unique(data$scenario)]

  # calculate break interval if breaks_x is given
  if(!is.null(breaks_x)){
    break_interval <- length(unique(data$x)) %/% breaks_x
  }

  # Plot
  p1 <- ggplot2::ggplot(data,
                        ggplot2::aes(x=x,y=value,
                        group=scenario,
                        color=scenario)) +
    ggplot2::theme_bw() +
    theme_default +
    ggplot2::scale_color_manual(breaks=names(palCharts),values=palCharts) +
    ggplot2::geom_line(size=size) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::facet_wrap(
          . ~ param,
          scales = scales,
          ncol = ncol,
          labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))
        ) +
    ggplot2::theme(legend.position="bottom")

  if(!is.null(breaks_x)|!is.null(break_interval)){
    p1 <- p1 +
      ggplot2::scale_x_discrete(breaks = function(x){
        x[c(TRUE, rep(FALSE, times = break_interval-1))]})
  }

  if(!is.null(theme)){p1 <- p1 + theme}
  invisible(p1)


}
