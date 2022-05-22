#' plot_param_absolute
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param size Default = 1.5. Line size
#' @param theme Default = NULL.
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @param include_points Default = FALSE. Add data points to all line charts.
#' @importFrom magrittr %>%
#' @export

plot_param_absolute <- function(data = NULL,
                               size = 1.5,
                               theme = NULL,
                               theme_default = ggplot2::theme_bw(),
                               ncol = 3,
                               scales = "free_y",
                               break_interval = NULL,
                               include_points = FALSE) {

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
  palCharts <- palCharts[names(palCharts)%>%sort()]; palCharts


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
    ggplot2::theme(legend.position="bottom")

  # facet wrap by param if more than one parameter
  if(length(unique(data$param)) > 1) {
    p1 <- p1 +
      ggplot2::facet_wrap(
        . ~ param,
        scales = scales,
        ncol = ncol,
        labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))
      )
  }
  else{
    p1 <- p1 +
      ggplot2::ylab((unique(data$param))[1])
  }

  # add points
  if(include_points){
    p1 <- p1+
      ggplot2::geom_point(size = size*3)
  }

  # make sure x axis is integers if x data are numeric
  if(is.numeric(data$x)){
    p1 <- p1 + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(
      # don't add more breaks than there are x values
      n = min(5, length(unique(data$x)))
    ))
  }
  # add specified break interval if x data are non-numeric
  else{
    if(!is.null(break_interval)){
      p1 <- p1 +
        ggplot2::scale_x_discrete(breaks = function(x){
          x[c(TRUE, rep(FALSE, times = break_interval-1))]})
    }
  }

  if(!is.null(theme)){p1 <- p1 + theme}
  invisible(p1)


}
