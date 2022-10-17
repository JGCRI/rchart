#' plot_reg_absolute
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param size Default = 1.5. Line size
#' @param theme Default = NULL.
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed",
#' @param size_text Default = 15. Text size
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @param include_points Default = FALSE. Add data points to all line charts.
#' @param palette Default = NULL. Named vector with custom palette colors (can include classes, regions, and/or scenarios; region colors will be used if provided)
#' @importFrom magrittr %>%
#' @export

plot_reg_absolute <- function(data = NULL,
                              size = 1.5,
                              theme = NULL,
                              theme_default = ggplot2::theme_bw(),
                              scales = "free_y",
                              size_text = 15,
                              break_interval = NULL,
                              include_points = FALSE,
                              palette = NULL) {

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario

  #...........................
  # Plot
  #...........................

  # Check Color Palettes ....................................
  palCustom <- palette
  # remove custom palette names from jgcricolors
  jgcricolors_subset <- jgcricolors::jgcricol()$pal_all[!names(jgcricolors::jgcricol()$pal_all) %in% names(palCustom)]
  # get classes not in the custom palette
  missNamesCustom <- unique(data$class)[!unique(data$class) %in% names(palCustom)]
  # get classes not in the custom palette or in jgcricolors
  missNames <- missNamesCustom[!missNamesCustom %in% names(jgcricolors::jgcricol()$pal_all)]
  # get extra colors to use for nonspecified classes
  palAdd <- rep(jgcricolors::jgcricol()$pal_basic,1000)


  if (length(missNames) > 0) {
    # assign extra colors to nonspecified classes
    palAdd <- palAdd[1:length(missNames)]
    names(palAdd) <- missNames
    palCharts <- c(palCustom, jgcricolors_subset, palAdd)
  } else{
    palCharts <- c(palCustom, jgcricolors_subset)
  }

  palCharts <- palCharts[names(palCharts) %in% unique(data$class)]
  palCharts <- palCharts[names(palCharts)%>%sort()]; palCharts

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
    ggplot2::theme(legend.position="bottom",
                   strip.text.y = ggplot2::element_text(color="black",angle=270, vjust=1.5, size=size_text),
                   strip.background.y = ggplot2::element_blank(),
                   strip.placement = "outside")

  # if multiple parameters and scenarios, facet wrap by param and scenario
  if(length(unique(data$param)) > 1 & length(unique(data$scenario)) > 1){
    p1 <- p1 +
      ggplot2::facet_grid(
        param ~ scenario,
        scales = scales,
        labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15)),
        switch='y'
      )
  } else if(length(unique(data$scenario)) > 1){
    # if one parameter and multiple scenarios, facet wrap by only scenario
    # and add parameter as ylab
    p1 <- p1 +
      ggplot2::facet_wrap(
        ~ scenario,
        scales = scales
      ) +
      ggplot2::ylab((unique(data$param))[1])
  } else if(length(unique(data$param)) > 1){
    # if one scenario and multiple parameters, facet wrap only by parameter
    p1 <- p1 +
      ggplot2::facet_wrap(
        ~ param,
        scales = scales
      )
  } else{
    # if one parameter and one scenario, just add parameter as ylab
    p1 <- p1 +
      ggplot2::ylab((unique(data$param))[1])
  }

  # add points
  if(include_points){
    p1 <- p1 +
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
