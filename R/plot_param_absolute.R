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
#' @param interaction_col_lty Default = NULL. Column to use for interaction plot linetype.
#' @param interaction_col_color Default = NULL. Column to use for interaction plot color.
#' @param palette Default = NULL. Named vector with custom palette colors (can include classes, regions, and/or scenarios; scenario colors will be used if provided)
#' @param linetype Default = NULL. Named vector with custom linetypes (solid lines will be used if not provided)
#' @importFrom magrittr %>%
#' @export

plot_param_absolute <- function(data = NULL,
                               size = 1.5,
                               theme = NULL,
                               theme_default = ggplot2::theme_bw(),
                               ncol = 3,
                               scales = "free_y",
                               break_interval = NULL,
                               include_points = FALSE,
                               palette = NULL,
                               linetype = NULL,
                               interaction_col_lty = NULL,
                               interaction_col_color = NULL) {

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
  missNamesCustom <- unique(data$scenario)[!unique(data$scenario) %in% names(palCustom)]
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

  palCharts <- palCharts[names(palCharts) %in% unique(data$scenario)]
  palCharts <- palCharts[names(palCharts)%>%sort()]; palCharts

  # Check Line Types ....................................
  ltyCustom <- linetype
  # check classes not in the custom linetypes
  missNames <- unique(data$scenario)[!unique(data$scenario) %in% names(ltyCustom)]
  # linetype options
  ltyAdd <- rep(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), 10)

  if (length(missNames) > 0) {
    # assign extra colors to nonspecified classes
    ltyAdd <- ltyAdd[1:length(missNames)]
    names(ltyAdd) <- missNames
    ltyCharts <- c(ltyCustom, ltyAdd)
  } else{
    ltyCharts <- ltyCustom
  }

  # Interactions
  if(!is.null(interaction_col_lty) & !is.null(interaction_col_color) & length((data$scenario)%>%unique())>1){
    if(any(interaction_col_lty %in% names(data)) & any(interaction_col_color %in% names(data))){

      # check if customized colors are corresponding to all the items
      if(!is.null(palCustom) & all(unique(data[[interaction_col_color]]) %in% names(palCustom))){
        palCharts <- palCustom[names(palCustom) %in% unique(data[[interaction_col_color]])]
      } else {
        message(paste0('Custom color palette does not identify colors for one or more of the followings: ',
                       paste0(unique(data[[interaction_col_color]]), collapse = ', '),
                       '. Using ramdom color palette instead.'))
        palCharts <- palCharts %>% as.vector()
      }

      # check if customized line types are corresponding to all the items
      if(!is.null(ltyCustom) & all(unique(data[[interaction_col_lty]]) %in% names(ltyCustom))){
        ltyCharts <- ltyCustom[names(ltyCustom) %in% unique(data[[interaction_col_lty]])]
      } else {
        message(paste0('Custom linetype does not identify line types for one or more of the followings: ',
                       paste0(unique(data[[interaction_col_lty]]), collapse = ', '),
                       '. Using ramdom line types instead.'))
        ltyCharts <- ltyCharts %>% as.vector()
      }

      data$interaction <- interaction(data[[interaction_col_lty]],data[[interaction_col_color]])

      # Plot
      p1 <- ggplot2::ggplot(data,
                            ggplot2::aes_string(x="x",y="value",
                                               group="interaction",
                                               color=interaction_col_color,
                                               lty=interaction_col_lty)) +
        ggplot2::theme_bw() +
        theme_default +
        ggplot2::scale_color_manual(values=palCharts) +
        ggplot2::scale_linetype_manual(values=ltyCharts) +
        ggplot2::geom_line(size=size) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme(legend.position="bottom")

    } else {
      print(paste0("Column names for interaction_col_lty or interaction_col_color provided do not exist in data. Skipping interaction"))
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

      }
  } else {
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
  }

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
