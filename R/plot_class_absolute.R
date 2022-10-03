#' plot_class_absolute
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param theme Default = NULL.
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param size_text Default = 15. Text size
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @param col_dim Default = "scenario". Choose between "scenario" or "region". Column facet variable.
#' @param row_dim Default = "param". Column facet variable.
#' @param summary_line Default = FALSE. Add parameter summary line to all bar charts.
#' @param data_agg Default = NULL. Aggregated param data for the summary line.
#' @param size Default = 1.5. Line size for summary lines
#' @importFrom magrittr %>%
#' @export

plot_class_absolute <- function(data = NULL,
                               theme = NULL,
                               theme_default = ggplot2::theme_bw(),
                               ncol = 3,
                               scales = "free_y",
                               size_text = 15,
                               size = 1.5,
                               break_interval = NULL,
                               col_dim = "scenario",
                               row_dim = "param",
                               summary_line = F,
                               data_agg = NULL) {



  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario->param

  #...........................
  # Plot
  #...........................

  plist <- list()
  count = 1

  for(i in 1:length(unique(data[[row_dim]]))){

    # Check Color Palettes ....................................
    palAdd <- rep(jgcricolors::jgcricol()$pal_16,1000)
    missNames <- unique(data$class)[!unique(data$class) %in% names(jgcricolors::jgcricol()$pal_all)]

    if (length(missNames) > 0) {
      palAdd <- palAdd[1:length(missNames)]
      names(palAdd) <- missNames
      palCharts <- c(jgcricolors::jgcricol()$pal_all, palAdd)
    } else{
      palCharts <- jgcricolors::jgcricol()$pal_all
    }

    data_plot <- data %>%
      dplyr::filter(get(row_dim)==unique(data[[row_dim]])[i])
    data_agg_plot = data_agg %>%
      dplyr::filter(get(row_dim)==unique(data[[row_dim]])[i])

   palCharts <- palCharts[names(palCharts) %in% unique(data_plot$class)]
   palCharts <- palCharts[names(palCharts)%>%sort()]; palCharts

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
    ggplot2::theme(legend.position="right",
                   strip.text.y = ggplot2::element_text(color="black",angle=270, vjust=1.5, size=size_text),
                   strip.background.y = ggplot2::element_blank(),
                   strip.placement = "outside");p1

  #if(length(unique(data_plot$class))==1){p1 <- p1 + guides(fill="none");p1}

  # if multiple parameters and scenarios/regions, facet wrap by param and scenario/region
  if(length(unique(data[[col_dim]])) > 1 & length(unique(data[[row_dim]])) > 1){
    p1 <- p1 +
      ggplot2::facet_grid(
        get(row_dim) ~ get(col_dim),
        scales = scales,
        labeller = ggplot2::labeller(row_dim = ggplot2::label_wrap_gen(15)),
        switch='y'
      )
  } else if(length(unique(data[[col_dim]])) > 1){
    # if one row_dim and multiple col_dims, facet wrap by only col_dim
    # and add row_dim as ylab
    p1 <- p1 +
      ggplot2::facet_grid(
        ~ get(col_dim),
        scales = scales
      ) +
      ggplot2::ylab((unique(data[[row_dim]]))[1])
  } else if(length(unique(data[[row_dim]])) > 1){
    # if one col dim and multiple row dims, facet wrap only by row dim
    p1 <- p1 +
      ggplot2::facet_grid(
        ~ get(row_dim),
        scales = scales
      )
  } else{
    # if one row_dim and one col_dim, just add row_dim as ylab
    p1 <- p1 +
      ggplot2::ylab((unique(data[[row_dim]]))[1])
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

  # add line for param total if desired
  if(summary_line){
    p1 <- p1 +
      ggplot2::geom_line(data = data_agg_plot,
                         ggplot2::aes(x = x, y = value,
                                      fill = NULL, group = NULL),
                         size = size)
  }

  if(!is.null(theme)){p1 <- p1 + theme}

  plist[[count]] <- p1

  count <- count + 1

  }

  # return just the single plot if only one parameter
  if(length(unique(data[[row_dim]])) == 1){
    invisible(p1)
  } else{
    # otherwise, return grid of parameters (cannot be modified later)
    plot_out <- cowplot::plot_grid(plotlist=plist, ncol = 1, align = "hv", axis = "lr")
    invisible(plot_out)
  }

}
