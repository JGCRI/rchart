#' plot_line_absolute
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param aspect_ratio Default = 0.75. aspect ratio
#' @param size Default = 1.5. Line size
#' @param size_text Default = 10. text size
#' @param theme Default = NULL.
#' @param ncol Default = 3. Number of columns.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @importFrom magrittr %>%
#' @export

plot_line_absolute <- function(data = NULL,
                               aspect_ratio = 0.75,
                               size = 1.5,
                               size_text = 10,
                               theme = NULL,
                               ncol = 3,
                               scales = "free_y") {

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario

  #...........................
  # Plot
  #...........................

  p1 <- ggplot2::ggplot(data,
                        ggplot2::aes(x=x,y=value,
                        group=scenario,
                        color=scenario)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(size=size) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::facet_wrap(
          . ~ param,
          scales = scales,
          ncol = ncol,
          labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))
        ) +
    ggplot2::theme(legend.position="top",
              legend.title = ggplot2::element_blank(),
              plot.margin = ggplot2::margin(20,20,20,0,"pt"),
              text = ggplot2::element_text(size=size_text),
              aspect.ratio = aspect_ratio,
              axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  if(!is.null(theme)){p1 <- p1 + theme}

  invisible(p1)

}
