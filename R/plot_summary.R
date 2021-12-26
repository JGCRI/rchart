#' plot_summary
#'
#' generate summary plot
#' @param data Default = NULL.
#' @param aspect_ratio Default = 0.75. aspect ratio
#' @param size Default = 10. text size
#' @param size_title Default = NULL. size_title text
#' @param theme Default = NULL.
#' @importFrom magrittr %>%
#' @export

plot_summary <- function(data = NULL,
                         aspect_ratio = 0.75,
                         size = 10,
                         size_title = NULL,
                         theme = NULL) {

  #...........................
  # Initialize
  #...........................

  NULL->x->value->scenario

  if(is.null(size_title)){
    size_title = size*1.2
  }


  #...........................
  # Plot
  #...........................

  p1 <- ggplot2::ggplot(data,
                  aes(x=x,y=value,
                      group=scenario,
                      color=scenario)) +
        ggplot2::theme_bw() +
        geom_line(size=2) +
        ylab(NULL) +  xlab(NULL) +
        facet_wrap(
          . ~ param,
          scales = "free",
          ncol = 3,
          labeller = labeller(param = label_wrap_gen(15))
        ) +
        theme(legend.position="top",
              legend.size_title = element_blank(),
              plot.margin=margin(20,20,20,0,"pt"),
              text=element_text(size=size),
              aspect.ratio = aspect_ratio)

  if(!is.null(theme)){p1 <- p1 + theme}

  invisible(p1)

}
