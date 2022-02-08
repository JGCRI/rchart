#' plot_class_difference
#'
#' generate chart plot for absolute difference and percent difference
#' @param data Default = NULL.
#' @param scenRef Default = NULL.
#' @param scenDiff Default = NULL.
#' @param theme Default = NULL
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param diff_text Default = NULL. Text to remove from diff scenario names.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param diff_type Default = "bar". Choose between "bar" or "line"
#' @param size Default = 1.5. Line size
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @importFrom magrittr %>%
#' @export

plot_class_difference <- function(data = NULL,
                                  scenRef = NULL,
                                  scenDiff = NULL,
                                  theme = NULL,
                                  theme_default = ggplot2::theme_bw(),
                                  diff_text = NULL,
                                  scales = "free",
                                  diff_type = "bar",
                                  size = 1.5,
                                  break_interval = NULL) {


  # data = NULL
  # scenRef = NULL
  # scenDiff = NULL
  # theme = NULL
  # theme_default = ggplot2::theme_bw()
  # diff_text = NULL

  #...........................
  # Initialize
  #...........................

  NULL -> value -> param -> x -> scenario

  #...........................
  # Plots
  #...........................

  plist <- list()
  count = 1

  if(!scenRef %in% unique(data$scenario)){scenRef = NULL}
  if(is.null(scenDiff)){
    if(is.null(scenDiff) & !is.null(scenRef)){
      scenDiff = unique(data$scenario)[!unique(data$scenario) %in% scenRef]} else {
        scenDiff = NULL
      }}

  #...........................
  # Plot
  #...........................

  if(!is.null(scenRef) & !is.null(scenDiff)){

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

      # Prep Data Ref and Diff ....................................
      data_ref <- data %>%
        dplyr::filter(param==unique(data$param)[i], scenario == scenRef)%>%
        droplevels()

      data_diff <- data %>%
        dplyr::filter(param==unique(data$param)[i], scenario %in% scenDiff) %>%
        droplevels()

      palCharts <- palCharts[names(palCharts) %in% c(unique(data_ref$class),unique(data_diff$class))]

      if(!is.null(diff_text)){
        data_diff <- data_diff %>%
          dplyr::mutate(scenario = gsub(paste0("_",scenRef),"",scenario))
      }

      # Plot data_ref ....................................
      p1 <-  ggplot2::ggplot(data_ref,
                             ggplot2::aes(x=x,y=value,
                                          group=class,
                                          fill=class
                                          ))+
        ggplot2::theme_bw() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(unique(data$param)[i])+
        ggplot2::scale_fill_manual(breaks=names(palCharts),values=palCharts) +
        ggplot2::scale_y_continuous(position = "left") +
        ggplot2::facet_grid(param~scenario, scales=scales,switch="y",
                            labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))) +
        ggplot2::geom_bar(position="stack", stat="identity") +
        ggplot2::theme(legend.position="none") +
        theme_default

      # make sure x axis is integers if x data are numeric
      if(is.numeric(data_ref$x)){
        p1 <- p1 + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(
          # don't add more breaks than there are x values
          n = min(5, length(unique(data_ref$x)))
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

      plist[[count]] <- p1

      # Plot empty ....................................
      plist[[count+1]] <- NULL

      # Plot data_diff ....................................
      p2 <-  ggplot2::ggplot(data_diff,
                             ggplot2::aes(x=x,y=value,
                                          group=class)) +
        ggplot2::theme_bw() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::scale_y_continuous(position = "left") +
        ggplot2::facet_grid(param~scenario, scales=scales,switch="y",
                            labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))) +
        ggplot2::theme(legend.position="right") +
        theme_default

      if(diff_type=="bar"){p2 <- p2 +
        ggplot2::geom_bar(ggplot2::aes(fill=class),position="stack", stat="identity") +
        ggplot2::scale_fill_manual(breaks=names(palCharts),values=palCharts)}

      if(diff_type=="line"){p2 <- p2 +
        ggplot2::geom_line(ggplot2::aes(color=class),size=size) +
        ggplot2::scale_color_manual(breaks=names(palCharts),values=palCharts)}

      # make sure x axis is integers if x data are numeric
      if(is.numeric(data_diff$x)){
        p2 <- p2 + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(
          # don't add more breaks than there are x values
          n = min(5, length(unique(data_diff$x)))
        ))
      }
      # add specified break interval if x data are non-numeric
      else{
        if(!is.null(break_interval)){
          p2 <- p2 +
            ggplot2::scale_x_discrete(breaks = function(x){
              x[c(TRUE, rep(FALSE, times = break_interval-1))]})
        }
      }

      if(!is.null(theme)){p2 <- p2 + theme}
      plist[[count + 2]] <- p2
      count =  count +3

    }

    plot_out <- cowplot::plot_grid(plotlist=plist, ncol=3, rel_widths = c(1, -0.25,length(scenDiff)))

  } else {

    print(paste0("scenRef and scenDiff provided do not exist. Skipping difference plot."))
    plot_out = NULL
  }

  invisible(plot_out)

}
