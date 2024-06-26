#' plot_param_difference
#'
#' generate chart plot for absolute difference and percent difference
#' @param data Default = NULL.
#' @param scenRef Default = NULL.
#' @param scenDiff Default = NULL.
#' @param size Default = 1.5. Line size
#' @param theme Default = NULL
#' @param ncol Default = 3. Number of columns.
#' @param theme_default Default = ggplot2::theme_bw(). Default rchart themes.
#' @param facet_label_diff Default = Difference
#' @param diff_text Default = NULL. Text to remove from diff scenario names.
#' @param scales Default = "free". Choose between "free", "free_y", "free_x", "fixed"
#' @param break_interval Default = NULL. Intervals between x breaks starting from first x point.
#' @param include_points Default = FALSE. Add data points to all line charts.
#' @param palette Default = NULL. Named vector with custom palette colors (can include classes, regions, and/or scenarios; scenario colors will be used if provided)
#' @importFrom magrittr %>%
#' @export

plot_param_difference <- function(data = NULL,
                                scenRef = NULL,
                                scenDiff = NULL,
                                size = 1.5,
                                ncol = 3,
                                theme = NULL,
                                theme_default = ggplot2::theme_bw(),
                                facet_label_diff = "Difference",
                                diff_text = NULL,
                                scales = "free",
                                break_interval = NULL,
                                include_points = FALSE,
                                palette = NULL) {


  # data = NULL
  # scenRef = NULL
  # scenDiff = NULL
  # size = 1.5
  # theme = NULL
  # theme_default = ggplot2::theme_bw()
  # facet_label_diff = "Difference"
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

  n_legend_rows <- ceiling(length(unique(scenDiff))/3)

  #...........................
  # Plot
  #...........................

  if(!is.null(scenRef) & !is.null(scenDiff)){

  for(i in 1:length(unique(data$param))){

    # Check Color Palettes ....................................
    palCustom <- palette
    # rename custom palette names to match diff scenarios
    if(!is.null(diff_text) & !is.null(palCustom)){
      names(palCustom) <- dplyr::case_when(
        names(palCustom) != scenRef ~ paste0(names(palCustom), "_",diff_text,"_",scenRef),
        T ~ names(palCustom)
      )
    }
    # remove custom palette names from jgcricolors
    jgcricolors_subset <- jgcricolors::jgcricol()$pal_all[!names(jgcricolors::jgcricol()$pal_all) %in% names(palCustom)]
    # get classes not in the custom palette
    missNamesCustom <- c(scenRef, scenDiff)[!c(scenRef, scenDiff) %in% names(palCustom)]
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


    # Prep Data Ref and Diff ....................................
    data_ref <- data %>%
      dplyr::filter(param==unique(data$param)[i], scenario == scenRef)%>%
      droplevels()

    data_diff <- data %>%
      dplyr::filter(param==unique(data$param)[i], scenario %in% scenDiff) %>%
      droplevels()


    palCharts_ref <- palCharts[names(palCharts) %in% unique(data_ref$scenario)]
    palCharts_diff <- palCharts[names(palCharts) %in% unique(data_diff$scenario)]
    palCharts_ref <- palCharts_ref[names(palCharts_ref)%>%sort()]; palCharts_ref
    palCharts_diff <- palCharts_diff[names(palCharts_diff)%>%sort()]; palCharts_diff

    if(!is.null(diff_text)){
      data_diff <- data_diff %>%
        dplyr::mutate(scenario = gsub(paste0("_",diff_text,"_",scenRef),"",scenario))
      names(palCharts_diff) <- gsub(paste0("_",diff_text,"_",scenRef),"",names(palCharts_diff))
    }

    # Plot data_ref ....................................
     p1 <-  ggplot2::ggplot(data_ref,
                            ggplot2::aes(x=x,y=value,
                            group=scenario,
                            fill=scenario,
                            color=scenario))+
        ggplot2::theme_bw() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(unique(data$param)[i])+
        ggplot2::scale_color_manual(breaks=names(palCharts_ref),values=palCharts_ref) +
        ggplot2::scale_y_continuous(position = "left") +
        ggplot2::facet_grid(param~"Reference", scales=scales,switch="y",
                            labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))) +
        ggplot2::geom_line(size=size) +
        ggplot2::theme(legend.position="bottom") +
        ggplot2::guides(color=ggplot2::guide_legend(nrow=n_legend_rows,byrow=TRUE)) +
        theme_default

     # add points
     if(include_points){
       p1 <- p1+
         ggplot2::geom_point(size = size*3)
     }

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
                           group=scenario,
                           color=scenario)) +
      ggplot2::theme_bw() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::scale_color_manual(breaks=names(palCharts_diff),values=palCharts_diff) +
      ggplot2::geom_line(size=size) +
      ggplot2::scale_y_continuous(position = "left") +
      ggplot2::facet_grid(param~paste0(facet_label_diff), scales=scales,switch="y",
                          labeller = ggplot2::labeller(param = ggplot2::label_wrap_gen(15))) +
      ggplot2::theme(legend.position="bottom")+
      ggplot2::guides(color=ggplot2::guide_legend(nrow=n_legend_rows,byrow=TRUE)) +
      theme_default

    # add points
    if(include_points){
      p2 <- p2+
        ggplot2::geom_point(size = size*3)
    }

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

    plot_out <- cowplot::plot_grid(plotlist=plist, ncol=3, rel_widths = c(1,-0.25,1))

  } else {

    print(paste0("scenRef and scenDiff provided do not exist. Skipping difference plot."))
    plot_out = NULL
  }

  invisible(plot_out)

  }
