#' Title
#'
#' @param data_long ppshapr_prep object
#' @param x variable1
#' @param y variable2
#' @param color_feature display color
#' @param smooth geom_smooth
#' @export
#'
PPshapdependence <- function(data_long,x,y=NULL, color_feature=NULL, smooth=TRUE){
  variable <- value <-  finalLeaf <- rfvalue  <- stdfvalue <- color_value <- NULL # due to NSE notes in R CMD check
  leafnum <- max(data_long$finalLeaf)
  yrange <- data_long[variable==x,range(value)]

  plotT <-list()
  for(i in 1:leafnum){
    data_long_leaf <- data_long[finalLeaf==i]
    if (is.null(y)) y <- x

    data0 <- data_long_leaf[variable== y, .(variable, value,rfvalue)]
    if (!is.null(color_feature)) {
      data0$color_value <- data_long_leaf[variable == color_feature, stdfvalue]
      scale_lim <- range(data0$color_value)
    }else{
      scale_lim <- NULL
    }
    plotT[[i]] <- ggplot2::ggplot(
      data = data0,
      ggplot2::aes(x = rfvalue,
                   y = value,
                   color = if (!is.null(color_feature)) color_value else NULL)
    ) +
      ggplot2::geom_jitter(alpha = 0.5) +
      ggplot2:: scale_colour_gradient2(
        low = "blue",
        mid = "yellow",
        high = "red",
        midpoint = mean(scale_lim),
        breaks=scale_lim, labels=c("Low","High")
      )+
      #ggplot2::scale_color_gradient(low = "#FFCC33",high = "#6600CC",breaks=scale_lim, labels=c("Low","High"))+
      ggplot2::labs(title = paste0("finalLeaf : ",i),
                    y = paste0("SHAP value for ", x),
                    x = x,
                    color = if (!is.null(color_feature))
                      paste0(color_feature, "\n","(Feature value)") else NULL)+
      ggplot2::ylim(yrange)+
      ggplot2::theme_bw()+
      ggplot2::geom_hline(yintercept = 0)

  }
  if(smooth) {
    for(i in 1:leafnum){
      plotT[[i]] <- plotT[[i]] +
        ggplot2::geom_smooth(formula = y ~ x,method = "loess", color = "red", size = 0.4, se = FALSE)
    }
  }

  if(!is.null(color_feature)){
    legend = gtable::gtable_filter(ggplot2::ggplotGrob(plotT[[1]]), "guide-box")
    for(k in 1:leafnum){
      plotT[[k]] = plotT[[k]]+ ggplot2::theme(legend.position="none")
    }
    grided <- gridExtra::arrangeGrob(grobs= plotT, ncol=2)
    gg <- gridExtra::grid.arrange(grided,legend,
                                  widths=grid::unit.c(grid::unit(1, "npc") - grid::unit(2, "lines") - legend$width,
                                                      legend$width),
                                  top = grid::textGrob("Dependence plot", gp = grid::gpar(cex = 1.3)),
                                  nrow=1)
  }else{
    gg <- gridExtra::grid.arrange(grobs= plotT, ncol=2)
  }
}
