#' Title
#'
#' @param data_long
#' @param ...
#'
#' @return
#' @export
#'
PPshapsummary <- function(data_long, finalLeaf =5,...){
  leafnum <- max(data_long$finalLeaf)
  plotT <-list()

  for(i in 1:leafnum){
    data_long_leaf <- data_long[variable!="none" & finalLeaf==i]
    scale_lim <- c(min(data_long_leaf$stdfvalue),max(data_long_leaf$stdfvalue))
    plotT[[i]] <- data_long_leaf %>%
      dplyr::mutate(variable = forcats::fct_reorder(variable, value, .fun = "var"))%>%
      ggplot2::ggplot(ggplot2::aes(x=variable, y=value, color= stdfvalue)) +
      ggplot2::coord_flip()+
      ggplot2::geom_boxplot(alpha=0.25,width=0.3,color="grey")+
      ggforce::geom_sina(shape = 16, size=2, alpha = .5,maxwidth = 0.7,method = "counts") +
      ggplot2:: scale_colour_gradient2(
        low = "blue",
        mid = "yellow",
        high ="red",
        midpoint = mean(scale_lim),
        breaks=scale_lim, labels=c("Low","High")
      )+
      #ggplot2::scale_color_gradient(low = "#FFCC33",high = "#6600CC",breaks=scale_lim, labels=c("Low","High"))+
      ggplot2::guides(color = ggplot2::guide_colorbar(
        barheight = grid::unit(1, "npc") - grid::unit(4, "line"),
        ticks.linewidth = 0))+
      ggplot2::xlab(NULL)+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     legend.position="right") +
      ggplot2::geom_hline(yintercept = 0)+

      ggplot2::labs(title= paste0("final leaf : ",i),
                    y = "SHAP value (impact on model output)", x = "", color = "Feature\nvalue")
  }
  legend = gtable::gtable_filter(ggplot2::ggplotGrob(plotT[[1]]), "guide-box")
  for(k in 1:leafnum){
    plotT[[k]] = plotT[[k]]+ ggplot2::theme(legend.position="none")
  }

  grided <- gridExtra::arrangeGrob(grobs= plotT, ncol=2)

  gg <- gridExtra::grid.arrange(grided,legend,
                                widths=grid::unit.c(grid::unit(1, "npc") - grid::unit(2, "lines") - legend$width,
                                                    legend$width),
                                top = grid::textGrob("Summary plot", gp = grid::gpar(cex = 1.3)),
                                nrow=1)

}
