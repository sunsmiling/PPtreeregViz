#' waterfallplot
#'
#' @param PPTreeregOBJ PPTreereg object
#' @param testObs testObs
#' @param final.rule final.rule
#' @param method simple or empirical
#' @export
#'
waterfallplot <- function(PPTreeregOBJ, testObs, final.rule = 5, method="simple"){
  icols <- V1 <-  ..icols <- NULL # due to NSE notes in R CMD check
  if(method=="simple"){
    tmp <- ppshapr.simple(PPTreeregOBJ,testObs,final.rule)$dt
  }else{
    tmp <- ppshapr.empirical(PPTreeregOBJ,testObs,final.rule)$dt
  }
  icols = c("finalLeaf")
  plotT<-list()
  for(i in tmp[,..icols][[1]]){
    tmp_PPshapvalue <- t(tmp[, -..icols][i,])%>% as.data.frame() %>% tibble::rownames_to_column() %>%
      dplyr::mutate(V1= round(V1,5))
    plotT[[i]] <- tmp_PPshapvalue %>%
      waterfalls::waterfall(calc_total=TRUE,
                            rect_text_labels = round(tmp[i,-6],3),
                            total_axis_text = "Prediction",
                            total_rect_text_color="black",
                            lines_anchors = c("left", "right"),
                            total_rect_color="goldenrod1")+
      ggplot2::coord_flip()+
      ggplot2::labs(subtitle = paste0("final leaf : ", i ), y="", x="")+
      ggplot2::theme_minimal()
  }
  tg <- grid::textGrob(paste0("Decision plot for test data"),
                       gp = grid::gpar(fontsize = 13, fontface = 'bold'))
  sg <- grid::textGrob(paste0("final.rule : ",final.rule),
                       gp = grid::gpar(fontsize = 10, fontface = 'bold'))
  margin <- grid::unit(0.5, "line")
  grided <- gridExtra::arrangeGrob(grobs= plotT,ncol=2)
  gg <- gridExtra::grid.arrange(tg, sg, grided,
                                heights = grid::unit.c(grid::grobHeight(tg) + 1.2*margin,
                                                       grid::grobHeight(sg) + margin,
                                                       grid::unit(1,"null")))

}


#' decisionplot
#'
#' @param PPTreeregOBJ PPTreereg object
#' @param testObs testObs
#' @param final.rule final.rule
#' @param method method
#' @param varImp shapImp or treeimp
#' @export
#'
decisionplot <- function(PPTreeregOBJ, testObs, final.rule = 5, method="simple",varImp = "shapImp"){
  icols <- V1 <- finalLeaf <- shapsums <- none <- value <- variable <- NULL

  if(method=="simple"){
    PPTreeshapObj_one <- ppshapr.simple(PPTreeregOBJ,testObs,final.rule)$dt %>% dplyr::as_tibble()
  }else{
    PPTreeshapObj_one <- ppshapr.empirical(PPTreeregOBJ,testObs,final.rule)$dt %>% dplyr::as_tibble()
  }
  cutn = length(PPTreeshapObj_one$finalLeaf)
  if(varImp == "shapImp"){
    impOrdering <- PPTreeshapObj_one %>% dplyr::select(-none, -finalLeaf)%>%
      dplyr::summarise_all(var) %>% sort(decreasing = TRUE) %>% colnames()
    impOrdering <- c("none",impOrdering,"finalLeaf")
  }else{
    TreeImp <- PPimportance(PPTreeregOBJ)
    namesImp <- TreeImp$imp_var %>% names()
    impOrdering <- c("none",namesImp[c(TreeImp$imp_var %>% order(decreasing = FALSE))],"finalLeaf")
  }

  PPTreeshapObj_one <- PPTreeshapObj_one[,impOrdering]
  PPTreeshapObj_melt <- PPTreeshapObj_one %>%
    reshape2::melt("finalLeaf") %>% dplyr::arrange(finalLeaf)%>%
    dplyr::group_by(finalLeaf) %>%
    dplyr::mutate(shapsums = cumsum(value)) %>% dplyr::select(-value) %>%
    dplyr::mutate(finalLeaf = as.factor(finalLeaf)) %>%
    dplyr::mutate(idnames="oneObs")
  PPTreeshapObj_melt %>%
    ggplot2::ggplot(ggplot2::aes(x=variable,y=shapsums, group=finalLeaf, color=finalLeaf)) +
    ggplot2::geom_line()+
    ggplot2::scale_color_manual(values = c(RColorBrewer::brewer.pal(cutn, "Set2"))) +
    ggplot2::scale_y_continuous(sec.axis =ggplot2:: dup_axis(name = "Yhat"))+
    ggplot2::coord_flip()+
    ggplot2::scale_x_discrete(expand = c(0,0))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",colour = NA),
                   panel.border = ggplot2::element_rect(fill = NA,colour = "grey20"),
                   panel.grid.major.y =  ggplot2::element_line(colour = "grey92"),
                   panel.grid.minor.y = ggplot2::element_line(colour = "grey92"),
                   panel.grid.major.x =  ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   legend.box.background = ggplot2::element_rect())+
    ggplot2::labs(title=paste0("Decision plot"),
         y = "Model output value", x = "", color = "Final Leaf", linetype = "Final Leaf")
}
