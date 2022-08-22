#' waterfallplot
#'
#' @param PPTreeregOBJ PPTreereg object
#' @param testObs testObs
#' @param final.rule final.rule
#' @param method simple or empirical
#' @param final.leaf final leaf
#' @export
#'
waterfallplot <- function(PPTreeregOBJ, testObs, final.rule = 5, method="simple", final.leaf = NULL){
  icols <- V1 <-  ..icols <- rowname <- finalLeaf <- NULL # due to NSE notes in R CMD check
  if(method=="simple"){
    tmp <- ppshapr.simple(PPTreeregOBJ,testObs,final.rule,final.leaf = final.leaf)$dt
  }else{
    tmp <- ppshapr.empirical(PPTreeregOBJ,testObs,final.rule)$dt
  }
  PPshapvalue <- tmp %>% as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::select(-c(rowname, finalLeaf)) %>%
    reshape2::melt(id.vars = NULL) #  tmp
  plotT <- PPshapvalue %>% waterfalls::waterfall(calc_total=TRUE,
                                                 rect_text_labels = round(PPshapvalue$value, 3),
                                                 total_rect_text = round(sum(PPshapvalue$value),5),
                                                 total_axis_text = "Prediction",
                                                 total_rect_text_color="black",
                                                 lines_anchors = c("left", "right"),
                                                 total_rect_color="goldenrod1")+
    ggplot2::coord_flip()+
    ggplot2::labs(
      title = "Decision plot for test data",
      subtitle = paste0("final leaf = ", tmp[,"finalLeaf"]),
      y="",
      x="")+
    ggplot2::theme_minimal()

  plotT
}



#' decisionplot_new
#'
#' @param PPTreeregOBJ PPTreereg object
#' @param testObs testObs
#' @param final.rule final.rule
#' @param method method
#' @param varImp shapImp or treeimp
#' @param final.leaf final.leaf
#' @param Yrange Yrange
#' @export
#'
decisionplot <- function(PPTreeregOBJ, testObs, final.rule = 5, method="simple",varImp = "shapImp",final.leaf = NULL, Yrange = FALSE){

  icols <- V1 <- finalLeaf <- shapsums <- none <- value <- id <- yhat <- ..impOrdering <- variable <- NULL
  if(method=="simple"){
    if(nrow(testObs)==1){
      ppSHAP <- ppshapr.simple(PPTreeregOBJ,testObs,final.rule,final.leaf = final.leaf)$dt %>% dplyr::as_tibble()
    }else if(nrow(testObs)>1){
      ppSHAP <- c()
      testNum = nrow(testObs)
      for(p in 1:testNum){
        ppSHAP[[p]] <-  ppshapr.simple(PPTreeregOBJ,testObs[p,], final.rule = final.rule, final.leaf = final.leaf)$dt
        ppSHAP[[p]][,':='(id,p)]
      }
      ppSHAP <- data.table::rbindlist(ppSHAP)
    }else{
      print("nrow(testObs) < 1")
    }
  }else{
    ppSHAP <- ppshapr.empirical(PPTreeregOBJ,testObs,final.rule)$dt %>% dplyr::as_tibble()###
    ####more
    #ppSHAP <- ppshapr.empirical(PPTreeregOBJ,testObs,final.rule,final.leaf = final.leaf)$dt %>% dplyr::as_tibble()###
    ####more
  }


  if(varImp == "shapImp"){
    if(nrow(ppSHAP)==1){
      namesImp <- ppSHAP %>% dplyr::select(-none, -finalLeaf)
      shapImp_value <- namesImp %>% as.numeric()
      namesImp <- namesImp %>% colnames()
      impOrdering <-  c("none", namesImp[c(shapImp_value %>% abs()%>%  order(decreasing = FALSE))],"finalLeaf")
      ppSHAP_reorder <- ppSHAP[,impOrdering]
      ppSHAP_reorder$id <- 1
    }else{
      namesImp <- ppSHAP %>% dplyr::select(-none, -finalLeaf, -id)
      shapImp_value <- namesImp %>% dplyr::summarise_all(var) %>% as.numeric()
      namesImp <- namesImp %>% colnames()
      impOrdering <- c("none", namesImp[c(shapImp_value %>% abs()%>%  order(decreasing = FALSE))],"finalLeaf","id")
      ppSHAP_reorder <- ppSHAP[,..impOrdering]
    }
  }else{
    TreeImp <- PPimportance(PPTreeregOBJ)
    namesImp <- TreeImp$imp_var %>% names()
    impOrdering <- c("none",namesImp[c(TreeImp$imp_var %>% order(decreasing = FALSE))],"finalLeaf")
    ppSHAP_reorder <- ppSHAP[,..impOrdering]
  }
  ppSHAP_melt <- ppSHAP_reorder %>%
    dplyr::select(-finalLeaf) %>%
    reshape2::melt("id") %>% dplyr::arrange(id)%>%
    dplyr::group_by(id) %>%
    dplyr::mutate(shapsums = cumsum(value)) %>% dplyr::select(-value) %>%
    dplyr::mutate(id = as.factor(id))%>%
    dplyr::mutate(yhat = dplyr::last(shapsums))

  Yhat_var <- var(c(PPTreeregOBJ$origY, predict.PPTreereg(PPTreeregOBJ, testObs = testObs, final.rule = final.rule)))
  Yhat_range <- range(c(PPTreeregOBJ$origY, predict.PPTreereg(PPTreeregOBJ, testObs = testObs, final.rule = final.rule)))
  Yhat_range <- round(range(c(Yhat_range-0.1*Yhat_var, Yhat_range+0.1*Yhat_var)),4)


  p <- ppSHAP_melt %>%
    ggplot2::ggplot(ggplot2::aes(x=variable,y=shapsums, group=id, color=yhat)) +
    ggplot2::geom_line()+
    ggplot2::scale_colour_stepsn(colours = c("#3300FF", "#FF0033"),
                                 breaks = round(PPTreeregOBJ$cut.class,4),
                                 guide = ggplot2::guide_coloursteps(show.limits = TRUE),
                                 limits = Yhat_range)+
    ggplot2::coord_flip()+
    ggplot2::scale_x_discrete(expand = c(0,0))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",colour = NA),
                   panel.border = ggplot2::element_rect(fill = NA,colour = "grey20"),
                   panel.grid.major.y =  ggplot2::element_line(colour = "grey92"),
                   panel.grid.minor.y = ggplot2::element_line(colour = "grey92"),
                   panel.grid.major.x =  ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.key.width = ggplot2::unit(3, "line")
    )+
    ggplot2::labs(title=paste0("Decision plot"),
                  y = "Model output value", x = "")

  p+ list(
    if(Yrange){
      ggplot2::scale_y_continuous(sec.axis = ggplot2:: dup_axis(name = "Yhat"), limits = Yhat_range)
    }
    else{
      ggplot2::scale_y_continuous(sec.axis = ggplot2:: dup_axis(name = "Yhat"))
    }
  )
}

