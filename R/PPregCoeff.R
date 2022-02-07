#' predict PPtree
#'
#' Predict class for the test set and calculate prediction error
#' After finding tree structure, predict class for the test set and calculate
#' prediction error.
#' @usage PPregcoeff(PPTreeregOBJ,c,...)
#' @param PPTreeregOBJ PPTreereg object
#' @param c ...
#' @param ... arguments to be passed to methods
#' @return coef_result coefficients of nodes
#' @export
#' @keywords tree
PPregcoeff<-function(PPTreeregOBJ,c,...){
  PPclassOBJ <-PPTreeregOBJ$Tree.result
  TS <- PPclassOBJ$Tree.Struct
  Alpha <- PPclassOBJ$projbest.node

  origdata <- PPclassOBJ$origdata
  if(length(c)==1){
    coef_result <- round(Alpha[TS[c, 4], ],4)
    coef_result <- as.data.frame(coef_result)
    colnames(coef_result)<-paste0("node",c)
  }else{
    coef_result <- t(round(Alpha[TS[c, 4], ],4))
    colnames(coef_result)<-  c(paste0("node",c))
  }

  return(coef_result)

}
