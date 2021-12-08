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
#' @examples
#' data(mtcars)
#' n <- nrow(mtcars)
#' tot <- c(1:n)
#' n.train <- round(n*0.8)
#' train <- sample(tot,n.train)
#' test <- tot[-train]
#' Tree.result <- PPTreereg(mpg~.,mtcars[train,],
#'                            final.rule=1,DEPTH=2)
#' PPregcoeff(Tree.result,c(1:3))
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
