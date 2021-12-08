#' Party Object for projection pursuit regression tree
#'
#' Change to party object, only conducted when final.rule = 1
#' @usage as_party(PPTreeregOBJ, Rule=1, data=TRUE,...)
#' @param PPTreeregOBJ PPTreereg object
#' @param Rule split rule
#'             1: mean of two group means
#'             2: weighted mean of two group means - weight with group size
#'             3: weighted mean of two group means - weight with group sd
#'             4: weighted mean of two group means - weight with group se
#'             5: mean of two group medians
#'             6: weighted mean of two group medians - weight with group size
#'             7: weighted mean of two group median - weight with group IQR
#'             8: weighted mean of two group median - weight with group IQR
#'                                                    and group size
#'             9: cutoff that minimize error rates in each node
#' @param data a (potentially empty) data.frame.
#' @param ... arguments to be passed to methods
#' @export

as_party<-function(PPTreeregOBJ, Rule=1, data=TRUE,...){
  ff <- data.frame(PPTreeregOBJ$Tree.result$Tree.Struct)
  ff <- ff[c(1,3,2,5,4,7,6),]

  n <- nrow(ff)
  if (n == 1)
    return(partykit::partynode(as.integer(1)))
  is.leaf <- (ff$Index == 0)
  ncomplete<-rep(2,n)
  ncomplete[is.leaf]<-0
  index<-cumsum(c(1,ncomplete+1*(!is.leaf)))
  primary<-numeric(n)
  primary[!is.leaf]<-index[c(!is.leaf,FALSE)]
  mf<-PPTreeregOBJ$Tree.result$origdata%*%t(PPTreeregOBJ$Tree.result$projbest.node)
  rownames(mf)<-rownames(PPTreeregOBJ$Tree.result$origdata)
  colnames(mf)<-paste("proj",1:ncol(mf),sep="")
  mf<-data.frame(mf)

  mf$origclass <- PPTreeregOBJ$Tree.result$origclass
  mf$predict <- predict.PPTreereg(PPTreeregOBJ, final.rule = 1)
  mf<-cbind(mf,PPTreeregOBJ$Tree.result$origdata)
  mf<-data.frame(mf)


  PPtreereg_fitted <- function() {
    ret <- as.data.frame(matrix(nrow = NROW(mf), ncol = 0))
    ret[["(fitted)"]] <- apply(matrix(as.numeric(names(predict.PPTreereg(PPTreeregOBJ))),ncol=1),1,
                               function(x) which((data.frame(ff)$R.node.ID==x)*is.leaf==1))

    ret[["(response)"]] <- PPTreeregOBJ$origclass
    ret
  }

  fitted <- PPtreereg_fitted()
  PPtreereg_kids <- function(i) {
    if (is.leaf[i])
      return(NULL)
    else
      return(c(ff[i,c(3,2)]))
  }

  PPtreereg_split <- function(j) {
    if (j < 1)
      return(NULL)
    idj <- as.integer(ff$Coef.ID[j])
    ret <- partykit::partysplit(varid = idj,
                                breaks = round(as.double(PPTreeregOBJ$Tree.result$splitCutoff.node[idj, Rule]),4),
                                right = FALSE,
                                #index = 2L:1L
                                index = 1L:2L)
    ret
  }


  PPtreereg_node <- function(i) {
    if (is.null(PPtreereg_kids(i)))
      return(partykit::partynode(as.integer(i)))
    nd <- partykit::partynode(as.integer(i), split = PPtreereg_split(i),
                              kids = lapply(PPtreereg_kids(i),PPtreereg_node))
    left <- partykit::nodeids(partykit::kids_node(nd)[[1L]], terminal = TRUE)
    right <- partykit::nodeids(partykit::kids_node(nd)[[2L]], terminal = TRUE)
    nd$split$prob <- c(0, 0)
    nl <- sum(fitted[["(fitted)"]] %in% left)
    nr <- sum(fitted[["(fitted)"]] %in% right)
    if(nl > nr) {
      nd$split$prob <- c(1, 0)
    } else {
      nd$split$prob <- c(0, 1)
    }
    nd$split$prob <- as.double(nd$split$prob)
    return(nd)
  }
  node <- PPtreereg_node(1)
  rval <- partykit::party(node = node,
                          data = if (data)
                            mf
                          else mf[0L, ],
                          fitted = fitted,
                          terms = PPTreeregOBJ$terms,
                          info = list(method = "PPTreereg"))
  class(rval) <- c(class(rval),"constparty")
  return(rval)
}
