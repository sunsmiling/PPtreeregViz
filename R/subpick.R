#' subpick
#'
#' @param data_long
#' @param final.leaf
#' @param obsnum
#'
#' @return
#' @export
#'
#' @examples
#' data("dataXY")
#' Model <- PPTreereg(Y~., data = dataXY, DEPTH = 2)
#' shap_long=ppshapr_prep(Model,final.rule =3,method="simple")
#' subpick(shap_long,final.leaf = 1, obsnum = 5)
#'
#'

subpick <- function(data_long,final.leaf, obsnum = 5){

  feature_dict = levels(data_long[,variable])
  d_prime = length(feature_dict)

  data.table::setkey(data_long, id)
  data_long_f = data_long[finalLeaf==final.leaf]

  W_ = data.table::dcast(data_long_f, finalLeaf+id ~variable, value.var = "value")
  sample_size = length(unique(W_[,id]))

  W <- W_[,..feature_dict]

  # Create the global importance vector, I_j described in the paper
  importance = sqrt(colSums(abs(W)))

  # NoW1 run the SP-LIME greedy algorithm
  remaining_indices = 1:sample_size
  remaining_indices = remaining_indices[!duplicated(remaining_indices)]

  V <- c()
  V_times <- c()

  for( k in c(1:obsnum)){
    best = 0
    best_ind = NA
    current = 0

    for (j in remaining_indices){
      current = sum(abs(W)[c(V,j),]*importance)
      #current = sum(abs(W)[c(V,j),])
      if(current >= best){
        best = current
        best_ind = j}
    }
    V <- c(V, best_ind)
    remaining_indices <-  setdiff(remaining_indices, best_ind)
  }
  V_ <- sort(V)

  origdata_ = data.table::dcast(data_long_f[id %in% V_], finalLeaf+id ~variable,value.var = "rfvalue")

  result <- list(df = origdata_[,..feature_dict], obs= V_)

  return(result)
}
