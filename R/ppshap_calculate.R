#' weight_matrix
#'
#' @param X X
#' @param normalize_W_weights TRUE
#' @export
#'
weight_matrix <- function(X, normalize_W_weights = TRUE) {

  # Fetch weights
  w <- X[["shapley_weight"]]

  if (normalize_W_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  W <- weight_matrix_cpp(
    subsets = X[["features"]],
    m = X[.N][["n_features"]], #3
    n = X[, .N], #8
    w = w
  )

  return(W)
}

#' feature_exact
#'
#' @param m m
#' @param weight_zero_m  weight_zero_m
#'
feature_exact <- function(m, weight_zero_m = 10^6) {
  features <- id_combination <- n_features <- shapley_weight <- N <- NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(id_combination = seq(2^m))
  combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
  dt[, features := unlist(combinations, recursive = FALSE)]
  dt[, n_features := length(features[[1]]), id_combination]
  dt[, N := .N, n_features]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  return(dt)
}

#' shapley_weights
#'
#' @param m m
#' @param N N
#' @param n_components n_components
#' @param weight_zero_m weight_zero_m
#'
shapley_weights <- function(m, N, n_components, weight_zero_m = 10^6) {
  x <- (m - 1) / (N * n_components * (m - n_components))
  x[!is.finite(x)] <- weight_zero_m
  x
}

#' Calculate shap values with simple methods
#'
#' @param PPTreeregOBJ PPTreereg object
#' @param testObs testObs
#' @param final.rule rule to calculate the final node value
#' @export
#'
ppshapr.simple <- function(PPTreeregOBJ, testObs, final.rule){
  origclass <- id_combination <- leafid <- p_hat <- finalLeaf <- NULL # due to NSE notes in R CMD check
  finalRule = as.integer(final.rule)
  leaf_len = length(PPTreeregOBJ$class.num)

  x_train <- data.table::as.data.table(PPTreeregOBJ$Tree.result$origdata)
  x_test <- data.table::as.data.table(testObs)
  feature_names <- names(x_train)
  n_features <- ncol(x_train)

  dt_combinations <- feature_exact(n_features, weight_zero_m= 10^6)
  weighted_mat <- weight_matrix(X = dt_combinations , normalize_W_weights = TRUE)
  feature_matrix <- shapr::feature_matrix_cpp(features =  dt_combinations[["features"]], m = n_features)
  n_combinations <- nrow(feature_matrix)
  n_samples <- 1e3

  x_train$origclass <- data.table::as.data.table(PPTreeregOBJ$Tree.result$origclass)

  index_features <- dt_combinations[, .I]
  S <-feature_matrix[index_features, ]

  dt_l <- list()

  for (l in seq(leaf_len)){
    x_train_ <- as.matrix(x_train[origclass==l,][,origclass:=NULL])
    n_train_ <- nrow(x_train_)
    n_samples_ <- min(n_samples, n_train_)
    index_simple <- seq(1,nrow(S))
    w <- 1 /n_samples_
    x_test <- as.matrix(x_test[1, , drop = FALSE])
    x_bars <- as.matrix( data.table::as.data.table(PPTreeregOBJ$class.origX.mean)[l,])

    dt_p <- observation_impute_cpp_simple(
      xbar = x_bars,
      index_simple = index_simple,
      xtest = x_test,
      S = S
    )

    # Add keys
    dt_l[[l]] <- data.table::as.data.table(dt_p)
    data.table::setnames(dt_l[[l]], colnames(x_train_))
    dt_l[[l]][, id_combination := index_simple]
    dt_l[[l]][, w := w] # IS THIS NECESSARY?
    dt_l[[l]][, leafid := l]
  }

  dt = data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  data.table::setkeyv(dt, c("leafid", "id_combination"))
  max_id_combination <- dt[, max(id_combination)]



  # Predictions
  prediction_zero = predict.PPTreereg(PPTreeregOBJ,PPTreeregOBJ$class.origX.mean,final.rule = finalRule)
  dt[id_combination != 1, p_hat := predict.PPTreereg(PPTreeregOBJ,newdata = .SD,final.rule = finalRule), .SDcols = feature_names]
  #dt[id_combination != 1, ':='(p_hat = predict.PPTreereg(PPTreeregOBJ,newdata = .SD,final.rule = finalRule)), .SDcols = feature_names]
  dt[id_combination == 1, p_hat := prediction_zero]

  p_all <- unique(dt[id_combination == max(id_combination), p_hat])
  names(p_all) <- names(x_test)

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .( leafid, id_combination)]
  data.table::setkeyv(dt_res, c( "leafid", "id_combination"))

  dt_mat <- data.table::dcast(dt_res, id_combination ~ leafid ,value.var = "k")
  dt_mat[, id_combination := NULL] # contribution_mat
  kshap <- t(weighted_mat %*% as.matrix(dt_mat))

  dt_kshap <- data.table::as.data.table(kshap)
  dt_kshap[,finalLeaf := seq(leaf_len)]
  colnames(dt_kshap) <- c("none", feature_names, "finalLeaf")

  r <- list(
    dt = dt_kshap,
    model = PPTreeregOBJ,
    p = p_all,
    final.rule = finalRule,
    x_test = x_test
  )

  return(r)

}


#' Calculate shap values with empirical methods
#'
#' @param PPTreeregOBJ PPTreeregOBJ
#' @param testObs testObs
#' @param final.rule rule to calculate the final node value
#' @export
#'
ppshapr.empirical <- function(PPTreeregOBJ, testObs, final.rule){
  origclass <- id_combination <- leafid <- V1 <- p_hat <- finalLeaf <- keep <- NULL # due to NSE notes in R CMD check

  finalRule = as.integer(final.rule)
  leaf_len = length(PPTreeregOBJ$class.num)

  x_train <- data.table::as.data.table(PPTreeregOBJ$Tree.result$origdata)
  x_test <- data.table::as.data.table(testObs)
  feature_names <- names(x_train)
  n_features <- ncol(x_train)

  dt_combinations <- feature_exact(n_features, weight_zero_m= 10^6)
  weighted_mat <- weight_matrix(X = dt_combinations , normalize_W_weights = TRUE)
  feature_matrix <- shapr::feature_matrix_cpp(features =  dt_combinations[["features"]], m = n_features)
  n_combinations <- nrow(feature_matrix)
  n_samples <- 1e3

  x_train$origclass <- data.table::as.data.table(PPTreeregOBJ$Tree.result$origclass)

  index_features <- dt_combinations[, .I]
  S <-feature_matrix[index_features, ]

  dt_l <- list()

  for (l in seq(leaf_len)){
    x_train_ <- as.matrix(x_train[origclass==l,][,origclass:=NULL])
    n_train_ <- nrow(x_train_)
    n_samples_ <- min(n_samples, n_train_)
    index_s_ <- rep(index_features, each = n_samples_)
    w <- 1 /n_samples_

    index_xtrain <- c(replicate(nrow(S), seq(n_train_)))

    x_test <- as.matrix(x_test[1, , drop = FALSE])
    dt_p <- shapr::observation_impute_cpp(
      index_xtrain = index_xtrain,
      index_s = index_s_,
      xtrain = x_train_,
      xtest = x_test,
      S = S
    )

    # Add keys
    dt_l[[l]] <- data.table::as.data.table(dt_p)
    data.table::setnames(dt_l[[l]], colnames(x_train_))
    dt_l[[l]][, id_combination := index_s_]
    dt_l[[l]][, w := w] # IS THIS NECESSARY?
    dt_l[[l]][, leafid := l]
    #dt_l[[l]][, testid := i]
  }

  dt = data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  data.table::setkeyv(dt, c("leafid", "id_combination"))
  dt[, keep := TRUE]
  max_id_combination <- dt[, max(id_combination)]

  first_element <- dt[, tail(.I, 1), .(leafid, id_combination)][id_combination %in% c(1, max_id_combination), V1]
  dt[id_combination %in% c(1, max_id_combination), keep := FALSE]
  dt[first_element, c("keep", "w") := list(TRUE, 1.0)]
  dt <- dt[keep == TRUE][, keep := NULL]

  # Predictions
  prediction_zero = predict.PPTreereg(PPTreeregOBJ,PPTreeregOBJ$class.origX.mean,final.rule = finalRule)
  dt[id_combination != 1, p_hat := predict.PPTreereg(PPTreeregOBJ,newdata = .SD,final.rule = finalRule), .SDcols = feature_names]
  dt[id_combination == 1, p_hat := prediction_zero]

  p_all <- unique(dt[id_combination == max(id_combination), p_hat])
  names(p_all) <- names(x_test)

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .( leafid, id_combination)]
  data.table::setkeyv(dt_res, c( "leafid", "id_combination"))

  dt_mat <- data.table::dcast(dt_res, id_combination ~ leafid ,value.var = "k")
  dt_mat[, id_combination := NULL] # contribution_mat
  kshap <- t(weighted_mat %*% as.matrix(dt_mat))

  dt_kshap <- data.table::as.data.table(kshap)
  dt_kshap[,finalLeaf := seq(leaf_len)]
  colnames(dt_kshap) <- c("none", feature_names, "finalLeaf")

  r <- list(
    dt = dt_kshap,
    model = PPTreeregOBJ,
    p = p_all,
    final.rule = finalRule,
    x_test = x_test
  )

  return(r)

}
