#' PPTreereg Models in train
#'
#' train in caret package
#' @title model method in caret
#' @export
#'
PPTreereg.M1 <- list(label = "Projection Pursuit Regression Tree",
                  library = "PPtreeregViz",
                  type = "Regression",
                  parameters = data.frame(parameter = c('DEPTH', 'PPmethod'),
                                          class = c("numeric","character"),
                                          label = c("Depth","LPAorPDA")),
                  grid = function(x, y, len = NULL, search = "grid")
                    data.frame(DEPTH = 2, PPmethod = "LDA"),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    out <- PPtreeregViz::PPTreereg(.outcome~., data=dat, DEPTH=param$DEPTH, PPmethod = param$PPmethod)
                    out
                  },

                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata, final.rule=4)
                  },
                  prob = NULL,
                  predictors = NULL,
                  tags = c("Projection Pursuit", "Regression Trees"))
