#' Load \code{PPTreereg} Models to train
#'
#' Load \code{PPTreereg} models to make explanation with caret package.
#' @title \code{PPTreereg} Models with \code{final.rule=3}
#' @export
#' @import caret
#' @examples
#' \donttest{
#' data(dataXY)
#' PP_model <- caret::train(Y ~., data = dataXY, method = PPTreereg.M3, DEPTH=2, PPmethod="LDA")
#'}
PPTreereg.M3 <- list(label = "Projection Pursuit Regression Tree",
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
                       predict(modelFit, newdata, final.rule=3)
                     },
                     prob = NULL,
                     predictors = NULL,
                     tags = c("Projection Pursuit", "Regression Trees"))

#' Load \code{PPTreereg} Models to train
#'
#' Load \code{PPTreereg} models to make explanation with caret package.
#' @title \code{PPTreereg} Models with \code{final.rule=4}
#' @import caret
#' @export
#' @examples
#' \donttest{
#' data(dataXY)
#' PP_model <- caret::train(Y ~., data = dataXY, method = PPTreereg.M4, DEPTH=2, PPmethod="LDA")
#' }
#'
PPTreereg.M4 <- list(label = "Projection Pursuit Regression Tree",
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

#' Load \code{PPTreereg} Models to train
#'
#' Load \code{PPTreereg} models to make explanation with caret package.
#' @title \code{PPTreereg} Models with \code{final.rule=5}
#' @import caret
#' @export
#' @examples
#' \donttest{
#' data(dataXY)
#' PP_model <- caret::train(Y ~., data = dataXY, method = PPTreereg.M5, DEPTH=2, PPmethod="LDA")
#' }
PPTreereg.M5 <- list(label = "Projection Pursuit Regression Tree",
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
                       predict(modelFit, newdata, final.rule=5)
                     },
                     prob = NULL,
                     predictors = NULL,
                     tags = c("Projection Pursuit", "Regression Trees"))
