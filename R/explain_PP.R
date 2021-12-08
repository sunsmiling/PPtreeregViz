#' explainer make for PPTreereg
#'
#' @title Make explainer for PPTreeregObj
#' @usage explain_PP(PPTreeregOBJ, data, y, final.rule,...)
#' @param PPTreeregOBJ PPTreereg object
#' @param data data.frame or matrix - data that was used for fitting. If not provided then will be extracted from the model. Data should be passed without target column (this shall be provided as the y argument).
#' @param y numeric vector with outputs / scores. If provided then it shall have the same size as data
#' @param final.rule rule to calculate the final node value
#' @param ... arguments to be passed to methods
#' @examples
#' \dontrun{library(DALEX)
#' library(PPtreeRegViz)
#' data(mtcars)
#' PP_Tree <- PPtreeregViz::PPTreereg(mpg~.,data=mtcars)
#' explainer_PPTreereg <- explain_PP(model = PP_Tree,
#' data = mtcars[, -1], y = mtcars[,1], final.rule = 4)
#' # model performance plot
#' DALEX::model_performance(explainer_PPTreereg) %>% plot()
#' #Break Down Profile
#' DALEX::predict_parts(explainer_PPTreereg, new_observation =  mtcars[1,]) %>% plot()
#' }
#' @export

explain_PP <- function(PPTreeregOBJ, data, y, final.rule,...){
  explainer_PPTreereg <- DALEX::explain(
    model = PPTreeregOBJ,
    data = data,
    y = y,
    predict_function = function(m,x) as.numeric(predict.PPTreereg(m, x, final.rule = final.rule)),
    label = paste("PPTreereg with finalRule: ",final.rule)
  )
  explainer_PPTreereg
}
