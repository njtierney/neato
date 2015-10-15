#' rss
#'
#' \description A function that gives returns the RSS of a decision tree model.
#'
#' @param x A fitted decision tree model - either `rpart`, `gbm` or `randomForest`.
#'
#' @return The Residuals Sums of Squares (RSS) $\sum_i^n(Y_i - \hat(Y))^2$ for the models `rpart`, `gbm.step`, and `randomForest`.
#'
#' @note when using the `caret` package, be sure to select `model$finalModel` when entering it into the `rss` function. Also note that the RSS only works for continuous variables.
#'
#' @examples
#'
#' library(rpart)
#'
#' fit.rpart <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#'
#' rss(fit.rpart)
#'
#' @export

#=======================#
# Constructor functions #
#=======================#

rss <- function(x){

  UseMethod("rss", x)

}

#=====================================#
#' Classification and Regression Tree #
#=====================================#

rss.rpart <- function(x){

  sum((residuals(x)^2))

}

#==========================#
#' Boosted Regression Tree #
#==========================#
rss.gbm <- function(x){

  sum(x$residuals^2)

}
#================#
#' Random Forest #
#================#
rss.randomForest <- function(x){

  res <- x$y - x$predicted

  sum(res^2)

} # end function
