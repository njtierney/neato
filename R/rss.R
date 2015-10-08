#' RSS
#' \description A function that gives returns the RSS of a decision tree model.
#'
#'
#' Constructor functions

rss <- function(x){
  
  UseMethod(rss)
  
}

#' 
#' 
#' 
#' 
#' CART

rss.rpart <- function(x){
  
  sum((residuals(x)^2))
  # cart_ghs_rss <- sum((residuals(rpart.ghs)^2))  
}


#' BRT

rss.gbm <- function(x){
  
  sum(x$residuals^2)
  # brt_ghs_rss <- sum(brt.ghs$residuals^2)  
}


#' Random Forest
#' 

rss.randomForest <- function(x){

  res <- x$finalModel$y - x$finalModel$predicted  
  
  sum(res^2)  
    
#   residual_rf <- rf_ghs_caret$finalModel$y - rf_ghs_caret$finalModel$predicted  
#   rf_ghs_rss <- sum(residual_rf^2)  
}





