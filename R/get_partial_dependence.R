#' partial_dependence
#' 
#' 
#' 
#' 
#' Description: A method that returns the partial dependence values for a given set of variables for a gbm.step model. Would be really great in the future to make this a  



get_partial_dependence <- function(x, var){
  
  i <- which(x$var.names == var)
  
  response_matrix <- plot.gbm(x,
                              i.var = i,
                              n.trees = x$n.trees,
                              return.grid = TRUE)
  
  # Mean Centre the data...
  #     df_box[[i]] <- data.frame(value = response_matrix[ ,1],
  #                               fitted_function = response_matrix[ ,2] -
  #                                 mean(response_matrix[ ,2])) %>%
  #       mutate(variable = x$var.names[i])
  
  # not mean centered
  
  df <- data.frame(value = response_matrix[ , 1],
                   fitted_function = response_matrix[ , 2]) %>%
    mutate(variable = x$var.names[i])
  
  return(df)
}
