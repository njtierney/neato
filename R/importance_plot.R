#' importance_plot
#'
#' \code{importance_plot} make a graph of variable importance
#'
#' takes an `rpart` or `gbm.step` fitted object and makes a plot of variable
#' importance
#'
#' @param x is an rpart or gbm.step object
#'
#' @return a ggplot plot of the variable importance
#'
#' @examples
#' turn a fitted object into a data_frame
#'
#'  \code{rpart} object
#' fit.rpart <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#'
#' importance_plot(fit.rpart)
#'
#' # you can even use piping
#'
#' fit.rpart %>% importance_plot
#'
#' @import dplyr
#'
#' @export
importance_plot <- function(x){

  # this is a plot to make it easier to get the interactions out into a table

  # this function takes an importance_table object and then

  if (class(x) == "imp_tbl"){

    x %>%
      # make sure the plot is ordered by most important
      transform(.,
                variable=reorder(variable,
                                 importance)) %>%
      # plot them!
      ggplot2::ggplot(data = .,
             ggplot2::aes(x = importance,
                          y = variable)) +
      ggplot2::geom_point()

  } else if (class(x) != "imp_tbl"){

  x %>%
  importance_table %>%
    # make sure the plot is ordered by most important
    transform(.,
              variable=reorder(variable,
                               importance)) %>%
    # plot them!
    ggplot2::ggplot(data = .,
           ggplot2::aes(x = importance,
                        y = variable)) +
    ggplot2::geom_point()

  } # end ifelse

} # end function
