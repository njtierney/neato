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
#'  \code{randomForest} object
#'
#'  set.seed(131)
#'   ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
#'                            importance=TRUE, na.action=na.omit)
#'   print(ozone.rf)
#'   ## Show "importance" of variables: higher value mean more important:
#'   importance(ozone.rf)
#'
#'   ## use importance_table
#'
#'   importance_table(ozone.rf)
#'
#'   # now plot it
#'
#'   importance_plot(ozone.rf)
#'
#' @import dplyr
#'
#' @export
importance_plot <- function(x){

  # this is a plot to make it easier to get the interactions out into a table

  # this function takes an importance_table object and then

  if ("imp_tbl" %in% class(x)){

    x %>%
      # make sure the plot is ordered by most important
      transform(.,
                variable = reorder(variable,
                                   importance)) %>%
      # plot them!
      ggplot2::ggplot(data = .,
             ggplot2::aes(x = importance,
                          y = variable)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Variables",
                    y = "Importance Score")

  } else if ("randomForest" %in% class(x)) {

    importance_table(x) %>%
      tidyr::gather(variable,
             value) %>%
      transform(.,
                variable = reorder(variable,
                                   value)) %>%
      ggplot2::ggplot(data = .,
                      ggplot2::aes(x = value,
                                   y = variable)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ variable.1,
                          scales = "free") +
      ggplot2::theme(axis.text.x = element_text(angle = 45))

  } else if (!("imp_tbl" %in% class(x))) {

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
