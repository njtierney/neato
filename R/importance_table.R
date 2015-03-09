#' importance_table
#'
#' \code{importance_table} returns a data_frame of variable importance
#'
#' This currently only works for rpart and gbm.step functions. In the
#' future more features will be added so that it works for many
#' decision trees
#'
#' @param x An rpart of gbm.step object to be turned into a tidy dataframe
#'     containing importance values
#'
#' @return A data_frame object made with the intention of turning it into a
#'     text table with `knitr` or `xtable`
#'
#' @examples
#'
#' # turn a fitted object into a data_frame
#'
#'  \code{rpart} object
#' fit.rpart <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#'
#' importance_table(fit.rpart)
#'
#' # you can even use piping
#'
#' fit.rpart %>% importance_table
#'
#' \code{gbm.step} object
#' fit.gbm.step <- gbm.step(data = iris,
#'                          gbm.x = c(1:3),
#'                          gbm.y = 4,
#'                          tree.complexity = 1,
#'                          family = "gaussian",
#'                          learning.rate = 0.01,
#'                          bag.fraction = 0.5)
#'
#' importance_table(fit.gbm.step)
#'
#' # with piping
#' fit.gbm.step %>% importance_table
#'
#' Unfortunately it cannot yet run a \code{gbm} object:
#' \dontrun{
#'
#' gbm.fit <- gbm(Sepal.Width ~ .,
#'                distribution = "gaussian",
#'                data = iris)
#'
#' importance_table(gbm.fit)
#' }
#'
#' @seealso \url{https://github.com/dgrtwo/broom}
#'
#' @export
importance_table <- function(x){

#========
# rpart
#========
  if (class(x) == "rpart") {
    # make a kable plot for the variable importance from the CART model

    x <-
      x$variable.importance %>%
      data.frame(variable = names(x$variable.importance),
                 importance = as.vector(x$variable.importance),
                 row.names = NULL) %>%
      select(variable,
             importance)

    res <- x
    class(res) <- c("imp_tbl", class(res))
    return(res)

#=====
# gbm
#=====

  } else if (class(x) == "gbm") {

    x <-
      x$contributions %>%
        # make it a dataframe
        as_data_frame %>%
        # rename the variables
        rename(variable = var,
               importance = rel.inf)

    res <- x
    class(res) <- c("imp_tbl", class(res))
    return(res)

#=============
# error catch
#=============

    } else stop("x is not an rpart or gbm.step type object")

} # end function
