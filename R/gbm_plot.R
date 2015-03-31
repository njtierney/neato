#' gbm_plot
#'
#'
#' \code{plot_gbm} plots the fitted values against the real values
#'
#' This is a development function that aims to improve the default plots used
#' in gbm.step by elith et al. Their plots are good, but I want them to be
#' better. In the future, this might even be implemented in ggvis.
#' NB:
#' This currently only works for gbm.step functions. In the
#' future more features will be added so that it works for many
#' decision trees.
#'
#'
#' @param gbm.step.oject A gbm.step object to be plotted
#'
#' @return A ggplot2 plot
#'
#' @examples
#'
#' \code{gbm.step} object
#' fit.gbm.step <- gbm.step(data = iris,
#'                          gbm.x = c(2:5),
#'                          gbm.y = 1,
#'                          tree.complexity = 2,
#'                          family = "gaussian",
#'                          learning.rate = 0.01,
#'                          bag.fraction = 0.5)
#'
#' plot_gbm(fit.gbm.step)
#'
#' # with piping
#' fit.gbm.step %>% plot_gbm.step
#'
#' Unfortunately it cannot yet run a \code{gbm} object:
#' \dontrun{
#'
#' gbm.fit <- gbm(Sepal.Width ~ .,
#'                distribution = "gaussian",
#'                data = iris)
#'
#' plot_gbm(gbm.fit)
#' }
#'
#' @import dplyr
#'
#' @export
gbm_plot <- function(x){

  # make a list to hold the data
  df_box <- list("vector", length(gbm.step.object$var.names))

  # make a loop to go through all the data and get the predictions
  for (i in 1:length(gbm.step.object$var.names)){

    # Get the

    response_matrix <- plot.gbm(gbm.step.object,
                                i.var = i,
                                n.trees = 500,
                                return.grid = TRUE)

    df_box[[i]] <- data.frame(value = response_matrix[ ,1],
                              fitted_function = response_matrix[ ,2] -
                                mean(response_matrix[ ,2])) %>%
      mutate(variable = gbm.step.object$var.names[i])

  }

  df <- bind_rows(df_box)

  ## end of loop

  gbm_pred_plot <-
    ggplot2::ggplot(data = df,
           ggplot2::aes(x = value,
               y = fitted_function)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~variable,
               scales = "free_x") +
    ggplot2::geom_hline(yintercept = 0,
               colour = "red")

  return(gbm_pred_plot)

}


## Areas of improvement
#'
#' Being able to set the `n.trees` parameter to the "best" trees available
#'
#' get the ggplot to plot the categorical data properly
#'
#' Add info about the variables into the axis (variable importance %)
#'
#' Add info about varaible names (e.g., categorial data isn't just 1, 2, 3, but
#' "setosa, ..., ...")
#'
#' Allow users to determine how many plots are used
#'
