#' ggplot_missing
#'
#' @description A function that plots missing data in ggplot
#'
#' @param x a data frame
#'
#' @return A ggplot of the missing data
#'
#' @import ggplot2
#' @import reshape2
#' @import dplyr
#'
#'@export

ggplot_missing <- function(x){

  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","NA")) +
    theme_minimal() +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
