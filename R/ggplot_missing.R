#' ggplot_missing
#'
#' @description A function that plots missing data in ggplot. For a more updated version, see `vis_miss` from visdat - github.com/njtierney/visdat
#'
#' @param x a data frame
#'
#' @return A ggplot of the missing data.
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
                    labels = c("Present","Missing")) +
    theme_minimal() +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
