#' imputation_plot
#'
#' \code{imputation_plot} graphically compare values across imputations
#'
#' This code takes an object fitted with the `mice` package and plots a density
#' of each of the specified variables. Currently the variables have to be
#' specified but in the future they might not be
#'
#' @param mice.object is an object of class `mids`
#'
#' @param vars a vector of variables as character
#'
#' @param include.data logical: Do you want to include the original data?
#'
#' @return a density plot from the ggplot2 package
#'
#' @examples
#'
#'    imp <- mice(nhanes)
#'    plot1 <- imputation_plot(mice.object = imp,
#'                             vars = c("age", "bmi"),
#'                             include.data = F)
#'
#'    # show the density
#'    plot1$density
#'
#' Required packages
#'
#' dplyr
#' reshape2
#' mice
#' ggplot2
#'
#' @export
imputation_plot <- function(mice.object,
                            vars,
                            include.data){

  # this is an error catcher, it checks if `mice.object` is of class `mids`
  # and if it is not, it stops the function and displays the message
  if (class(mice.object) != "mids") stop("mice.object must be of class `mids`")

  # get the number of imputations used and store
  m <- mice.object$m

  # make a list to contain all of the imputed dataframes (m times)
  dat.mi.list <- list("vector", m)

  # now, go through 1...m times and do the following
  for (i in (1:m)){

    # set the data to be
    dat.mi.list[[i]] <-
      # the i-th completed dataset from multiple imputation
      complete(mice.object, i) %>%
      # then subset the data based upon the variables specified
      select(one_of(vars)) %>%
      # then make a column called `m`, and make this a factor
      mutate(m = as.factor(i)) %>%
      # then reshape the data according to the ID var being `m`
      melt(id.vars = "m")

  }

  # are we including the original data into the plot?
  if (include.data == F){

  #combine data
  dat.combine <-
    do.call("rbind", # do this
            dat.mi.list) # to this list

  #===========
  # Plotting
  #===========

  # do a density plot
  ggplot(data = dat.combine,
         aes(x = value,
             fill = m)) +
    geom_density(alpha = (1/m)) +  # set alpha to be a fraction of m
    facet_wrap(~variable,         # facet by variable
               scales = "free") +
    theme(legend.position="none")

  # want to include the original data in the plot?
  } else if (include.data ==T){

    #combine data
    data.imputed.melt <-
      do.call("rbind", # do this
              dat.mi.list) # to this list

    #grab the data used by imputation
    data.melt <-
      mice.object$data %>%
      select(one_of(vars)) %>%
      mutate(m = as.factor(0)) %>%
      melt(id.vars = "m")

    ## plot the data
    ggplot(data = data.imputed.melt,
           aes(x = value,
               fill = m)) +
      geom_density(data = data.melt,
                   aes(x = value),
                   fill = "black") +
      geom_density(alpha = (1/6)) +
      facet_wrap(~variable,
                 scales = "free") +
      theme(legend.position="none")

  } # end if-else sattement

} # end function

## future work, plot histograms and other plots

#     ############
#     # Histogram
#     ############
#       # as a histogram with counts
#       histogram <- ggplot(data = dat.combine,
#                           aes(x = value,
#                               fill = m)) +
#         # plot a histogram with binwidth 1, a transparency of set to the number
#         # of imputations
#         # position is set to identity so that it doesn't stack the plots
#         geom_histogram(binwidth = 1,
#                        alpha = (1/m),
#                        position = "identity") +
#         # facet by variable, set the scales to be free
#         facet_wrap(~variable,
#                    scales = "free"
#   # this function will return these values, which you can call - see example
#   return(list(histogram,
#               density))
