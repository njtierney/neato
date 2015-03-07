# random functions

# make a function to show the first four variables in a kable
#
# brt.kable <- function(brt.object){
#
#   brt.object <- brt.object
#
#   brt.object$contributions %>%
#     # make it a dataframe
#     as_data_frame %>%
#     # rename the variables
#     rename(variable = var,
#            relative.imp = rel.inf) %>%
#     # take the first four
#     .[1:4, ] %>%
#     # show it as a table
#     kable
#
# }



# rpart.vimp.table <- function(rpart.object){
#
#   # make a kable plot for the variable importance from the CART model
#
#   rpart.object$variable.importance %>%
#     data.frame(variable = names(rpart.object$variable.importance),
#                importance = as.vector(rpart.object$variable.importance),
#                row.names = NULL) %>%
#     select(variable,
#            importance) %>%
#     kable
#
# }



gbm.plot2 <- function(brt.object){
  # this is a function to change the default drawing of gbm plots
  # so that I have more control over plots...and don't copypasta

  # in this function we grab a brt object
  # and plot it
  gbm.plot(brt.object,
           # setting the plot layout to 2 by 2
           plot.layout = c(2,2),
           # and only plotting 4 plots
           n.plots = 4,
           # and not writing a title.
           write.title = F)


  # Writing a function saved me time, as it means that I can control the output of
  # all of the plots below with ONE piece of code, rather than copying and pasting
  # effectively the same code 8 times.


}


brt.interaction.plot <- function(brt.object){ # an object of gbm class

  # me attempting to make a function that takes a gbm object and turn it into a
  # plot.

  # find the interactions in the data
  brt.int <- gbm.interactions(brt.object)

  # then select the top interaction pair.
  # these are the interaction pair below.

  brt.int.1 <- brt.int$rank.list$var1.index[1]

  brt.int.2 <- brt.int$rank.list$var2.index[1]

  # then use the gbm perspective plot, which is just an image plot.
  # rather than the 3d plot.
  # the reason being...I hate 3d plots!
  gbm.perspec(brt.object,
              brt.int.1,
              brt.int.2,
              perspective = F)

} # end function

brt.vimp.plot <- function(brt.object){

  # this is a plot to make it easier to get the interactions out into a table

  brt.object <- brt.object

  brt.object$contributions %>%
    # make it a dataframe
    as_data_frame %>%
    # rename the variables
    rename(variable = var,
           relative.imp = rel.inf) %>%
    # order by importance
    arrange(-relative.imp) %>%
    # take the first 10
    .[1:10, ] %>%
    # make sure the plot is ordered by most important
    transform(.,
              variable=reorder(variable,
                               relative.imp)) %>%
    # plot them!
    ggplot(data = .,
           aes(x = relative.imp,
               y = variable)) +
    geom_point()

} # end function


# a function to compare the values across multiple imputations.

ggplot.mi.compare <- function(mi.data, # mice object type data
                              vars){ # vector of variables as character

  ## e.g. vars = c("var1", "var2", "var3") etc.
  # this function requires the following packages:
  ## dplyr
  ## reshape2
  ## mice


  # this is an error catcher, it checks if `mi.data` is of class `mids`
  # and if it is not, it stops the function and displays the message
  if(class(mi.data) != "mids") stop("mi.data must be of class `mids`")

  # get the number of imputations used and store
  m <- mi.data$m

  # make a list to contain all of the imputed dataframes (m times)
  dat.mi.list <- list("vector",
                      m)

  # now, go through 1...m times and do the following
  for(i in (1:m)){

    # set the data to be
    dat.mi.list[[i]] <-
      # the i-th completed dataset from multiple imputation
      complete(mi.data, i) %>%
      # then subset the data based upon the variables specified
      select(one_of(vars)) %>%
      # then make a column called `m`, and make this a factor
      mutate(m = as.factor(i)) %>%
      # then reshape the data according to the ID var being `m`
      melt(id.vars = "m")

  }

  #combine data
  dat.combine <-
    do.call("rbind", # do this
            dat.mi.list) # to this list

  #===========
  # Plotting
  #===========

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
  #                    scales = "free")

  ############
  # Density
  ############
  # plot a density
  ggplot(data = dat.combine,
         aes(x = value,
             fill = m)) +
    geom_density(alpha = (1/m)) +  # set alpha to be a fraction of m
    facet_wrap(~variable,         # facet by variable
               scales = "free") +
    remove_legends

  #
  #
  #   # this function will return these values, which you can call - see example
  #   return(list(histogram,
  #               density))

  # ==========
  # examples
  # ==========
  #
  #       imp <- mice(nhanes)
  #
  #       plot1 <- ggplot.mi.compare(mi.data = imp,
  #                                  vars = c("age", "bmi"))
  #
  #       #show the histogram
  #       plot1$histogram
  #
  #       # show the density
  #       plot1$density

} # end function

ggplot.compare.all <- function(mice.object, # mice object
                               vars){ # vector of variables as character

  # this function compares all imputed values against the whole dataset

  if(class(mice.object) != "mids") stop("mi.data must be of class `mids`")

  mice.object <- mi.rpart.data

  # get the number of imputations used and store
  m <- mice.object$m

  # make a list to contain all of the imputed dataframes (m times)
  dat.mi.list <- list("vector",
                      m)

  # now, go through 1...m times and do the following
  for(i in (1:m)){

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

  ##
  ggplot(data = data.imputed.melt,
         aes(x = value,
             fill = m)) +
    geom_density(data = data.melt,
                 aes(x = value),
                 fill = "black") +
    geom_density(alpha = (1/6)) +
    facet_wrap(~variable,
               scales = "free") +
    remove_legends


  ##



} # end function

ggplot.compare <- function(data1, # first dataset
                           data2, # second dataset
                           vars, # vector of variables as character
                           lab1, # first label for data1
                           lab2, # second label for data2
                           density) { #TRUE or FALSE to plot a density


  # let's understand and explore the imputed data
  # let's plot the data without imputations, and the data with imputed values


  data1 <-
    data1 %>%
    select(one_of(vars)) %>%
    mutate(type = lab1) %>%
    melt(id.vars = "type")

  data2 <-
    data2 %>%
    select(one_of(vars)) %>%
    mutate(type = lab2) %>%
    melt(id.vars = "type")

  #combine data
  dat.combine <-
    rbind_list(data1,
               data2)

  if (density == F){ # do you want to plot a density? if not, histogram
    ggplot(data = dat.combine,
           aes(x = value,
               fill = type)) +
      geom_histogram(binwidth = 1,
                     alpha = 0.5,
                     position = "identity") +
      facet_wrap(~variable,
                 scales = "free") +
      remove_legends

  } else { # else plot a density
    ggplot(data = dat.combine,
           aes(x = value,
               fill = type)) +
      geom_density(alpha = 0.5,
                   colour = NA) +
      facet_wrap(~variable,
                 scales = "free") +
      remove_legends

  } # end else statement

} # end function

