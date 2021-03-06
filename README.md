neato
=====

This is a set of functions that I use somewhat regularly. These are currently to do with decision trees and plotting multiple imputation.

My general workflow in R is to make a function after I copy and paste some code more than once. Sometimes I find myself using these functions more than once, across different papers. So I decided to make an R package with a few of these functions, called “neato”. I called it this in the hope that one day someone will find one of these functions and say: “neato!”, because hey, it’s pretty neat!

I hope that the more I write into this package, the better I’ll get at writing R packages and the more I can contribute to the R community. I’d also like to thank people like Hadley Wickham and Hilary Parker for writing materials that inspire me and help me contribute to R.

Examples
========

Decision Tree related functions
-------------------------------

The below functions all work (mostly!) for objects of class `rpart`, `gbm.step`, and `randomForest`, and `train` (from the `caret` package...these are still under development).

### `importance_table`

Create a tidy dataframe of importance values

``` r
library(rpart)

kyphosis_rpart <- rpart(Kyphosis ~ Age + Number + Start, 
                        data = kyphosis)

library(neato)

# before neato

kyphosis_rpart$variable.importance
```

    ##    Start      Age   Number 
    ## 8.198442 3.101801 1.521863

``` r
# after neato

importance_table(kyphosis_rpart)
```

    ## # A tibble: 3 x 2
    ##   variable importance
    ##     <fctr>      <dbl>
    ## 1    Start   8.198442
    ## 2      Age   3.101801
    ## 3   Number   1.521863

``` r
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
set.seed(71)
iris_rf <- randomForest(Sepal.Width ~ ., data=iris)

# before neato

iris_rf$importance
```

    ##              IncNodePurity
    ## Sepal.Length      4.725972
    ## Petal.Length      6.045972
    ## Petal.Width       5.932889
    ## Species           3.798640

``` r
# after neato

importance_table(iris_rf)
```

    ## # A tibble: 4 x 2
    ##       variable IncNodePurity
    ##         <fctr>         <dbl>
    ## 1 Sepal.Length      4.725972
    ## 2 Petal.Length      6.045972
    ## 3  Petal.Width      5.932889
    ## 4      Species      3.798640

### `importance_plot`

Plot importance values

``` r
importance_plot(kyphosis_rpart)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# importance_plot(iris_rf) # currently broken...
```

### rss

Obtain the residual sums of squares

### get\_partial\_dependence

**only for gbm.step**

### gg\_partial\_plot

### grid\_partial\_plot

### imputation\_plot

Create a ggplot of a given list of variables for an imputed object of class "mids" from the `mice` package.

Known issues
============

1.  Does not work for gbm, only gbm.step. Get the source code for gbm.step [here](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2008.01390.x/full)

2.  Partial dependence only works for `gbm.step`. Would like to generalize to `rpart`, `randomForest`, and `train`.

3.  recently `importance_plot` is broked for .randomForest. Looking into fixing this before March

Future work
===========

Decision Trees
--------------

I have a great desire to make the decision tree specific functions work for all decision tree packages, as I feel like there are certain things that you want to do when you're looking at decision trees, and that is:

-   To see what variables are most important
-   To see how variables and their interactions influence prediction

And the plots often provided with the packages give you what you need, but they aren't immediately of publishable quality. And I'd like to change that by making them output ggplot objects.

In the future this work will be expanded to include other decision trees, and might even be made into a seperate decision tree tool package. I don't know what I would call it, but probably some sort of tree-related pun, like `secateurs` or something. Although that is hard to spell, so maybe something else like `topiary` or `clippers`. I am open to suggestions!

Possible suggestions:

-   `topiary`
-   `topiRy`
-   `bonsai`
-   `ikebana`
-   `secatur`
-   `treezy`
