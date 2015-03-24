# neato

This is a set of functions that I use somewhat regularly. These are currently to do with decision trees and multiple imputation.

My general workflow in R is to make a function after I copy and paste some code more than once. Sometimes I find myself using these functions more than once, across different papers. So I decided to make an R package with a few of these functions, called “neato”. I called it this in the hope that one day someone will find one of these functions and say: “neato!”, because hey, it’s pretty neat!

I hope that the more I write into this package, the better I’ll get at writing R packages and the more I can contribute to the R community. I’d also like to thank people like Hadley Wickham and Hilary Parker for writing materials to inspire me to contribute to R.

Current functions in `neato` are:

- `importance_table`
- `importance_plot`
- `imputation_plot`

**importance_table**

Create an table of the importance values for `rpart` or `gbm.step` class

**importance_plot**

Create a plot of the importance values for `rpart` or `gbm.step` class

**imputation_plot**

Create a ggplot of a given list of variables for an imputed object of class "mids" from the `mice` package.

# How to install

```
devtools::install_github("tierneyn/neato")
```

# Known issues

1. Currently the packages work but might throw errors if certain packages aren't loaded.

2. Does not work for gbm, only gbm.step. Get the source code for gbm.step [here](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2008.01390.x/full)

# Future work

## Decision Trees

I have a great desire to make the decision tree specific functions work for all decision tree packages, as I feel like there are certain things that you want to do when you're looking at decision trees, and that is:

- To see what variables are most important
- To see how variables and their interactions influence prediction

And the plots often provided with the packages give you what you need, but they aren't immediately of publishable quality. And I'd like to change that.

I'm also particularly frustrated by the standard plots used in the `gbm.step` source code written by Elith et al., and I would like to, in the very near future, create some standard ggplots of their existing stuff. So I guess that's at the top of the list for now.

In the future this work will be expanded to include other decision trees, and might even be made into a seperate decision tree tool package. I don't know what I would call it, but probably some sort of tree-related pun, like `secateurs` or something. 
Although that is hard to spell, so maybe something else like `topiary` or `clippers`. I am open to suggestions!
