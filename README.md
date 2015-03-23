# neato

This is a set of functions that I use somewhat regularly.

Current functions are:

- importance_table

Create an table of the importance values for `rpart` or `gbm.step` class

- importance_plot

Create a plot of the importance values for `rpart` or `gbm.step` class

- imputation_plot

Create a ggplot plot of a given list of variables for an imputed object of class "mids" from the `mice` package.

# How to install

```
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("tierneyn/neato")
```

# Known issues

1. Currently the packages work but might throw errors if certain packages aren't loaded.

2. Does not work for gbm, only gbm.step

# Future work

## Decision Trees

I have a great desire to make the decision tree specific functions work for all decision tree packages, as I feel like there are certain things that you want to do when you're looking at decision trees, and that is:

- To see what variables are most important
- To see how variables and their interactions influence prediction

And the plots often provided with the packages give you what you need, but they aren't immediately of publishable quality. And I'd like to change that.

I'm also particularly frustrated by the standard plots used in the `gbm.step` source code written by Elith et al., and I would like to, in the very near future, create some standard ggplots of their existing stuff. So I guess that's at the top of the list for now.

In the future this work will be expanded to include other decision trees, and might even be made into a seperate decision tree tool package. I don't know what I would call it, but probably some sort of tree-related pun, like `secateurs` or something. 
Although that is hard to spell, so maybe something else like `topiary` or `clippers`. I am open to suggestions!
