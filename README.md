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

In the future this work will be expanded to include other decision trees, and might even be made into a seperate decision tree tool package.
I don't know what I would call it, but probably some sort of tree-related pun, like `secateurs` or something. 
Although that is hard to spell, so maybe something else like `topiary`



