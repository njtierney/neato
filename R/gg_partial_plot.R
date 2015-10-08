#' gg_partial_plot
#' 
#' 
#' 
#' # this uses the "get_partial_dependence" function to then plot the partial dependence.
#' Would be really great to generalize this to RPART, BRT, and RF models

# note, would be good to set this up as an S3 method...

# what are the variables that we want to plot?

important_vars <- c("LF", "sys", "dias", "crs", "bmi")

df_box <- list("vector", length(important_vars))

for (i in (1:length(important_vars))){
  
  df_box[[i]] <- get_partial_dependence(brt.ghs, 
                                        important_vars[[i]])
  
}

df <- bind_rows(df_box)

# make another 

df.mean <- 
  df %>% 
  group_by(variable) %>%
  summarise(mean = mean(fitted_function))

ggplot(data = df,
       aes(x = value,
           y = fitted_function)) + 
  geom_line() + 
  facet_wrap(~variable,
             ncol = 2,
             scales = "free_x") +
  geom_hline(data = df.mean,
             aes(yintercept = mean),
             colour = "red",
             linetype = "dashed",
             alpha = 0.75) +
  geom_hline(aes(yintercept = 3.729),
             colour = "orange") + 
  geom_hline(aes(yintercept = 12.675),
             colour = "green")