
# References based on which I put that together:
# https://stats.stackexchange.com/questions/50560/how-to-calculate-partial-dependence-when-i-have-4-predictors
# https://www.alexpghayes.com/blog/understanding-multinomial-regression-with-partial-dependence-plots/
# https://bgreenwell.github.io/pdp/articles/pdp.html

# This repo is purely for my own reference in case I want to come back to this 
# at some point in the future

library(tidyverse)
library(ranger)
library(broom)

mtcars %<>%
  as_tibble() %>%
  select(hp, mpg, disp, wt, qsec)

# Let's fit the model that we will later approximate with PDP
model <- ranger(hp ~ ., mtcars)

# Let's try to calculate PDP for the disp variable
pdp_y <- select(mtcars, disp)
pdp_x <- select(mtcars, -disp)

print("Number of unique levels in the disp variable")
length(unique(pdp_y$disp))

print("Number of unique rows in the dataframe used for explanations")
nrow(distinct(pdp_x))

print("The total number of rows for calculating the PDP plot is:")
length(unique(pdp_y$disp)) * nrow(distinct(pdp_x))

# We should get the same number of rows from the crossing function
(grid <- crossing(pdp_y, pdp_x))

# So over here we are multiplying every single unique value of the variable
# that we want to calculate the PDP for, times the entire, distinct space
# of other variable values that were used in the model. In this way, we can
# get a truly 'average' prediction for every unique observation of the PDP variable
# we will explain based on the entire training set. That's why when we combine
# these average predictions together for every distinct value of the PDP variable 
# "marginalize" we calculate the marginal effects

# Now we need to get predictions for every point in our grid
augment_ranger <- function(x, newdata) {
  newdata <- as_tibble(newdata)
  mutate(newdata, pred = predict(x, newdata)$predictions)
}

(grid_pred <- augment_ranger(model, grid))

# Now we have the predictions and we 'marginalize' them by taking the average
# prediction for each point
grid_pred_summary <- grid_pred %>%
  group_by(disp) %>%
  summarize(pdp_pred = mean(pred))

# Let's visualize the results
grid_pred_summary %>%
  ggplot(aes(disp, pdp_pred)) +
  geom_line(size = 1) +
  labs(title = "Partial dependence plot for displacement",
       y = "Average prediction across all other predictors",
       x = "Engine displacement") +
  theme_bw()

# Let's check this implementation against the pdp package
library(pdp)

pdp_pck <- partial(
  model,
  "disp",
  distinct(mtcars, disp),
  train = mtcars
)

model %>%
  partial("disp", train = mtcars) %>%
  plotPartial(rug = TRUE, train = as.data.frame(mtcars))

# I came across some other blog acorss a simplified approach: instead of
# averaging the prediction over the entire space of training data, simple
# medians/ averages/ modes are calculated for each column and predictions
# for each unique value of the target PDP variable are calculated. Hence, 
# no need for averaging which makes the procedure much faster. But how does
# this method compare with regards to accuracy?

# How would the PDP look like if I used the median values for all other features?
pdp_x_med <- pdp_x %>% map_dfc(median)

(grid_med <- crossing(pdp_y, pdp_x_med))

grid_pred_med <- augment_ranger(model, grid_med)

grid_pred_summary_med <- grid_pred_med %>%
  group_by(disp) %>%
  summarize(pdp_pred = mean(pred))

grid_pred_summary_med %>%
  ggplot(aes(disp, pdp_pred)) +
  geom_line(size = 1) +
  labs(title = "Partial dependence plot for displacement",
       y = "Average prediction across all other predictors",
       x = "Engine displacement") +
  theme_bw()

# Let's compare both methods using plots
grid_pred_summary %>% 
  mutate(method = "full") %>% 
  bind_rows(
    grid_pred_summary_med %>% 
      mutate(method = "mini")
  ) %>%
  ggplot(aes(disp, pdp_pred, color = method)) +
  geom_line(size = 1) +
  labs(title = "Difference in PDP for displacement between both methods",
       y = "Average prediction across all other predictors",
       x = "Engine displacement") +
  theme_bw()

