library(tidyverse)
library(tidymodels)
tidymodels_prefer()


college_train <- read_csv(file = "1_Data/college_train.csv")

# Convert character to factor
college_train <- college_train %>%
  mutate(across(where(is.character), factor))

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

### or as a workflow
# create workflow
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_formula(Grad.Rate ~ PhD)

# fit model
Grad.Rate_glm <- fit(lm_wflow, college_train)

tidy(Grad.Rate_glm)

pred <- predict(Grad.Rate_glm, college_train) %>% 
  bind_cols(college_train %>% select(Grad.Rate))

ggplot(pred, aes(x = Grad.Rate, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Graduation Rate", x = "Graduation Rate") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()


reg_metrics <- metric_set(rmse, rsq, mae)

reg_metrics(pred, truth = Grad.Rate, estimate = .pred)

### E 
lm_wflow <- lm_wflow %>% 
  update_formula(Grad.Rate ~ PhD + Room.Board + Terminal + S.F.Ratio)

grad_glm <- fit(lm_wflow, college_train)

tidy(grad_glm)

pred <- predict(grad_glm, college_train) %>% 
  bind_cols(college_train %>% select(Grad.Rate))

reg_metrics(pred, truth = Grad.Rate, estimate = .pred)

ggplot(pred, aes(x = Grad.Rate, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Graduation Rate", x = "Graduation Rate") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

### F 
lm_wflow <- lm_wflow %>% 
  update_formula(Grad.Rate ~ .)

grad_glm <- fit(lm_wflow, college_train)

tidy(grad_glm)

pred <- predict(grad_glm, college_train) %>% 
  bind_cols(college_train %>% select(Grad.Rate))

reg_metrics(pred, truth = Grad.Rate, estimate = .pred)

ggplot(pred, aes(x = Grad.Rate, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Graduation Rate", x = "Graduation Rate") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

### classification

# Look at the class of the variable Private, should be a factor!
class(college_train$Private)
levels(college_train$Private)
# relevel factors such that first level is positive (tidymodels treats first level as positive)
college_train <- college_train %>% 
  mutate(Private = fct_relevel(Private, c("Yes", "No")))


logistic_model <- 
  logistic_reg() %>% 
  set_engine("glm")

logistic_wflow <- 
  workflow() %>% 
  add_model(logistic_model) %>% 
  add_formula(Private ~ .)

private_glm <- fit(logistic_wflow, college_train)
tidy(private_glm)

pred <- predict(private_glm, college_train, type = "prob") %>% 
  bind_cols(predict(private_glm, college_train)) %>% 
  bind_cols(college_train %>% select(Private))
  

class_metrics <- metric_set(accuracy, bal_accuracy, f_meas, kap)

conf_mat(pred, truth = Private, estimate = .pred_class)
class_metrics(pred, truth = Private, estimate = .pred_class)

# auc
roc_auc(pred, Private, .pred_Yes)

# plot roc curve
roc_curve(pred, Private, .pred_Yes) %>% 
  autoplot()
