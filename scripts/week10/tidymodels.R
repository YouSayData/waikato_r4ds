library(tidymodels)
library(lime)

iris %>% as_tibble

iris %>% ggplot() +
  geom_point(aes(Sepal.Length, Petal.Length, col = Species))


# Preprocess --------------------------------------------------------------

# Sampling ----------------------------------------------------------------

iris_split <- initial_split(iris, prop = .6)
iris_split

iris_split %>%
  training %>%
  glimpse

iris_split %>%
  testing %>%
  glimpse

iris_split %>%
  testing %>%
  class


# Pre-Process interface ---------------------------------------------------

# recipe() takes the models formula and starts a set of transformations
# transformations in tidymodels start with step_ . There quite a few.
# prep() executes them

iris_recipe <- iris_split %>%
  training %>%
  # start recipe for model
  recipe(Species ~.) %>%
  # removes variables that have large correlations with other variables
  step_corr(all_predictors()) %>% 
  # numeric data is centered to a mean of 0
  step_center(all_predictors(), -all_outcomes()) %>%
  # numeric data is scaled to a sd of 1
  step_scale(all_predictors(), -all_outcomes()) %>%
  # execute the transformations
  prep
  

# transform the test data -------------------------------------------------

# we bake the testing data with the recipe derived from the training data
iris_testing <- iris_recipe %>%
  bake(testing(iris_split)) 

iris_testing

# for the training data we don't need an extra preprocessing step,
# because it has already been done during the recipe creation
# we only need to extract aka juice it

iris_training <- juice(iris_recipe)
iris_training


# Model training ----------------------------------------------------------

# before tidymodels one needed to know all kind of parameters for all kind of libraries
# e.g. ranger and randomforest do similar things, but have differently named parameters
# tidymodels give easy access to that

iris_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Species ~ ., data = iris_training)

iris_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(Species ~ ., data = iris_training)


# Predictions -------------------------------------------------------------

predict(iris_ranger, iris_testing)

iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing)


# Validations -------------------------------------------------------------

iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  metrics(truth = Species, estimate = .pred_class)

iris_rf %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  metrics(truth = Species, estimate = .pred_class)


# Probabilities -----------------------------------------------------------

iris_ranger %>%
  predict(iris_testing, type = "prob")

iris_probs <- iris_ranger %>%
  predict(iris_testing, type = "prob") %>%
  bind_cols(iris_testing)

iris_probs%>%
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()

# explain models with lime ------------------------------------------------

# Create an explainer object
explainer <- lime(iris_training, iris_rf)

# create an explanation
explanation <- explain(iris_testing, explainer, n_labels = 1, n_features = 3)

# The output is provided in a consistent tabular format and includes the
# output from the model.
explanation

plot_explanations(explanation)


