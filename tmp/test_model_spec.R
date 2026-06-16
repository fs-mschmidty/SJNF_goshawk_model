library(tidyverse)
library(tidymodels)
library(tidysdm)
library(targets)

covs <- tar_read(all_covs)
covs |>
  count(ForTypName)
cov <- covs |>
  drop_na() |>
  select(-id)

nest_rec <- recipe(
  cov,
  formula = class ~ .
) |>
  step_normalize(all_numeric()) |>
  step_dummy(all_nominal_predictors(), -all_outcomes())

nest_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = nest_rec),
    models = list(
      # the standard glm specs
      # glm = sdm_spec_glm(),
      # rf specs with tuning
      rf = sdm_spec_rf(),
      # boosted tree model (gbm) specs with tuning
      gbm = sdm_spec_boost_tree(),
      # maxent specs with tuning
      # gam = sdm_spec_gam()
      maxent = sdm_spec_maxent()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) |>
  # set formula for gams
  # update_workflow_model(
  #   "default_gam",
  #   spec = sdm_spec_gam(),
  #   formula = gam_formula(nest_rec)
  # ) |>
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())

set.seed(1234)
nest_cv <- spatial_block_cv(cov, v = 5)
autoplot(nest_cv)
tidysdm::check_splits_balance(nest_cv, .col = class)

set.seed(45657)
nest_models <- nest_models |>
  workflow_map(
    "tune_grid",
    resamples = nest_cv,
    grid = 25,
    metrics = sdm_metric_set(),
    verbose = T
  )


autoplot(nest_models)

nest_ensamble <- simple_ensemble() |>
  add_member(nest_models, metric = "roc_auc")

nest_ensamble
