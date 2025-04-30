build_nogo_nest_model <- function(covs) {
  cov <- covs |>
    drop_na() |>
    mutate(canopy_cover = as.numeric(canopy_cover)) |>
    select(-id)

  nest_rec <- recipe(
    cov,
    formula = class ~
      meters_h +
        elevation +
        slope +
        aspect +
        TPI +
        TRI +
        canopy_cover +
        focal_mean +
        evt_gp_n_grouped +
        sclass
  ) |>
    step_normalize(all_numeric()) |>
    step_dummy(evt_gp_n_grouped, sclass)

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
        maxent = sdm_spec_maxent()
      ),
      # make all combinations of preproc and models,
      cross = TRUE
    ) %>%
    # tweak controls to store information needed later to create the ensemble
    option_add(control = control_ensemble_grid())

  set.seed(1234)
  nest_cv <- spatial_block_cv(cov, v = 5)

  set.seed(45657)
  nest_models <- nest_models |>
    workflow_map(
      "tune_grid",
      resamples = nest_cv,
      grid = 25,
      metrics = sdm_metric_set(),
      verbose = T
    )

  nest_ensamble <- simple_ensemble() |>
    add_member(nest_models, metric = "boyce_cont")

  nest_ensamble
}
