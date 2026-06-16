build_nogo_nest_model <- function(cov, nest_rec) {
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
