get_rd_prediction <- function(
  model_path,
  rds,
  rd_id,
  terrain_model,
  canopy_cover,
  evt_gp_n_new_level
) {
  model <- readRDS(model_path)
  rd_boundary <- rds |>
    filter(RANGERDISTRICTID == rd_id)

  rd_boundary

  # covs <- get_covs_for_predict(
  #   rd_boundary,
  #   terrain_model,
  #   canopy_cover,
  #   evt_gp_n_new_level
  # )
  # covs

  # predict_raster(
  #   model,
  #   covs
  # )
}
