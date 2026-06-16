predict_rd <- function(geom_input, model, tree_map_path) {
  treemap_for_type <- rast(tree_map_path) |>
    crop(st_transform(geom_input, 5070), mask = T)

  treemap <- get_gee_treemap(
    geom_input = treemap_for_type
  )

  canopy_height <- get_gee_canopy_height(
    geom_input = treemap_for_type
  )

  terrain <- get_gee_terrain(
    geom_input = treemap_for_type
  )

  climate <- get_gee_climate(
    geom_input = treemap_for_type
  )

  ## Soils Data
  soil <- get_gee_ph(
    geom_input = treemap_for_type
  )

  canopy_cover <- get_gee_canopy_cover(
    geom_input = treemap_for_type
  )

  distance_to_water <- get_gee_distance_to_water(
    geom_input = treemap_for_type
  )

  covs <- make_predict_covs(
    c(
      treemap,
      treemap_for_type,
      canopy_height,
      canopy_cover,
      distance_to_water,
      terrain,
      climate,
      soil
    )
  )

  rm(treemap)
  rm(treemap_for_type)
  rm(canopy_height)
  rm(canopy_cover)
  rm(distance_to_water)
  rm(terrain)
  rm(climate)
  rm(soil)

  predicted_raster <- predict_raster(
    model,
    covs,
    typ = "prob"
  )

  return(predicted_raster)
}
