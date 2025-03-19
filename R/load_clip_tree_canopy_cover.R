load_clip_tree_canopy_cover <- function(file_path, model_area, projection) {
  tcc <- terra::rast(file_path)

  model_area_tr <- model_area |>
    sf::st_transform(crs(tcc)) |>
    as("SpatVector")

  output <- tcc |>
    terra::crop(model_area_tr) |>
    terra::project(projection)

  output
}
