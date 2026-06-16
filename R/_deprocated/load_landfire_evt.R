load_landfire_evt <- function(path, model_area, cat) {
  r <- rast(path)
  activeCat(r) <- cat
  model_area <- model_area |>
    st_transform(crs(r))
  r |>
    crop(model_area, mask = T)
}
