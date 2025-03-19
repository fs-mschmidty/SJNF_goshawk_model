get_covariates <- function(x, model_points, cat = FALSE, new_level = FALSE) {
  if (is.character(x)) {
    r <- rast(x)
  } else {
    r <- x
  }

  if (is.character(cat)) {
    activeCat(r) <- cat
  }
  if (is.data.frame(new_level)) {
    levels(r) <- new_level
  }
  model_points_cl <- model_points |>
    st_transform(crs(r))

  r_points <- terra::extract(r, model_points_cl) |>
    as_tibble()

  model_points |>
    as_tibble() |>
    select(id) |>
    bind_cols(r_points) |>
    select(-ID)
}
