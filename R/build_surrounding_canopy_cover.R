build_surrounding_canopy_cover <- function(x, model_points) {
  cc <- rast(x)

  model_points_cl <- model_points |>
    st_transform(crs(cc))

  cc_cl <- cc |>
    crop(ext(model_points_cl))

  wind_c <- cc_cl |>
    focal(w = 15, fun = "mean")

  r_points <- terra::extract(wind_c, model_points_cl) |>
    as_tibble()

  model_points |>
    as_tibble() |>
    select(id) |>
    bind_cols(r_points) |>
    select(-ID)
}
