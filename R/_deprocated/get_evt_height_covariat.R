get_evt_height_covariat <- function(x, points) {
  r <- rast(x)

  p <- points |>
    st_transform(crs(r))

  r <- r |>
    crop(p)

  r <- evh_to_num(r)

  p_extract <- terra::extract(r, p) |>
    as_tibble()

  p |>
    as_tibble() |>
    select(id) |>
    bind_cols(p_extract) |>
    select(-ID)
}
