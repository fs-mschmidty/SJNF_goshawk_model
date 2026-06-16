get_tree_map_attribute <- function(x, attribute, y) {
  tm <- rast(x)

  activeCat(tm) <- attribute

  y_cl <- y |>
    st_transform(crs(tm))

  if (all(st_is(y, c("POINT", "MULTIPOINT")))) {
    values <- terra::extract(tm, y_cl) |>
      as_tibble()

    y_cl |>
      as_tibble() |>
      select(id) |>
      bind_cols(values) |>
      select(-ID)
  } else if (all(st_is(y, c("POLYGON", "MULTIPOLYGON")))) {
    tm |>
      crop(y_cl, mask = T)
  }
}
