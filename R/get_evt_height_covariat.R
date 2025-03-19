get_evt_height_covariat <- function(x, points) {
  r <- rast(x)

  c <- cats(r)[[1]] |>
    mutate(
      height_m = str_extract(CLASSNAMES, "\\d+"),
      height_m = ifelse(is.na(height_m), 0, height_m) |>
        as.numeric()
    ) |>
    select(Value, height_m)

  levels(r) <- list(c)

  activeCat(r) <- "height_m"

  p <- points |>
    st_transform(crs(r))

  p_extract <- terra::extract(r, p) |>
    as_tibble()

  p |>
    as_tibble() |>
    select(id) |>
    bind_cols(p_extract) |>
    select(-ID)
}
