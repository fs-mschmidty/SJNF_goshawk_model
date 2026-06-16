build_treemap_covs <- function(path, y, bands) {
  r <- rast(path)
  y1 <- y |>
    st_transform(crs(r))

  r2 <- r |>
    crop(y1, mask = T)

  l <- cats(r2)[[1]] |>
    select(Value, bands)
  levels(r2) <- l
  catalyze(r2)
}
