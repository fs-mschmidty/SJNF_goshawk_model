get_covariates_sclass <- function(x, points) {
  r <- rast(x)

  area_cl <- points |>
    st_transform(crs(r))

  r <- r |>
    crop(area_cl)

  sclass_cl <- sclass_clean(r)

  get_covariates(sclass_cl, points)
}
