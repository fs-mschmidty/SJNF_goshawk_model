get_elevation_data <- function(base_raster) {
  elev <- get_elev_raster(
    base_raster,
    z = 10,
    override_size_check = T,
    clip = "bbox"
  ) |>
    rast()

  cropped <- elev

  names(cropped) <- "elevation"

  slope <- cropped |>
    terrain("slope")

  aspect <- cropped |>
    terrain("aspect")

  tpi <- cropped |>
    terrain("TPI")

  tri <- cropped |>
    terrain("TRI")

  c(cropped, slope, aspect, tpi, tri)
}
