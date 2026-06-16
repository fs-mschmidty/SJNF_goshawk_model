download_gee <- function(geom_input, raster_stack) {
  if (inherits(geom_input, "sf")) {
    print("Is sf object")
    g <- sf::st_geometry_type(geom_input, by_geometry = TRUE)

    if (all(g %in% c("POINT", "MULTIPOINT"))) {
      points <- geom_input |>
        dplyr::mutate(id = as.character(id))

      pts_ee <- sf_as_ee(points |> sf::st_transform(4326))

      extracted_data <- raster_stack$reduceRegions(
        collection = pts_ee,
        reducer = ee$Reducer$first(), # 'first' grabs the exact pixel value intersecting the point
        scale = 30
      )
      result <- drive_extract_export(extracted_data) |>
        as_tibble() |>
        select(-geometry, -class) |>
        mutate(id = as.integer(id))
    } else {
      stop("sf object is not POINT or MULTIPOINT.")
    }
  } else if (inherits(geom_input, "SpatRaster")) {
    print("is raster")
    # get extent
    e <- geom_input |>
      terra::project("EPSG:4326") |>
      terra::ext()

    # convert to Earth Engine BBox
    roi_ee <- ee$Geometry$BBox(
      west = e$xmin,
      south = e$ymin,
      east = e$xmax,
      north = e$ymax
    )

    raster_stack <- raster_stack$clip(roi_ee)$toFloat()
    epsg_code <- crs(geom_input, describe = T)$code
    epsg_tot <- paste0("EPSG:", epsg_code)

    result <- get_gee_rast(
      raster_stack = raster_stack,
      roi_ee = roi_ee,
      proj = epsg_tot
    ) |>
      terra::project(geom_input) |>
      terra::crop(geom_input, mask = TRUE)
  } else {
    stop("geom_input is neither a SpatRaster nor a sf object")
  }
}
