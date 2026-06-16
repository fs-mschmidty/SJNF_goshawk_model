gee_terrain <- function(
  geom_input
) {
  init_ee()

  # --- 1. Compute Earth Engine Terrain Stack ---
  glo_30_collection <- ee$ImageCollection('COPERNICUS/DEM/GLO30')
  glo_30_proj <- glo_30_collection$first()$projection()
  dem <- glo_30_collection$select('DEM')$mosaic()$setDefaultProjection(
    glo_30_proj
  )

  terrain_products <- ee$Terrain$products(dem)

  slope <- terrain_products$select('slope')
  aspect <- terrain_products$select('aspect')

  tpi_kernel <- ee$Kernel$circle(radius = 3, units = 'pixels')
  mean_dem <- dem$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = tpi_kernel
  )
  # mean_dem <- dem$focal_mean(kernel = tpi_kernel)
  tpi <- dem$subtract(mean_dem)$rename('TPI')

  tri_kernel <- ee$Kernel$square(radius = 1, units = 'pixels')
  # 2. Compute focal sum of the DEM (s)
  dem_sum <- dem$reduceNeighborhood(
    reducer = ee$Reducer$sum(),
    kernel = tri_kernel
  )

  # 3. Compute the square of the DEM (E^2)
  dem_sq <- dem$multiply(dem)

  # 4. Compute focal sum of the squared DEM (t)
  dem_sq_sum <- dem_sq$reduceNeighborhood(
    reducer = ee$Reducer$sum(),
    kernel = tri_kernel
  )

  # 5. Use an expression to compute Riley's formula
  # Formula expanded: sqrt(dem_sq_sum + (9 * dem_sq) - (2 * dem * dem_sum))
  tri <- dem_sq_sum$add(dem_sq$multiply(9))$subtract(
    dem$multiply(2)$multiply(dem_sum)
  )$sqrt()$rename('TRI')

  raster_stack <- dem$addBands(slope)$addBands(aspect)$addBands(tpi)$addBands(
    tri
  )$rename(c('elevation', 'slope', 'aspect', 'TPI', 'TRI'))

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

    result <- get_gee_rast(raster_stack = raster_stack, roi_ee = roi_ee) |>
      terra::project(geom_input) |>
      terra::crop(geom_input, mask = TRUE)
  } else {
    stop("geom_input is neither a SpatRaster nor a sf object")
  }
  return(result)
}
