get_gee_terrain <- function(geom_input) {
  init_ee()
  glo_30_collection <- ee$ImageCollection('COPERNICUS/DEM/GLO30')
  glo_30_proj <- glo_30_collection$first()$projection()
  dem <- glo_30_collection$select('DEM')$mosaic()$setDefaultProjection(
    glo_30_proj
  )

  terrain_products <- ee$Terrain$products(dem)
  aspect_raw <- terrain_products$select('aspect')
  slope_raw <- terrain_products$select('slope') # Slope in degrees

  # 3. Convert slope to radians and calculate sin(slope)
  # Flat = 0, 90-degree cliff = 1
  slope_rad <- slope_raw$multiply(pi)$divide(180)
  slope_sin <- slope_rad$sin()

  # 4. Calculate unweighted Northness and Eastness
  northness <- aspect_raw$multiply(pi)$divide(180)$cos()
  eastness <- aspect_raw$multiply(pi)$divide(180)$sin()

  # 5. Apply slope weights
  # Range: -1 (steep south) to 1 (steep north). Flat areas = 0.
  slope_weighted_northness <- northness$multiply(slope_sin)$rename(
    "slope_weighted_northness"
  )
  slope_weighted_eastness <- eastness$multiply(slope_sin)$rename(
    "slope_weighted_eastness"
  )

  tpi_kernel <- ee$Kernel$circle(radius = 3, units = 'pixels')
  mean_dem <- dem$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = tpi_kernel
  )
  # mean_dem <- dem$focal_mean(kernel = tpi_kernel)
  tpi <- dem$subtract(mean_dem)$rename('TPI')

  tpi_200_kernel <- ee$Kernel$circle(radius = 7, units = 'pixels')
  mean_dem_200 <- dem$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = tpi_200_kernel
  )
  tpi_200 <- dem$subtract(mean_dem_200)$rename('TPI_200m')

  tpi_400_kernel <- ee$Kernel$circle(radius = 13, units = 'pixels')
  mean_dem_400 <- dem$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = tpi_400_kernel
  )
  tpi_400 <- dem$subtract(mean_dem_400)$rename('TPI_400m')

  make_tri <- function(input_layer, name = "TRI") {
    tri_kernel <- ee$Kernel$square(radius = 1, units = 'pixels')
    # 2. Compute focal sum of the DEM (s)
    dem_sum <- input_layer$reduceNeighborhood(
      reducer = ee$Reducer$sum(),
      kernel = tri_kernel
    )

    # 3. Compute the square of the DEM (E^2)
    dem_sq <- input_layer$multiply(input_layer)

    # 4. Compute focal sum of the squared DEM (t)
    dem_sq_sum <- dem_sq$reduceNeighborhood(
      reducer = ee$Reducer$sum(),
      kernel = tri_kernel
    )

    # 5. Use an expression to compute Riley's formula
    # Formula expanded: sqrt(dem_sq_sum + (9 * dem_sq) - (2 * dem * dem_sum))
    tri <- dem_sq_sum$add(dem_sq$multiply(9))$subtract(
      input_layer$multiply(2)$multiply(dem_sum)
    )$sqrt()$rename(name)

    return(tri)
  }

  dem_100 <- dem$reduceResolution(
    reducer = ee$Reducer$mean(),
    maxPixels = 1024
  )$reproject(
    crs = dem$projection()$crs(),
    scale = 90
  )

  dem_400 <- dem$reduceResolution(
    reducer = ee$Reducer$mean(),
    maxPixels = 1024
  )$reproject(
    crs = dem$projection()$crs(),
    scale = 390
  )

  tri <- make_tri(dem, name = "TRI")
  tri_100 <- make_tri(dem_100, name = "TRI_100")$resample()$reproject(
    crs = dem$projection()$crs(),
    scale = 30
  )
  tri_400 <- make_tri(dem_400, name = "TRI_400")$resample()$reproject(
    crs = dem$projection()$crs(),
    scale = 30
  )

  raster_stack <- dem$addBands(list(
    slope_raw,
    aspect_raw,
    tpi,
    tri,
    tri_100,
    tri_400,
    tpi_200,
    tpi_400,
    slope_weighted_northness,
    slope_weighted_eastness
  ))$rename(c(
    'elevation',
    'slope',
    'aspect',
    'TPI',
    'TRI',
    'TRI_100',
    'TRI_400',
    'TPI_200',
    'TPI_400',
    'slope_weighted_northness',
    'slope_weighted_eastness'
  ))

  result <- download_gee(geom_input = geom_input, raster_stack = raster_stack)
  return(result)
}
