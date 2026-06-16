get_elevation_data_rgee <- function(geom_input, rast_input) {
  init_ee()
  ## Set the bbox for the info
  poly_bbox <- geom_input |>
    st_transform(4326) |>
    st_bbox()

  roi_ee <- ee$Geometry$BBox(
    west = poly_bbox[["xmin"]],
    south = poly_bbox[["ymin"]],
    east = poly_bbox[["xmax"]],
    north = poly_bbox[["ymax"]]
  )

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

  terrain_stack <- dem$addBands(slope)$addBands(aspect)$addBands(tpi)$addBands(
    tri
  )$rename(c('elevation', 'slope', 'aspect', 'TPI', 'TRI'))$clip(
    roi_ee
  )$toFloat()

  task_desc <- paste0(
    "terrain_rast_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  export_folder <- "rgee_exports"

  task <- rgee::ee_image_to_drive(
    image = terrain_stack,
    description = task_desc,
    folder = export_folder,
    region = roi_ee,
    scale = 30,
    crs = "EPSG:4269",
    maxPixels = 1e9
  )

  task$start()
  rgee::ee_monitoring(task, quiet = FALSE)

  # 6. Download safely to a temporary file
  export_folder <- googledrive::drive_get("rgee_exports")

  repeat {
    drive_file <- googledrive::drive_ls(
      path = export_folder,
      pattern = task_desc
    )

    if (nrow(drive_file) > 0) {
      break
    }

    Sys.sleep(5)
  }

  temp_tif <- tempfile(fileext = ".tif")

  googledrive::drive_download(
    file = drive_file[1, ],
    path = temp_tif,
    overwrite = TRUE
  )

  geom_input_cl <- geom_input |>
    st_transform(26913)

  # 7. Load into a SpatRaster and name the layer cleanly
  final_rast <- terra::rast(temp_tif) |>
    terra::project(rast_input) |>
    terra::crop(rast_input, mask = T)

  # Clean up Google Drive
  googledrive::drive_rm(drive_file[1, ])

  return(final_rast)
}
