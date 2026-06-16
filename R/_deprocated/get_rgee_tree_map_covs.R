get_rgee_tree_map_covs <- function(geom_input, rast_input) {
  init_ee()
  poly_bbox <- geom_input |>
    st_transform(4326) |>
    st_bbox()

  roi_ee <- ee$Geometry$BBox(
    west = poly_bbox[["xmin"]],
    south = poly_bbox[["ymin"]],
    east = poly_bbox[["xmax"]],
    north = poly_bbox[["ymax"]]
  )
  treemap_img <- ee$ImageCollection('USFS/GTAC/TreeMap/v2022')$first()

  # 1. Compute neighborhood mean directly from the main image
  mean_canopy_200m <- treemap_img$select('CANOPYPCT')$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(200, "meters")
  )$rename("CANOPYPCT_200M")

  # 2. Append the new band to the original image, then slice out your specific 8 bands
  target_bands <- c(
    'BALIVE',
    'QMD',
    'TPA_LIVE',
    'TPA_DEAD',
    'CANOPYPCT',
    'STANDHT',
    'SDIsum',
    'CANOPYPCT_200M'
  )
  raster_stack <- treemap_img$addBands(mean_canopy_200m)$select(
    target_bands
  )$clip(roi_ee)$toFloat()

  task_desc <- paste0(
    "treemap_rast_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  export_folder <- "rgee_exports"

  task <- rgee::ee_image_to_drive(
    image = raster_stack,
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

