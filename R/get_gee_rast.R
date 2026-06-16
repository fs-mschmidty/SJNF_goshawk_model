get_gee_rast <- function(raster_stack, roi_ee, proj) {
  task_desc <- paste0(
    "gee_rast_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  export_folder <- "rgee_exports"

  task <- rgee::ee_image_to_drive(
    image = raster_stack,
    description = task_desc,
    folder = export_folder,
    region = roi_ee,
    scale = 30,
    crs = proj,
    maxPixels = 1e9
  )

  task$start()
  rgee::ee_monitoring(task, quiet = FALSE)

  # 6. Download safely to a temporary file
  export_folder <- googledrive::drive_get(export_folder)

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

  # 7. Load into a SpatRaster and name the layer cleanly
  final_rast <- terra::rast(temp_tif)
  # Clean up Google Drive
  googledrive::drive_rm(drive_file[1, ])

  return(final_rast)
}
