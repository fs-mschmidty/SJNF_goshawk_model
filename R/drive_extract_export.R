drive_extract_export <- function(ee_fc) {
  task_name <- paste0("rgeexport_", format(Sys.time(), "%Y%m%d_%H%M%S"))

  task <- ee$batch$Export$table$toDrive(
    collection = ee_fc,
    folder = "rgee_exports",
    description = task_name,
    fileFormat = "GeoJSON"
  )
  task$start()

  ee_monitoring(task, quiet = FALSE)

  Sys.sleep(5)
  # Locate Drive folder and file
  folder <- googledrive::drive_get("rgee_exports")
  repeat {
    file <- googledrive::drive_ls(folder, pattern = task_name)

    if (nrow(file) > 0) {
      break
    }

    Sys.sleep(5)
  }

  # Download to temporary file
  local_path <- tempfile(fileext = ".geojson")
  googledrive::drive_download(file[1, ], path = local_path, overwrite = TRUE)

  googledrive::drive_rm(file[1, ]) # clean up Drive
  sf::st_read(local_path, quiet = TRUE)
}
