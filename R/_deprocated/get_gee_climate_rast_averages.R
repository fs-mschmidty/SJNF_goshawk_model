#' Download generalized PRISM 30-Year Normals from Earth Engine
#' @param poly_sf An sf polygon object representing the project area.
#' @param band Character. The PRISM band to download (e.g., "tmax", "tmin", "ppt").
#' @param stat Character. The summary statistic across the 12 months ("max", "min", or "mean").
#' @param crop_rast A terra raster that will be used to project the raster to the correct projection and crop.
#' @param final_name Character. The final name of the layer.
#' @return A terra::SpatRaster object.
get_gee_climate_rast_averages <- function(
  poly_sf,
  band = "tmax",
  stat = "max",
  crop_rast,
  final_name
) {
  init_ee()

  # 1. Convert sf to ee$Geometry

  poly_bbox <- poly_sf |>
    st_transform(4326) |>
    st_bbox()

  roi_ee <- ee$Geometry$BBox(
    west = poly_bbox[["xmin"]],
    south = poly_bbox[["ymin"]],
    east = poly_bbox[["xmax"]],
    north = poly_bbox[["ymax"]]
  )

  # 2. Filter exactly the 12 months
  months_list <- rgee::ee$List(list(
    "01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12"
  ))

  prism_months <- rgee::ee$ImageCollection("OREGONSTATE/PRISM/Norm91m") |>
    rgee::ee$ImageCollection$select(band) |>
    rgee::ee$ImageCollection$filter(rgee::ee$Filter$inList(
      "system:index",
      months_list
    ))

  # 3. Assign the correct Earth Engine Reducer dynamically
  ee_reducer <- switch(
    stat,
    "max" = rgee::ee$Reducer$max(),
    "min" = rgee::ee$Reducer$min(),
    "mean" = rgee::ee$Reducer$mean(),
    "sum" = rgee::ee$Reducer$sum(),
    stop("Invalid stat argument. Choose 'max', 'min', or 'mean'.")
  )

  # 4. Apply generic reduce(), clip, and cast
  annual_stat_img <- prism_months$reduce(ee_reducer)$clip(roi_ee)$toFloat()

  # 5. Export to Google Drive with dynamic task naming
  # e.g., "PRISM_tmin_min_20260513_143000"
  task_desc <- paste0(
    "PRISM_",
    band,
    "_",
    stat,
    "_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  task <- rgee::ee_image_to_drive(
    image = annual_stat_img,
    description = task_desc,
    folder = "rgee_exports",
    region = roi_ee,
    scale = 800,
    crs = "EPSG:4269",
    maxPixels = 1e9
  )

  task$start()
  rgee::ee_monitoring(task, quiet = TRUE)

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

  # 7. Load into a SpatRaster and name the layer cleanly
  final_rast <- terra::rast(temp_tif) |>
    terra::project(crop_rast) |>
    terra::crop(crop_rast, mask = T)

  names(final_rast) <- final_name

  # Clean up Google Drive
  googledrive::drive_rm(drive_file[1, ])

  return(final_rast)
}
