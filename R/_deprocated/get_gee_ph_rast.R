#' Download Soil pH data and perfectly align it to a local reference raster
#' @param ref_raster A terra::SpatRaster object, or a file path to one, to act as the template.
#' @param depth_band Character. The depth band to extract (e.g., "b0", "b10", "b30", "b60", "b100", "b200"). Default is "b60".
#' @return A terra::SpatRaster perfectly aligned and masked to the reference raster.
get_gee_ph_rast <- function(ref_raster, depth_band) {
  # 1. Handle the reference raster input
  if (is.character(ref_raster)) {
    ref_rast <- terra::rast(ref_raster)
  } else {
    ref_rast <- ref_raster
  }

  # Initialize EE with Drive
  init_ee()
  # =========================================================================
  # STEP 1: EXTRACT BBOX AND CONVERT TO WGS84 FOR EARTH ENGINE
  # =========================================================================
  ref_ext_poly <- terra::as.polygons(
    terra::ext(ref_rast),
    crs = terra::crs(ref_rast)
  )
  ref_ext_wgs84 <- terra::project(ref_ext_poly, "EPSG:4326")

  roi_sf <- sf::st_as_sf(ref_ext_wgs84)
  roi_ee <- rgee::sf_as_ee(roi_sf)$geometry()

  # =========================================================================
  # STEP 2: EARTH ENGINE BBOX EXTRACTION
  # =========================================================================
  # OpenLandMap is a single static image, so no reducers or filters needed!
  ph_image <- rgee::ee$Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02") |>
    rgee::ee$Image$select(depth_band) |>
    rgee::ee$Image$clip(roi_ee) |>
    rgee::ee$Image$toFloat()

  # Export to Drive
  task_desc <- paste0(
    "Soil_pH_",
    depth_band,
    "_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  task <- rgee::ee_image_to_drive(
    image = ph_image,
    description = task_desc,
    folder = "rgee_exports",
    region = roi_ee,
    scale = 250, # Native resolution of OpenLandMap
    crs = "EPSG:4326", # Standard projection for this global dataset
    maxPixels = 1e9
  )

  task$start()
  rgee::ee_monitoring(task, quiet = TRUE)

  export_folder <- googledrive::drive_get("rgee_exports")
  drive_file <- googledrive::drive_ls(path = export_folder, pattern = task_desc)

  temp_tif <- tempfile(fileext = ".tif")

  googledrive::drive_download(
    file = drive_file[1, ],
    path = temp_tif,
    overwrite = TRUE
  )

  raw_ee_rast <- terra::rast(temp_tif)

  # Clean up Drive
  googledrive::drive_rm(drive_file[1, ])

  # =========================================================================
  # STEP 3: LOCAL ALIGNMENT, CROPPING, AND MASKING
  # =========================================================================
  # Snap the 250m global WGS84 soil data precisely into your local grid
  aligned_rast <- terra::project(raw_ee_rast, ref_rast)

  # Apply the local mask (e.g., project boundaries, water bodies)
  final_rast <- terra::mask(aligned_rast, ref_rast)

  # Name the layer cleanly for downstream use
  names(final_rast) <- paste0("ph_", depth_band)

  return(final_rast)
}

