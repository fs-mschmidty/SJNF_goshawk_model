get_climate <- function(points) {
  init_ee()

  # Convert id to character to avoid Python integer overflows
  points <- points |>
    dplyr::mutate(id = as.character(id))

  pts_ee <- sf_as_ee(points |> sf::st_transform(4326))

  # ------------------------------------
  # 1. Load PRISM monthly normals
  # ------------------------------------
  prism <- ee$ImageCollection("OREGONSTATE/PRISM/Norm91m")

  # Compute annual summaries for each variable
  annual_tmax <- prism$select("tmax")$max()
  annual_tmin <- prism$select("tmin")$min()
  annual_tmean <- prism$select("tmean")$mean()
  annual_ppt <- prism$select("ppt")$sum()

  # ------------------------------------
  # 2. Combine into a single multi-band image
  # ------------------------------------
  climate_img <- annual_tmax$rename("annual_tmax")$addBands(annual_tmin$rename(
    "annual_tmin"
  ))$addBands(annual_tmean$rename("annual_tmean"))$addBands(annual_ppt$rename(
    "annual_ppt"
  ))

  # ------------------------------------
  # 3. One reduceRegions call (fast + stable)
  # ------------------------------------
  extracted <- climate_img$reduceRegions(
    collection = pts_ee,
    reducer = ee$Reducer$first(),
    scale = 800
  )

  # Remove geometry to avoid big JSON in Drive export
  extracted <- extracted$map(function(f) f$setGeometry(NULL))

  # ------------------------------------
  # 4. Export to Drive, download, read locally
  # ------------------------------------
  drive_export <- function(ee_fc) {
    task_name <- paste0("climate_", format(Sys.time(), "%Y%m%d_%H%M%S"))

    task <- ee$batch$Export$table$toDrive(
      collection = ee_fc,
      folder = "rgee_exports",
      description = task_name,
      fileFormat = "GeoJSON"
    )
    task$start()

    ee_monitoring(task, quiet = FALSE)

    # Locate Drive folder and file
    folder <- googledrive::drive_get("rgee_exports")
    file <- googledrive::drive_ls(folder, pattern = task_name)

    # Download to temporary file
    local_path <- tempfile(fileext = ".geojson")
    googledrive::drive_download(file[1, ], path = local_path, overwrite = TRUE)

    googledrive::drive_rm(file[1, ]) # clean up Drive
    sf::st_read(local_path, quiet = TRUE)
  }

  result <- drive_export(extracted)

  # ------------------------------------
  # 5. Clean + return tidy tibble
  # ------------------------------------
  result |>
    dplyr::as_tibble() |>
    dplyr::select(
      id,
      annual_max_temp = annual_tmax,
      annual_min_temp = annual_tmin,
      annual_mean_temp = annual_tmean,
      annual_ppt = annual_ppt
    )
}
