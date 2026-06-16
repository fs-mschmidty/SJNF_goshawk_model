# get_soil <- function(points) {
#   init_ee()
#
#   ph_image <- ee$Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02")
#
#   pts_ee <- sf_as_ee(points |> st_transform(4326))
#
#   extracted_ee <- ph_image$reduceRegions(
#     collection = pts_ee,
#     reducer = ee$Reducer$first(),
#     scale = 250 # OpenLandMap native resolution
#   )
#
#   ee_as_sf(extracted_ee) |>
#     as_tibble() |>
#     select(-geometry) |>
#     rename_with(~ paste0("ph_", .x), c(b0, b10, b30, b60, b100, b200)) |>
#     select(id, ph_b60) |>
#     mutate(ph_b60 = as.numeric(ph_b60))
# }

get_soil <- function(points) {
  init_ee()

  # 1. Load image
  ph_image <- ee$Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02")

  # 2. Convert points to EE
  pts_ee <- sf_as_ee(points |> st_transform(4326))

  # 3. Extract values (stays server-side)
  extracted_ee <- ph_image$reduceRegions(
    collection = pts_ee,
    reducer = ee$Reducer$first(),
    scale = 250
  )

  # 4. Export to Drive
  task_desk <- paste0(
    "ph_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  task <- ee$batch$Export$table$toDrive(
    collection = extracted_ee,
    folder = "rgee_exports",
    description = task_desk,
    fileFormat = "GeoJSON"
  )
  task$start()

  # 5. Monitor
  ee_monitoring(task, quiet = FALSE)

  # 6. Download from Drive to temp file
  export_folder <- googledrive::drive_get("rgee_exports")
  drive_file <- googledrive::drive_ls(path = export_folder, pattern = task_desk)

  temp_file <- tempfile(fileext = ".json")

  googledrive::drive_download(
    file = drive_file[1, ],
    path = temp_file,
    overwrite = TRUE
  )

  # 7. Read the file (Drive writes .geojson)
  soil_sf <- sf::st_read(temp_file, quiet = TRUE)

  googledrive::drive_rm(drive_file[1, ])
  # 8. Clean + return same format you had
  soil_sf |>
    dplyr::as_tibble() |>
    dplyr::select(-geometry) |>
    dplyr::rename_with(~ paste0("ph_", .x), c(b0, b10, b30, b60, b100, b200)) |>
    dplyr::select(id, ph_b60) |>
    dplyr::mutate(ph_b60 = as.numeric(ph_b60))
}
