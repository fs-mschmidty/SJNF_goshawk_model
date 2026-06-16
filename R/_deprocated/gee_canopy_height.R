gee_canopy_height <- function(geom_input) {
  init_ee()

  canopy_height <- ee$ImageCollection(
    "projects/sat-io/open-datasets/ICESAT/CHM_CONUS"
  )$mosaic()
  geom_type <- as.character(st_geometry_type(geom_input, by_geometry = FALSE))
  if (geom_type %in% c("POINT", "MULTIPOINT")) {
    points <- geom_input |>
      dplyr::mutate(id = as.character(id))

    pts_ee <- sf_as_ee(points |> sf::st_transform(4326))

    extracted_data <- canopy_height$reduceRegions(
      collection = pts_ee,
      reducer = ee$Reducer$first(),
      scale = 30
    )

    result <- drive_extract_export(extracted_data)

    result |>
      as_tibble() |>
      select(-geometry, -class) |>
      mutate(id = as.integer(id)) |>
      rename(CANOPY_HT = first)
  }
}
