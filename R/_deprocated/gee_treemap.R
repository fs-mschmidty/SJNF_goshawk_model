gee_treemap <- function(
  geom_input
) {
  init_ee()

  points <- geom_input |>
    dplyr::mutate(id = as.character(id))

  pts_ee <- sf_as_ee(points |> sf::st_transform(4326))

  # Alternative streamlined approach
  treemap_img <- ee$ImageCollection('USFS/GTAC/TreeMap/v2022')$first()

  # 1. Compute neighborhood mean directly from the main image
  mean_canopy_200m <- treemap_img$select('CANOPYPCT')$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(200, "meters")
  )$rename("CANOPYPCT_200M")

  mean_canopy_100m <- treemap_img$select('CANOPYPCT')$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(100, "meters")
  )$rename("CANOPYPCT_100M")

  # 2. Append the new band to the original image, then slice out your specific 8 bands
  target_bands <- c(
    'BALIVE',
    'QMD',
    'TPA_LIVE',
    'TPA_DEAD',
    'CANOPYPCT',
    'SDIsum',
    'CANOPYPCT_200M',
    'CANOPYPCT_100M'
  )
  raster_stack <- treemap_img$addBands(mean_canopy_200m)$addBands(
    mean_canopy_100m
  )$select(target_bands)
  extracted_data <- raster_stack$reduceRegions(
    collection = pts_ee,
    reducer = ee$Reducer$first(), # 'first' grabs the exact pixel value intersecting the point
    scale = 30 # USFS TreeMap native pixel resolution is 30 meters
  )

  result <- drive_extract_export(extracted_data)

  result |>
    as_tibble() |>
    select(-geometry, -class) |>
    mutate(id = as.integer(id))
}
