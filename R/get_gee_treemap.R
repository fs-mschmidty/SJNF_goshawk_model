get_gee_treemap <- function(geom_input) {
  init_ee()
  treemap_img <- ee$ImageCollection('USFS/GTAC/TreeMap/v2022')$first()

  # 2. Append the new band to the original image, then slice out your specific 8 bands
  target_bands <- c(
    'BALIVE',
    'QMD',
    'TPA_LIVE',
    'SDIsum'
  )
  raster_stack <- treemap_img$select(target_bands)

  result <- download_gee(geom_input = geom_input, raster_stack = raster_stack)
  return(result)
}
