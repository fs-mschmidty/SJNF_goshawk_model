get_gee_canopy_cover <- function(geom_input) {
  init_ee()
  image <- ee$ImageCollection("USGS/NLCD_RELEASES/2023_REL/TCC/v2023-5")
  canopy_cover <- image$filter(ee$Filter$calendarRange(
    2023,
    2023,
    'year'
  ))$filter('study_area=="CONUS"')$select(
    "NLCD_Percent_Tree_Canopy_Cover"
  )$first()$rename("CANOPYPCT")

  mean_canopy_50m <- canopy_cover$select(
    'CANOPYPCT'
  )$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(50, "meters")
  )$rename("CANOPYPCT_50M")

  mean_canopy_100m <- canopy_cover$select(
    'CANOPYPCT'
  )$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(100, "meters")
  )$rename("CANOPYPCT_100M")

  mean_canopy_200m <- canopy_cover$select(
    'CANOPYPCT'
  )$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(200, "meters")
  )$rename("CANOPYPCT_200M")

  raster_stack <- canopy_cover$addBands(mean_canopy_50m)$addBands(
    mean_canopy_200m
  )$addBands(
    mean_canopy_100m
  )$select(c('CANOPYPCT', 'CANOPYPCT_50M', 'CANOPYPCT_100M', 'CANOPYPCT_200M'))

  result <- download_gee(geom_input = geom_input, raster_stack = raster_stack)
  return(result)
}

