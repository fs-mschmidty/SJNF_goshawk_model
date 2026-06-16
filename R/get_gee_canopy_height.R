get_gee_canopy_height <- function(geom_input) {
  init_ee()
  canopy_height <- ee$ImageCollection(
    "projects/sat-io/open-datasets/ICESAT/CHM_CONUS"
  )$mosaic()$select("b1")$rename("CANOPY_HEIGHT")

  mean_canopy_50m <- canopy_height$select(
    'CANOPY_HEIGHT'
  )$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(50, "meters")
  )$rename("CANOPY_HEIGHT_50M")

  mean_canopy_100m <- canopy_height$select(
    'CANOPY_HEIGHT'
  )$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(100, "meters")
  )$rename("CANOPY_HEIGHT_100M")

  mean_canopy_200m <- canopy_height$select(
    'CANOPY_HEIGHT'
  )$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(200, "meters")
  )$rename("CANOPY_HEIGHT_200M")

  raster_stack <- canopy_height$addBands(mean_canopy_50m)$addBands(
    mean_canopy_200m
  )$addBands(
    mean_canopy_100m
  )$select(c(
    "CANOPY_HEIGHT",
    "CANOPY_HEIGHT_50M",
    "CANOPY_HEIGHT_100M",
    "CANOPY_HEIGHT_200M"
  ))

  result <- download_gee(geom_input = geom_input, raster_stack = raster_stack)

  return(result)
}
