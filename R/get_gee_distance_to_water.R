get_gee_distance_to_water <- function(geom_input) {
  init_ee()
  water <- ee$Image("JRC/GSW1_4/GlobalSurfaceWater")$select("occurrence")

  conus <- ee$FeatureCollection("TIGER/2018/States")$filter(ee$Filter$notEquals(
    "STUSPS",
    "AK"
  ))$filter(ee$Filter$notEquals("STUSPS", "HI"))$union()$geometry()

  water_mask <- water$gt(60)$clip(conus)
  distance_to_water <- water_mask$fastDistanceTransform()$sqrt()$rename(
    "DISTANCE_TO_WATER"
  )$clip(conus)
  result <- download_gee(
    geom_input = geom_input,
    raster_stack = distance_to_water
  )
  return(result)
}

