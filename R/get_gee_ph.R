get_gee_ph <- function(geom_input) {
  init_ee()
  ph_image <- ee$Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02")
  raster_stack <- ph_image$select(c("b30", "b200"))

  result <- download_gee(geom_input = geom_input, raster_stack = raster_stack)
  return(result)
}
