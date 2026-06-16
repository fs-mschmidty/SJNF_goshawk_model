get_covs_for_predict <- function(
  area,
  evt_gps,
  proj
) {
  t <- get_elevation_data(area)

  crd_bd_cl <- area |>
    st_transform(crs(t))

  t_cl <- t |>
    crop(crd_bd_cl, mask = T)

  cc <- load_clip_tree_canopy_cover(
    "D:\\GIS_Data\\NLCD\\nlcd_tcc_CONUS_2021_v2021-4\\nlcd_tcc_conus_2021_v2021-4.tif",
    area,
    proj
  )

  names(cc) <- "canopy_cover"

  crd_bd_cl2 <- area |>
    st_transform(crs(cc))

  cc_cl <- cc |>
    crop(crd_bd_cl2, mask = T)

  t_cl_p <- project(t_cl, cc_cl)

  focal_cc <- cc_cl |>
    focal(w = 11, fun = "mean")

  evh_raw <- rast(
    "D:\\GIS_Data\\Landfire\\LF2023_EVH_240_CONUS\\LF2023_EVH_240_CONUS\\Tif\\LC23_EVH_240.tif"
  )

  crd_bd_cl3 <- area |>
    st_transform(crs(evh_raw))

  evh_raw <- evh_raw |>
    crop(crd_bd_cl3, mask = T) |>
    project(cc_cl)

  evh_cl <- evh_to_num(evh_raw)

  evt_raw <- rast(
    "D:\\GIS_Data\\Landfire\\LF2023\\LF2023_EVT_240_CONUS\\LF2023_EVT_240_CONUS\\Tif\\LC23_EVT_240.tif"
  )

  evt <- evt_raw |>
    crop(crd_bd_cl3, mask = T) |>
    project(cc_cl)

  levels(evt) <- list(evt_gps)

  sclass <- rast(
    "D:\\GIS_Data\\Landfire\\LF2023_SClass_240_CONUS\\LF2023_SClass_240_CONUS\\Tif\\LC23_SCla_240.tif"
  ) |>
    crop(crd_bd_cl3) |>
    project(cc_cl)

  sclass <- sclass_clean(sclass)

  cov_rast <- c(t_cl_p, cc_cl, focal_cc, evh_cl, evt, sclass)
  cov_rast
}
