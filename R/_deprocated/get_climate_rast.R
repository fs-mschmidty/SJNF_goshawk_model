get_climate_rast <- function(poly, mask_rast) {
  init_ee()
  poly_bbox <- poly |>
    st_transform(4326) |>
    st_bbox()

  roi_ee <- ee$Geometry$BBox(
    west = poly_bbox[["xmin"]],
    south = poly_bbox[["ymin"]],
    east = poly_bbox[["xmax"]],
    north = poly_bbox[["ymax"]]
  )

  tc <- ee$ImageCollection("OREGONSTATE/PRISM/Norm91m")

  ## Annual Max Temp
  tmax_monthly <- tc$select("tmax")

  annual_tmax <- tmax_monthly$max()

  annual_tmax_rast <- ee_as_rast(
    image = annual_tmax,
    region = roi,
    scale = 800,
    via = "getDownloadURL"
  )
  annual_tmax_rast |>
    project(mask_rast) |>
    crop(mask_rast, mask = TRUE)

  # annual_max_temp <- ee_as_sf(extracted_ee) |>
  #   as_tibble() |>
  #   select(id, annual_max_temp = max)
  #
  # tmin_monthly <- tc$select("tmin")
  #
  # annual_tmin <- tmax_monthly$min()
  #
  # extracted_ee <- annual_tmin$reduceRegions(
  #   collection = pts_ee,
  #   reducer = ee$Reducer$min(),
  #   scale = 800
  # )
  #
  # annual_min_temp <- ee_as_sf(extracted_ee) |>
  #   as_tibble() |>
  #   select(id, annual_min_temp = min)
  #
  # ## Mean temp
  #
  # tmean_monthly <- tc$select("tmean")
  #
  # annual_tmean <- tmax_monthly$mean()
  #
  # extracted_ee <- annual_tmean$reduceRegions(
  #   collection = pts_ee,
  #   reducer = ee$Reducer$min(),
  #   scale = 800
  # )
  #
  # annual_mean_temp <- ee_as_sf(extracted_ee) |>
  #   as_tibble() |>
  #   select(id, annual_mean_temp = min)
  #
  # ## Annual Precip
  # ppt_monthly <- tc$select("ppt")
  #
  # annual_ppt <- ppt_monthly$sum()
  #
  # extracted_ee <- annual_ppt$reduceRegions(
  #   collection = pts_ee,
  #   reducer = ee$Reducer$min(),
  #   scale = 800
  # )
  #
  # annual_ppt <- ee_as_sf(extracted_ee) |>
  #   as_tibble() |>
  #   select(id, annual_ppt = min)
  #
  # annual_max_temp |>
  #   left_join(annual_min_temp, by = "id") |>
  #   left_join(annual_mean_temp, by = "id") |>
  #   left_join(annual_ppt)
}
