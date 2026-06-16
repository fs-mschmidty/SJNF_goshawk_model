get_gee_climate <- function(geom_input) {
  init_ee()
  all_months <- rgee::ee$List(list(
    "01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12"
  ))

  spring_months <- rgee::ee$List(list(
    "03",
    "04",
    "05",
    "06"
  ))
  # Helper to build a reducer and return the single-band result

  reduce_band <- function(band_name, reducer_fn, period_list) {
    rgee::ee$ImageCollection("OREGONSTATE/PRISM/Norm91m")$select(
      band_name
    )$filter(rgee::ee$Filter$inList("system:index", period_list))$reduce(
      reducer_fn
    )
  }

  # Compute each stat
  tmax_max <- reduce_band("tmax", rgee::ee$Reducer$max(), all_months)$rename(
    "tmax_max"
  )
  tmin_min <- reduce_band("tmin", rgee::ee$Reducer$min(), all_months)$rename(
    "tmin_min"
  )
  tmean_mean <- reduce_band(
    "tmean",
    rgee::ee$Reducer$mean(),
    all_months
  )$rename(
    "tmean_mean"
  )
  ppt_sum <- reduce_band("ppt", rgee::ee$Reducer$sum(), all_months)$rename(
    "ppt_sum"
  )

  tmax_max_spring <- reduce_band(
    "tmax",
    rgee::ee$Reducer$max(),
    spring_months
  )$rename(
    "tmax_max_spring"
  )

  tmin_min_spring <- reduce_band(
    "tmin",
    rgee::ee$Reducer$min(),
    spring_months
  )$rename(
    "tmin_min_spring"
  )
  tmean_mean_spring <- reduce_band(
    "tmean",
    rgee::ee$Reducer$mean(),
    spring_months
  )$rename(
    "tmean_mean_spring"
  )
  ppt_sum_spring <- reduce_band(
    "ppt",
    rgee::ee$Reducer$sum(),
    spring_months
  )$rename(
    "ppt_sum_spring"
  )
  # Combine them into one stacked image
  annual_img <- tmax_max$addBands(list(
    tmin_min,
    tmean_mean,
    ppt_sum,
    tmax_max_spring,
    tmin_min_spring,
    tmean_mean_spring,
    ppt_sum_spring
  ))
  result <- download_gee(geom_input = geom_input, raster_stack = annual_img)
  return(result)
}
